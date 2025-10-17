open Types (* Using types.ml *)

type t {
  price: price;
  orders: order Queue.t;
  order_ids: (order_id, unit) Hashtbl.t; (* unit is a special type to say "void". See README *)
  mutable total_qty: quantity;
}

let create(price) = {
    price;
    orders = Queue.create();
    order_ids = Hashtbl.create(32);
    total_qty = 0;
}

let price(level) = level.price
let total_quantity(level) = level.total_qty
let order_count(level) = Queue.length(level.orders)
let is_empty(level) = Queue.is_empty(level.orders)

let peak_front(level) =
  if is_empty(level) then None
  else Some Queue.peek(level.orders)

(* Adding an order to a price level *)
let add_order(level, order) =
  Queue.add(order, level.orders);
  Hashtbl.add(level.order_ids, order.id, ());
  level.total_qty <- level.total_qty + remaining_quantity(order)
  (* Recall - we use <- for mutating, = is only for instantiating! *)

(* Removing an order from a price level *)
(* This fn will be O(n) but is important for order cancellations *)
let remove_order(level, order_id) =
  let new_queue = Queue.create () in
  let removed_order = ref None in
  Queue.iter(
    fun order ->
      if order.id = order_id then (
        removed_order = Some order;
        Hashtbl.remove(level.order_ids, order_id);
        level.total_qty <- level.total_qty - remaining_quantity(order);
      )
      else
        Queue.add(order, new_queue)
  ) level.orders;

  Queue.clear(level.orders);
  Queue.transfer(new_queue, level.orders);
  !removed_order (* Simply return the referenced variable removed_order *)

let execute_quantity(level, incoming_order, quantity_to_fill) =
  let trades = ref [] in
  let remaining = ref quantity_to_fill in
  while !remaining > 0 && not Queue.is_empty(level.orders) do
    let front_order = Queue.peek(level.orders) in
    let front_remaining = remaining_quantity(front_order) in
    
    if front_remaining <= !remaining then (
      (* This means we can fully fill the front order *)
      let filled_order = fill_order(front_order, front_quantity) in
      let trade = {
        symbol = incoming_order.symbol;
        trade_id = Printf.sprintf "Trade_%s_%s" incoming_order.id front_order.id;
        price = level.price;
        quantity = front_remaining;
        timestamp = Unix.gettimeofday();
        buy_order_id = if is_buy(incoming_order) then incoming_order.id else front_order.id;
        sell_order_id = if is_sell(incoming_order) then incoming_order.id else front_order.id;
      } in
      remaining := !remaining - front_remaining;
      levels.total_qty <- levels.total_qty - front_remaining;
      trades := trade :: !trades;  (* Remember, :: is cons, add to front of list *)
      ignore Queue.pop(level.orders);
      Hashtbl.remove(level.order_ids, front_order.id);
      )
    else (
    (* This means we should partially fill the front order *)
      let filled_order = fill_order(front_order, !remaining) in 
      let trade = {
        symbol = incoming_order.symbol;
        trade_id = Printf.sprintf "Trade_%s_%s" incoming_order.id front_order.id;
        price = level.price;
        quantity = !remaining;
        timestamp = Unix.gettimeofday();
        buy_order_id = if is_buy(incoming_order) then incoming_order.id else front_order.id;
        sell_order_id = if is_sell(incoming_order) then incoming_order.id else front_order.id;
      } in
      trades := trade :: !trades; (* Remember, :: is cons, add to front of list *)
      levels.total_qty <- levels.total_qty - !remaining;
      ignore Queue.pop(level.orders);
      Queue.add(filled_order, level.orders);
      remaining := 0;  (* <- for mutable record fields and := for references *)
    )
  done;
  (* Return a tuple of the reversed list of all the trades we executed and the rem. qty. *) 
  (List.rev !trades, !remaining)


(* Get all the orders at this level as market data *)
let get_orders(level) =
  let new_queue = Queue.create() in
  let market_data = ref [] in
  while not Queue.is_empty(level.orders) do
    let order = Queue.pop(level.orders) in
    market_data := order :: !market_data;
    Queue.add(order, new_queue);
  done;
  Queue.transfer(new_queue, level.orders);
  List.rev(!market_data)

(* To string for the price level *)
let to_string(level) =
  Printf.sprintf "Price %.2f: %d orders %d shares total"
  level.price order_count(level) level.total_qty
