
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
  !removed_order

let execute_quantity(level, incoming_order, quantity_to_fill) =
  let trades = ref [] in
  let quantity = ref quantity_to_fill in
  






