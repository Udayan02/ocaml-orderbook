(* Types File for OCaml Orderbook *)

type order_id = string;
type price = float;
type symbol = string;
type user_id = string;
type quantity = float;
type order_id = string;
type timestamp = float;

type side =
  | Buy
  | Sell

type order_type =
  | Market
  | Limit of price
  | Stop of price
  | StopLimit of price * price (* Stop limit order consists of limit, stop *)
  | StopMarket

type order_status =
  | New
  | Filled
  | Cancelled
  | PartiallyFilled of float
  | Rejected of string

type time_in_force =
  | GoodTillCancel     (* Remains until its filled or cancelled *)
  | ImmediateOrCancel  (* Fill what can be filled immediately, cancel the rest *)
  | FillOrKill         (* Fill entire order immediately or cancel *)
  | Day                (* Cancel at the end of the trading day *)

type order = {
  symbol: symbol;
  user_id: user_id;
  id: order_id;
  timestamp: timestamp;
  side: side;
  price: price option; (* Don't always need a price. Ex: Market Orders *)
  quantity: quantity;
  filled_quantity: quantity;
  order_type: order_type;
  status: order_status;
  time_in_force: time_in_force;
}

(* Trade type when two sides of a trade are matched *)
type trade = {
  trade_id: string;
  buy_order_id: string;
  sell_order_id: string;
  price: price;
  timestamp: timestamp;
  symbol: symbol;
  quantity: quantity;
}

(* Market Data Types: Quote and Depth Level *)

(* Quote: Current best bid/ask sizes and prices *)
type quote = {
  bid: price option;
  bid_size: quantity option;
  ask: price option;
  ask_size: quantity option;
  timestamp: timestamp;
}

(* Depth Level: Info about a price level -
   price, total qty, no. of orders *)
type depth_level = {
  price: price;
  total_quantity: quantity;
  order_count: int;
}

(* ––––––––––––––––––––––––––––––––––––––––––––––––––––––––– *)
(* ––––––––––––––––––––––––––––––––––––––––––––––––––––––––– *)
(*                      HELPER FUNCTIONS                     *)
(* ––––––––––––––––––––––––––––––––––––––––––––––––––––––––– *)
(* ––––––––––––––––––––––––––––––––––––––––––––––––––––––––– *)

(* Converting enums to strings for printing *)

let side_to_string =
  | Buy -> "Buy"
  | Sell -> "Sell"

let order_type_to_string =
  | Market -> "Market"
  | Limit(price) -> Printf.sprintf "Limit @ %0.2f" price
  | Stop(price) -> Printf.sprintf "Stop @ %.2f" price
  | StopLimit(stop, limit) -> Printf.sprintf "Stop @ %.2f | Limit @ %.2f" stop limit

let order_to_string(order) =
  "Order[%s] : %s %.2f %s @ %s"
  order.id
  side_to_string(order.side)
  order.quantity
  order.symbol
  order_type_to_string(order.order_type)

let remaining_quantity(order) = order.quantity - order.filled_quantity

let is_buy(order) = order.side = Buy
let is_sell(order) = order.side = Sell

(* Constructor to create a new order *)
(* Each ~<name> represents a parameter, use ~<param>: <value> to specify default value *)
let create_order ~id ~symbol ~timestamp ~quantity ~user_id ~order_type ~time_in_force ~side =
  let price = match order_type with
    | Market -> None
    | Limit(p) -> Some p
    | Stop(p) -> Some p
    | StopLimit(stop, limit) -> Some limit
  in {
    id;
    price;
    symbol;
    user_id;
    side;
    timestamp;
    time_in_force;
    quantity;
    order_type;
    filled_quantity = 0;
    status = New;
  }

let fill_order(order, fill_qty) = 
  let new_qty = order.filled_quantity + fill_qty in
  let new_status =
    if new_qty >= order.quantity then Filled
    else PartiallyFilled new_qty
  in { order with status = new_status; filled_quantity = new_qty }

