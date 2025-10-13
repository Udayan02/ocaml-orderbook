open Types

let create_price_level(price) = {
    price;
    orders = Queue.create();
    order_ids = Hashtbl.create(32);
    total_qty = 0;
}


