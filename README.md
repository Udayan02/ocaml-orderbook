(* We're using a hashtable for order_ids to have O(1) lookup for wheter an order ID exists or not.
   We don't need to save any values in the hashtable, so we're using unit. A Set in OCaml would
   give us O(logn) look-up instead of O(1). *)

