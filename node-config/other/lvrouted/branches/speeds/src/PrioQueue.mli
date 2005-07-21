(* This is a priority queue functorized so you can plug in any type you
   can define a comparison operator for. For example:

   module IntQueue = PrioQueue.Make(struct
   	type priority = int
	let compare = compare
   end)

   will give you a queue that has integer priority *)
module type OrderedType = sig
	type priority 
	val compare : priority -> priority -> int
end
module Make : functor (P : OrderedType) -> sig
	type priority = P.priority
	type 'a queue
	val empty : 'a queue
	val insert : 'a queue -> priority -> 'a -> 'a queue
	exception Queue_is_empty
	val extract : 'a queue -> priority * 'a * 'a queue
end
