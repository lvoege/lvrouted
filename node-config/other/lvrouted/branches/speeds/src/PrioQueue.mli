(* Functorized priority queue from the manual. *)
module type S = sig
	type priority
	type 'a queue
	val empty : 'a queue
	val insert : 'a queue -> priority -> 'a -> 'a queue
	exception Queue_is_empty
	val extract : 'a queue -> priority * 'a * 'a queue
end

module Make(P : Set.OrderedType) : S with type priority = P.t
