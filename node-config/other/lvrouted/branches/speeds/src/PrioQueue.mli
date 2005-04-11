module type OrderedType =
  sig type priority val compare : priority -> priority -> int end
module type S =
  sig
    type priority
    type 'a queue
    val empty : 'a queue
    val insert : 'a queue -> priority -> 'a -> 'a queue
    exception Queue_is_empty
    val extract : 'a queue -> priority * 'a * 'a queue
  end
module Make :
  functor (P : OrderedType) ->
    sig
      type priority = P.priority
      type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
      val empty : 'a queue
      val insert : 'a queue -> priority -> 'a -> 'a queue
      exception Queue_is_empty
      val remove_top : 'a queue -> 'a queue
      val extract : 'a queue -> priority * 'a * 'a queue
    end
