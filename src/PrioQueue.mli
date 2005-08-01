module Make :
  functor (P : Set.OrderedType) ->
    sig
      type priority = P.t
      type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
      val empty : 'a queue
      val insert : 'a queue -> priority -> 'a -> 'a queue
      exception Queue_is_empty
      val remove_top : 'a queue -> 'a queue
      val extract : 'a queue -> priority * 'a * 'a queue
      val size : 'a queue -> int
    end
