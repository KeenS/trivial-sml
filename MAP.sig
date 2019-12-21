signature MAP = sig
    type 'a t
    type key
    val empty: 'a t
    val isEmpty: 'a t -> bool
    val find: 'a t -> key -> 'a option
    val isMember: 'a t -> key -> bool
    val insert: 'a t -> key -> 'a -> 'a t
    val remove: 'a t -> key -> ('a t * 'a option)
    val delete: 'a t -> key -> 'a t
    val fold: 'a t -> 'b -> ((key * 'a) * 'b -> 'b) -> 'b
    val fromList: (key *  'a) list -> 'a t
    val toList: 'a t -> (key * 'a) list
end
