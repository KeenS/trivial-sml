signature SET = sig
    type t
    type elem
    val empty: t
    val isEmpty: t -> bool
    val isMember: t -> elem -> bool
    val insert: t -> elem -> t
    val delete: t -> elem -> t
    val fromList: elem list -> t
    val toList: t -> elem list
end
