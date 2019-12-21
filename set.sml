functor SetFromMap(M: MAP): SET = struct
    type t = unit M.t
    type elem = M.key
    val empty = M.empty
    val isEmpty = M.isEmpty
    val isMember = M.isMember
    fun insert t e = M.insert t e ()
    val delete = M.delete
    val fromList = M.fromList o List.map (fn e => (e, ()))
    val toList = List.map (fn (e, _) => e) o M.toList
end
