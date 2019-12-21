functor MkHamt(X: sig eqtype key val hash: key -> Int32.int end) : MAP
= struct
    open Int32

    type key = X.key
    datatype 'a t = Leaf
               | Exact of {hash: int, size: int, key: key, value: 'a }
               | Node of {hash: int, size: int, zero: 'a t, one: 'a t }

    val empty = Leaf
    fun isEmpty Leaf = true
      | isEmpty _ = false

    fun isEven i = i mod 2 = 0

    fun expt base ex =
        if ex = 0
        then 1
        else let val half = expt base (ex div 2)
             in if isEven ex
                then half * half
                else base * half * half
             end



    fun firstDiffBit x y = let
        fun countSame x y count = if x mod 2 = y mod 2
                                  then countSame (x div 2) (y div 2) (count + 1)
                                  else count
    in
        countSame x y (fromInt 0)
    end

    datatype branch = ZERO | ONE
    datatype horder = HASH_DIFFER of { count: int, base: int, base_rest: int, rest: int, branch: branch}| HASH_SAME of { rest: int, branch: branch}
    exception HashConflict

    fun compare base_hash hash size =
        if base_hash = hash
        then let val q = expt 2 size
                 val rest = hash div q
                 val branch = if isEven hash then ZERO else ONE
             in HASH_SAME { rest = rest, branch = branch } end
        else let
            val count = firstDiffBit base_hash hash
            (* assert count < size *)
            val q = expt 2 count
            val base = base_hash div q
            val base_rest = base_hash mod q
            val rest = hash mod q
            val branch = if isEven rest then ZERO else ONE
        in HASH_DIFFER { count = count, base = base, base_rest = base_rest, rest = rest, branch = branch} end

    fun findHash Leaf hash key = NONE
      | findHash (Exact {hash = hash, size = size, key = key, value = value}) hash2 key2 =
        if key = key2 then SOME value else NONE
      | findHash (Node {hash = hash, size = size, zero = zero, one = one}) hash2 key2 =
        (case compare hash hash2 size of
             HASH_SAME { rest = rest, branch = ZERO} => findHash zero rest key2
          |  HASH_SAME { rest = rest, branch =  ONE} => findHash one rest key2
          | _ => NONE)

    fun find t key = findHash t (X.hash key) key
    fun isMember t key = Option.isSome (find t key)


    fun insertHash Leaf hash key value = Exact { hash = hash, size = (fromInt 32), key = key, value = value}
      | insertHash (Exact {hash = hash, size = size, key = key, value = value}) hash2 key2 value2  =
        if key = key2 then Exact {hash = hash, size = size, key = key, value = value}
        else (case compare hash hash2 size of
                  HASH_DIFFER { count = count, base = base, base_rest = base_rest, rest = rest, branch = branch} => let
                   val t1 = Exact { hash = base_rest, size = size - count, key = key, value = value}
                   val t2 = Exact { hash = rest, size = size - count, key = key, value = value}
               in case branch of
                      ZERO => Node { hash = base, size = count, zero = t2, one = t1}
                   |   ONE => Node { hash = base, size = count, zero = t2, one = t1}
               end
                | HASH_SAME _ => raise HashConflict
             )
      | insertHash (Node {hash = hash, size = size, zero = zero, one = one}) hash2 key2 value2 =
        (case compare hash hash2 size of
             HASH_SAME { rest = rest, branch = ZERO} => Node {hash = hash, size = size, zero = (insertHash zero rest key2 value2), one = one}
           | HASH_SAME { rest = rest, branch =  ONE} => Node {hash = hash, size = size, zero = zero, one = (insertHash one rest key2 value2)}
           | HASH_DIFFER { count = count, base = base, base_rest = base_rest, rest = rest, branch = branch} => let
              val t1 = Node { hash = base_rest, size = size - count, zero = zero, one = one}
              val t2 = Exact { hash = rest, size = size - count, key = key2, value = value2 }
           in case branch of
                  ZERO => Node { hash = base, size = count, zero = t2, one = t1}
                |  ONE => Node { hash = base, size = count, zero = t1, one = t2}
           end
        )


    fun insert t k v = insertHash t (X.hash k) k v

    fun removeHash Leaf hash key = (Leaf, NONE)
      | removeHash (t as Exact {hash = hash, key = key, value = value, ...}) hash2 key2 =
        if key = key2
        then (Leaf, SOME(value))
        else (t, NONE)
      | removeHash (t as Node {hash = hash, size = size, zero = zero, one = one}) hash2 key2 = 
        (case compare hash hash2 size of
             HASH_SAME {rest = rest, branch = ZERO} => let
              val (zero, v) = removeHash zero rest key2
          in (Node {hash = hash, size = size, zero = zero, one = one}, v) end
          |  HASH_SAME {rest = rest, branch =  ONE} => let
              val ( one, v) = removeHash  one rest key2
          in (Node {hash = hash, size = size, zero = zero, one = one}, v) end
          |  HASH_DIFFER _=> (t, NONE))

    fun remove t key = removeHash t (X.hash key) key

    fun delete t key= #1 (remove t key)

    fun fold Leaf acc f = acc
      | fold (Exact {key = key, value = value, ...}) acc f= f((key, value), acc)
      | fold (Node {zero, one, ...}) acc f= fold one (fold zero acc f) f

    fun fromList l = List.foldl (fn ((k, v), t) => insert t k v) empty l
    fun toList t = fold t [] (op::)

end

(* usage
structure IntHamt = MkHamt(struct type k = int
                                fun hash x = Int32.fromInt x
                         end)

val t = IntHamt.fromList [(1, "one"), (2, "two"), (3, "three"), (4, "four")]
IntHamt.find t 1 (* -> SOME "one" *)
val (t, _) = IntHamt.remove t 1
IntHamt.find t 1 (* -> NONE *)
*)
