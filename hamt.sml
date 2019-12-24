functor MkAssoc(X: sig eqtype key end) = struct
       open X
       type 'a t = (key * 'a) list
       val empty = []
       fun isEmpty [] = true
         | isEmpty _ = false

       fun fromPair (key, value) = [(key, value)]
       fun find [] key = NONE
         | find ((k, v) :: rest) key =
           if k = key
           then SOME v
           else find rest key
       fun push t (key, value) = (key, value) :: t
       fun pushNew t (key, value) =
           case find t key of
               SOME(_) => t
             | NONE => push t (key, value)
       fun removeOne [] key = ([], NONE)
         | removeOne ((k, v) :: rest) key =
           if k = key
           then (rest, SOME(v))
           else let val (rest, data) = removeOne rest k
                in  ((k, v) :: rest, data) end
       val foldl = List.foldl

end

functor MkHamt(X: sig eqtype key val hash: key -> Int32.int end) : MAP
= struct
    open Int32
    structure Assoc = MkAssoc(X)

    type key = X.key
    datatype 'a t = Leaf
               | Exact of {hash: int, size: int, assoc:  'a Assoc.t }
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
        countSame x y 0
    end

    fun nthBit i n = let val q = expt 2 n in (i div q) mod 2 end

    datatype branch = ZERO | ONE
    datatype horder = HASH_DIFFER of { count: int, base: int, base_rest: int, rest: int, branch: branch}| HASH_SAME of { rest: int, branch: branch}

    fun compare base_hash hash size = let
        val q = expt 2 size handle Overflow => Option.valOf Int32.maxInt
        val true_base_hash = base_hash mod q
        val true_hash = hash mod q
    in
        if true_base_hash = true_hash
        then let
            val rest = hash div q div 2
            val branch = if nthBit hash size = 0 then ZERO else ONE
        in HASH_SAME { rest = rest, branch = branch } end
        else let
            val nth = firstDiffBit (base_hash mod q) (hash mod q)
            (* assert nth < size *)
            val q = expt 2 nth
            val base = base_hash mod q
            val base_rest = base_hash div q div 2
            val rest = hash div q div 2
            val branch = if nthBit hash nth = 0 then ZERO else ONE
        in HASH_DIFFER { count = nth, base = base, base_rest = base_rest, rest = rest, branch = branch} end
    end

    fun findHash Leaf hash key = NONE
      | findHash (Exact {hash = hash, size = size, assoc = assoc}) hash2 key =
        if hash = hash2 then Assoc.find assoc key else NONE
      | findHash (Node {hash = hash, size = size, zero = zero, one = one}) hash2 key =
        (case compare hash hash2 size of
             HASH_SAME { rest = rest, branch = ZERO} => findHash zero rest key
          |  HASH_SAME { rest = rest, branch =  ONE} => findHash one  rest key
          | _ => NONE)

    fun find t key = findHash t (X.hash key) key
    fun isMember t key = Option.isSome (find t key)


    fun insertHash Leaf hash key value = Exact { hash = hash, size = 32, assoc = Assoc.fromPair(key, value) }
      | insertHash (Exact {hash = hash, size = size, assoc = assoc}) hash2 key value  =
        (case compare hash hash2 size of
             HASH_DIFFER { count = count, base = base, base_rest = base_rest, rest = rest, branch = branch} => let
              val existing = Exact { hash = base_rest, size = size - count - 1, assoc = assoc}
              val new      = Exact { hash = rest,      size = size - count - 1, assoc = Assoc.fromPair(key, value)}
          in case branch of
                 ZERO => Node { hash = base, size = count, zero = new, one = existing }
              |   ONE => Node { hash = base, size = count, zero = existing, one = new }
          end
           | HASH_SAME _ => Exact { hash = hash, size = size, assoc = Assoc.pushNew assoc (key, value) }
        )
      | insertHash (Node {hash = hash, size = size, zero = zero, one = one}) hash2 key value =
        (case compare hash hash2 size of
             HASH_SAME { rest = rest, branch = ZERO} => Node {hash = hash, size = size, zero = (insertHash zero rest key value), one = one}
           | HASH_SAME { rest = rest, branch =  ONE} => Node {hash = hash, size = size, zero = zero, one = (insertHash one rest key value)}
           | HASH_DIFFER { count = count, base = base, base_rest = base_rest, rest = rest, branch = branch} => let
              val existing = Node  { hash = base_rest, size = size - count - 1, zero = zero, one = one}
              val new      = Exact { hash = rest,      size = size - count - 1, assoc = Assoc.fromPair (key, value) }
           in case branch of
                  ZERO => Node { hash = base, size = count, zero = new, one = existing }
                |  ONE => Node { hash = base, size = count, zero = existing, one = new }
           end
        )


    fun insert t k v = insertHash t (X.hash k) k v

    fun removeHash Leaf hash key = (Leaf, NONE)
      | removeHash (t as Exact {hash = hash, size = size, assoc = assoc}) hash2 key =
        if hash = hash2
        then let val (assoc, data) = Assoc.removeOne assoc key
             in if Assoc.isEmpty assoc then (Leaf, data) else (Exact { hash = hash, size = size, assoc = assoc } , data) end
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
      | fold (Exact {assoc = assoc, ...}) acc f = List.foldl f acc assoc
      | fold (Node {zero, one, ...}) acc f= fold one (fold zero acc f) f

    fun fromList l = List.foldl (fn ((k, v), t) => insert t k v) empty l
    fun toList t = fold t [] (op::)

end

(* usage

structure IntHamt = MkHamt(struct type key = int
                                fun hash x = Int32.fromInt x
                         end)

val t = IntHamt.fromList [(1, "one"), (2, "two"), (3, "three"), (4, "four")]
IntHamt.find t 1 (* -> SOME "one" *)
val (t, _) = IntHamt.remove t 1
IntHamt.find t 1 (* -> NONE *)
*)
