functor MkRedBlackTree(X: sig
    type key
    val compare: key * key -> order
end): MAP
= struct
    open X;

    datatype color = Red | Black
    datatype 'a t = Leaf | Node of color * 'a t * key * 'a *  'a t

    val empty = Leaf

    fun isEmpty Leaf = true
      | isEmpty _    = false

    fun find Leaf _ = NONE
      | find (Node (_, left, key, value, right)) x =
        (case compare (key, x) of
             LESS => find right x
           | EQUAL => SOME value
           | MORE => find left x)

    fun isMember t x = Option.isSome (find t x)


    fun red   l key value r = Node(Red,   l, key, value, r)
    fun black l key value r = Node(Black, l, key, value, r)


    fun balance (Node(Black, Node(Red, Node(Red, a, xk, xv, b), yk, yv, c), zk, zv, d)) = red (black a xk xv b) yk yv (black c zk zv d)
      | balance (Node(Black, Node(Red, a, xk, xv, Node(Red, b, yk, yv, c)), zk, zv, d)) = red (black a xk xv b) yk yv (black c zk zv d)
      | balance (Node(Black, a, xk, xv, Node(Red, Node(Red, b, yk, yv, c), zk, zv, d))) = red (black a xk xv b) yk yv (black c zk zv d)
      | balance (Node(Black, a, xk, xv, Node(Red, b, yk, yv, Node(Red, c, zk, zv, d)))) = red (black a xk xv b) yk yv (black c zk zv d)
      | balance t = t


    fun insert tree k v = let
        fun ins Leaf key value = red Leaf key value Leaf
          | ins (t as Node(color, left, k1, v1, right)) k2 v2 =
            (case compare (k1, k2) of
                 LESS => balance (Node(color, left, k1, v1, (ins right k2 v2)))
               | EQUAL => t
               | MORE => balance (Node(color, (ins left k2 v2), k1, v1, right))
            )
    in
        case ins tree k v of
            (* unreachable *)
            Leaf => Leaf
          | Node(_, left, key, value, right) => black left key value right

    end

    fun maxKey Leaf = NONE
      | maxKey (Node(_, _, key, _, Leaf)) = SOME key
      | maxKey (Node(_, _,   _, _, right)) = maxKey right

    fun minKey Leaf = NONE
      | minKey (Node(_, Leaf, key, _, _)) = SOME key
      | minKey (Node(_, left,   _, _, _)) = minKey left

    fun rotateL (Node(color, a, xk, xv, Node(Black, Node(Red, b, yk, yv, c), zk, zv, d))) = (false, Node(color, black a xk xv b, yk, yv, black c zk zv d))
      | rotateL (Node(color, a, xk, xv, Node(Black, b, yk, yv, Node(Red, c, zk, zv, d)))) = (false, Node(color, black a xk xv b, yk, yv, black c zk zv d))
      | rotateL (Node(color, a, xk, xv, Node(Black, b, yk, yv, c))) = (color = Black, Node(Black, a, xk, xv, Node(Red, b, yk, yv, c)))
      | rotateL (Node(color, a, xk, xv, Node(Red, b, yk, yv, c))) = let val (_, left) = rotateL (red a xk xv b)
                                                          in (false, black left yk yv c) end
      | rotateL t = (false, t)

    fun rotateR (Node(color, Node(Black, a, xk, xv, Node(Red, b, yk, yv, c)), zk, zv, d)) = (false, Node(color, black a xk xv b, yk, yv, black c zk zv d))
      | rotateR (Node(color, Node(Black, Node(Red, a, xk, xv, b), yk, yv, c), zk, zv, d)) = (false, Node(color, black a xk xv b, yk, yv, black c zk zv d))
      | rotateR (Node(color, Node(Black, a, xk, xv, b), yk, yv, c)) = (color = Black, Node(Black, Node(Red, a, xk, xv, b), yk, yv, c))
      | rotateR (Node(color, Node(Red,   a, xk, xv, b), yk, yv, c)) = let val (_, right) = rotateR (red b yk yv c)
                                                            in (false, black a xk xv right) end
      | rotateR t = (false, t)


    fun rotateLIf true  tree = rotateL tree
      | rotateLIf false tree = (false, tree)

    fun rotateRIf true  tree = rotateR tree
      | rotateRIf false tree = (false, tree)

    fun remove tree x = let
        fun removeMax Leaf = (Leaf, NONE)
          | removeMax (Node(color, left, key, value, Leaf)) = (left, SOME (color, key, value))
          | removeMax (Node(color, left, key, value, right)) =
            let val (right, result) = (removeMax right)
            in (balance (Node(color, left, key, value, right)), result) end
        fun rem Leaf x = (NONE, (false, Leaf))
          | rem (Node(color, left, key, value, right)) x =
            (case compare(key, x) of
                 LESS => let val (ret, (toRotate, right)) = rem right x
                         in (ret, rotateRIf toRotate (Node(color, left, key, value, right))) end
               | EQUAL => let val (left, result) = removeMax left
                          in (case result of
                                  NONE => (SOME value, (false, right))
                                | SOME((Black, k, v)) => (SOME value, rotateL (Node(color, left, k, v, right)))
                                | SOME((Red, k, v)) => (SOME value, (false, Node(color, left, k, v, right))))
                          end
               | MORE => let val (ret, (toRotate, left)) = rem left x
                         in (ret, rotateLIf toRotate (Node(color, left, key, value, right))) end
            )
    in
        case rem tree x of
            (* unreachable *)
            (ret, (_, Leaf)) => (Leaf, ret)
          | (ret, (_, Node(_, left, key, value, right))) => (black left key value right, ret)
    end

    fun delete tree key = #1 (remove tree key)


    fun fold Leaf init f = init
      | fold (Node(_, left, key, value, right)) init f = let
          val init = fold left init f
          val init = f ((key, value), init)
      in fold right init f end

    fun fromList [] = empty
      | fromList ((key, value)::xs) = insert (fromList xs) key value


    fun toList Leaf = []
      | toList (Node(_, left, key, value, right)) = (toList left) @ (key, value) :: (toList right)

end

