structure Ratio : RATIO = struct
    type int = Int.int
    type ratio = int * int

    fun gcd (x, y) = if x = y then x
                     else if x > y then gcd (x - y, y)
                     else gcd (x, y - x)

    fun fromIntPair (num, 0) = raise Div
      | fromIntPair (0, den) = (0, 1)
      | fromIntPair (num, den) = let
          val g = if den > 0 then gcd (abs num, abs den)
                  else ~(gcd (abs num, abs den))
      in (num div g, den div g) end

    fun toIntPair r = r
    fun (x, y) + (z, w) = fromIntPair (Int.+(Int.*(w, x), Int.*(y, z)), Int.*(y, w))
    fun (x, y) - (z, w) = fromIntPair (Int.-(Int.*(w, x), Int.*(y, z)), Int.*(y, w))
    fun (x, y) * (z, w) = fromIntPair (Int.*(x, z), Int.*(y, w))
    fun (x, y) / (z, w) = fromIntPair (Int.*(x, w), Int.*(y, z))
    fun rem (r1, r2) = let val (num, den) = r1 / r2
                       in (Int.rem(num, den), den) end
    fun ~ (num, den) = (Int.~ num, den)
    fun abs (num, den) = (Int.abs num, Int.abs den)

    fun compare ((x, y), (z, w)) = Int.compare (Int.*(w, x), Int.*(y, z))
    fun (x, y) < (z, w)= Int.<(Int.*(w, x), Int.*(y, z))
    fun (x, y) <= (z, w)= Int.<=(Int.*(w, x), Int.*(y, z))
    fun (x, y) > (z, w) = Int.>(Int.*(w, x), Int.*(y, z))
    fun (x, y) >= (z, w) = Int.>=(Int.*(w, x), Int.*(y, z))

    fun min (r1, r2) = if r1 < r2 then r1 else r2
    fun max (r1, r2) = if r1 < r2 then r2 else r1
    fun sign (num, den) = Int.*(Int.sign num, Int.sign den)
    fun sameSign (r1, r2) = sign r1 = sign r2

    fun split (num, den) = { whole = (Int.div (num, den), 1), frac = (Int.mod (num, den), den)}
    fun ratioMod (num, den) = (Int.mod (num, den), den)

    fun floor r = Int.div r
    fun ceil r = Int.~(floor (~r))
    fun trunc r = Int.quot r
    fun round r = let val { whole = (whole, _), frac = frac} = split r in
                      case compare(frac, (1, 2)) of
                          GREATER => Int.+(whole, 1)
                        | LESS =>  whole
                        | EQUAL => if whole mod 2 = 0 then whole else Int.+(whole, 1)
                  end

    fun fmt radix (num, den) = (Int.fmt radix num) ^ "/" ^ (Int.fmt radix den)
    fun toString r = fmt StringCvt.DEC r

    fun scan radix chrdr stream = let
        fun slashrdr stream = let
            val stream = StringCvt.skipWS chrdr stream
        in
            case chrdr stream of
               SOME(#"/", stream) => SOME((), stream)
             | _ => NONE
        end
        infix 1 >>=
        fun (SOME(x)) >>= f = f x
         |  NONE >>= _ = NONE
        val intrdr = Int.scan radix chrdr
    in
        intrdr stream >>= (fn
        (num, stream) => (slashrdr stream >>= (fn
        ((), stream) => intrdr stream >>= (fn
        (den, stream) => SOME (fromIntPair (num, den), stream)))))
    end

    fun fromString s = StringCvt.scanString (scan StringCvt.DEC) s
end
