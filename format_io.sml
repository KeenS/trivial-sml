structure Printf = struct

    type ('a, 'b) format = string -> (string -> 'a) -> 'b
    type 'a simple_format = ('a, 'a) format
    type 'a formatter = 'a -> string
    infix ++

    fun lit x s k = k (s ^ x)
    fun eol s k = k (s ^ "\n")
    fun f1 ++ f2 = (fn (s: string) => (fn k => (f1 s (fn (s: string) => f2 s k))))
    fun int x = Int.toString x
    fun str x = x
    fun % to_str s k = (fn x => k (s ^ to_str x))
    fun sprintf p = p "" (fn x => x)
end

structure SubstringCvt = struct
    fun scanWith scan s = scan Substring.getc s
end


fun readFixed s input =
    if Substring.isPrefix s input
    then SOME(s, (Substring.triml (String.size s) input))
    else NONE

structure Scanf = struct

    type ('a, 'b, 'c) format = 'c -> 'a -> ('b * 'c) option
    type ('a, 'c) simple_format = ('a, 'a, 'c) format
    type ('a, 'c) scanner = (char, 'c) StringCvt.reader -> ('a, 'c) StringCvt.reader

    infix ++

    fun lit x s v = case readFixed x s of
                        SOME(_, rest) => SOME(v, rest)
                      | NONE => NONE
    fun eol s v = lit "\n" s v
    fun f1 ++ f2 = (fn s => (fn v => Option.mapPartial (fn (v, s) => f2 s v) (f1 s v)))
    fun int rdr = Int.scan StringCvt.DEC rdr
    fun bool rdr = Bool.scan rdr
    fun % scan s cb = Option.map (fn (v, s) => (cb v, s)) (SubstringCvt.scanWith scan s)
    fun sscanf p s = p (Substring.full s)
end
