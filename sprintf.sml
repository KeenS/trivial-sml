structure Printf = struct

    infix ++

    fun lit x s k = k (s ^ x)
    fun eol s k = k (s ^ "\n")
    fun f1 ++ f2 = (fn s => (fn k => (f1 s (fn s => f2 s k))))
    fun int x = Int.toString x
    fun str x = x
    fun % to_str s k = (fn x => k (s ^ to_str x))
    fun sprintf p = p "" (fn x => x)
    (*
        (* string *)
        sprintf (lit "Hello world!" ++ eol)
        (* -> "Hello world!\n" *)

        (* string -> string *)
        sprintf (lit "Hello " ++ % str ++ lit "!" ++ eol)

        (* string *)
        sprintf (lit "Hello " ++ % str ++ lit "!" ++ eol) "world"
        (* -> "Hello world!\n" *)

        (* string -> int -> string *)
        sprintf (lit "The value of " ++ % str ++ lit " is " ++ % int ++ eol)


        (* string *)
        sprintf (lit "The value of " ++ % str ++ lit " is " ++ % int ++ eol) "x" 3
        (* -> "The value of x is 3\n" *)
    *)
end

structure SubstringCvt = struct
    fun scanWith scan s = scan Substring.getc s
end


fun readFixed s input =
    if Substring.isPrefix s input
    then SOME(s, (Substring.triml (String.size s) input))
    else NONE

structure Scanf = struct

    infix ++

    fun lit x s v = case readFixed x s of
                        SOME(_, rest) => SOME(v, rest)
                      | NONE => NONE
    fun eol s v = lit "\n" s v
    fun f1 ++ f2 = (fn s => (fn v => Option.mapPartial (fn (v, s) => f2 s v) (f1 s v)))
    fun int rdr = Int.scan StringCvt.DEC rdr
    fun bool rdr = Bool.scan rdr
    fun % scan s v1 = Option.map (fn (v2, s) => ((v1, v2), s)) (SubstringCvt.scanWith scan s)
    fun sscanf p s = p (Substring.full s) ()
    (*
        (* string -> ((unit * int) * substring) option *)
        sscanf (lit "input: " ++ % int ++ lit ".")

        (*  ((unit * int) * substring) option *)
        sscanf (lit "input: " ++ % int ++ lit ".") "input: 42"
    *)
end

structure Scanf2 = struct

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
    (*
        (* string -> (int -> ?.X1) -> ?.X1 option *)
        sscanf (lit "input: " ++ % int ++ lit ".")

        (* (int -> ?.X1) -> ?.X1 option *)
        sscanf (lit "input: " ++ % int ++ lit ".") "input: 42"
    *)
end

structure Scanf3 = struct

    infix ++

    exception NoMatch

    fun lit x s k = if Substring.isPrefix x s
                    then k (Substring.triml (String.size x) s)
                    else raise NoMatch
    fun eol s k = lit "\n" s k
    fun f1 ++ f2 = (fn s => (fn k => f1 s (fn s => f2 s k)))
    fun int rdr = Int.scan StringCvt.DEC rdr
    fun bool rdr = Bool.scan rdr
    fun % scan s k = case (SubstringCvt.scanWith scan s) of
                         SOME(v, s) => k s v
                       | NONE => raise NoMatch
    fun sscanf p s = p (Substring.full s)
    (*
        (* string -> (int -> ?.X1) -> ?.X1 option *)
        sscanf (lit "input: " ++ % int ++ lit ".")

        (* (int -> ?.X1) -> ?.X1 option *)
        sscanf (lit "input: " ++ % int ++ lit ".") "input: 42"
    *)
end
