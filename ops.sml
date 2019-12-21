infixr 1 $
fun f $ e = f e

infix 1 |>
fun e |> f = f e

fun printLn s = print s before print "\n"

infix 4 <\ \>

fun e1 <\ f = fn e2 => f (e1, e2)
fun f \> e2 = f e2


fun id x = x
fun const x _ = x
fun negate f x = not $ f x
fun curry f x y = f (x, y)
fun uncurry f (x, y) = f x y
fun flip f (x, y) = f (y, x)

exception Finally_raise of exn
fun protect finally body = let
    fun finally' () = finally () handle exn => raise Finally_raise exn
in
    (body () handle exn => (finally' (); raise exn)) before finally'()
end

infix 4 % :% <-
fun a % i = Array.sub(a, i)
fun a :% i = fn e => Array.update(a, i, e)
fun f <- e = f e

structure Ref = struct
    infix 1 += -= *=
    fun r += n = r := !r + n
    fun r -= n = r := !r - n
    fun r *= n = r := !r * n
end

structure Result = struct
    datatype ('a, 'e) t = Ok of 'a | Error of 'e
    fun ok a = Ok a
    fun error e = Error e
    fun value (Ok a) _ = a
      | value (Error _) a = a
end

structure ArrayRef = struct
    datatype 'a arref = At of 'a array * int
    fun a @ i = At (a, i)
    fun ! (At (a, i)) = Array.sub(a, i)
    fun (At (a, i)) := e = Array.update(a, i, e)
end
