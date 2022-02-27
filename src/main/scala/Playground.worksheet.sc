type returnT[A] = A match
    case String => Int
    case Int => String

def f[A]: A => returnT[A] = {
    case s: String => s.length
    case i: Int => s"Some int: $i"
}

val v: [A] => A => returnT[A] = 
    [A] => (v: A) => f[A](v)

("test", 42).map(v)