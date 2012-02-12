//crashes 2.9.1 and 2.10.0-M1 compiler
/*object scalacrash extends App {
	val reg = "regex".r
	val reg(a*) = "input"
}*/
object scalacrash extends App {
	class c {
		def unapplySeq(s:String) = Some(List(s))
	}
	val foo = new c
	val foo(bar*) = "input"
}

