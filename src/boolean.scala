object boolean extends App {

  abstract class Boolean {
    def ifThenElse(t : => Boolean, f : => Boolean) : Boolean
    def unary_! : Boolean = ifThenElse(False, True)

    def ==(b : Boolean):Boolean = ifThenElse(b, !b)
    def &&(b : Boolean):Boolean = ifThenElse(b, False)
    def ||(b : Boolean):Boolean = ifThenElse(True, b)
    def <(b : Boolean):Boolean = ifThenElse(False, b)

  }
  object True extends Boolean {
    def ifThenElse(t : => Boolean, f : => Boolean) : Boolean = t
    //def ==(b : Boolean):Boolean = ifThenElse(b, False)
  }

  object False extends Boolean {
    def ifThenElse(t : => Boolean, f : => Boolean) : Boolean = f
    //def ==(b: Boolean):Boolean = ifThenElse(False,!b)
  }
  val a = True
  val b = False


  println(a == b)
  println(a || b)
  println(a && b)
  println(a < b)


  /*
  a == b ?
  a.==(b) // if (a) b else !b
  if (a == True) then grana1
                 else grana2

   */
}
