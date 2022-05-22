object intset extends App {

  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(s : IntSet):IntSet
    def intersection(s : IntSet):IntSet

    def printSet():Unit
  }

  class Empty extends IntSet {
    def contains(x : Int) = false
    def incl(x : Int) =
      new NonEmpty(x, new Empty, new Empty)

    override def union(s: IntSet): IntSet = s
    override def intersection(s: IntSet): IntSet = new Empty
    def printSet() = print("")
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def this(elem: Int) = this(elem, new Empty, new Empty)
    def contains(x : Int) = {
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true
    }
    def incl(x : Int) = {
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this
    }

    override def union(s: IntSet): IntSet = ((left union right) union s) incl elem

    override def intersection(s: IntSet): IntSet = {
      val intersectionWithoutRoot = (left intersection s) union (right intersection s)
      if (s contains elem) intersectionWithoutRoot incl elem
      else intersectionWithoutRoot
    }


    def printSet() = {
      left.printSet
      print(elem + ",")
      right.printSet
    }
  }

  val x = new NonEmpty(10, new Empty, new Empty)
  val s1 = new NonEmpty( 15, x, new NonEmpty(20, new Empty, new Empty))
  val s2 = new  NonEmpty( 15, new Empty, new NonEmpty(17, new Empty, new Empty))

  (s1 intersection s2).printSet()
  println("")
  (s1 union s2).printSet()
  println("")
  //println(x.contains(11))
}