import scala.annotation.tailrec

// liste.scala
object list extends App {

  abstract class List[T] {
    def prazna: Boolean
    def glava: T
    def rep: List[T]
    def printList():Unit
  }

  def ntiElem[T](n:Int, lista: List[T]): T ={
    if (lista.prazna) throw new IndexOutOfBoundsException
    else if (n==0) lista.glava
    else ntiElem[T](n-1, lista.rep)

  }

  def izbaciN[T](n:Int, lista: List[T]): List[T] ={
    if (lista.prazna) throw new IndexOutOfBoundsException
    else if (n==0) lista
    else izbaciN[T](n-1, lista.rep)
  }

  def izbaciDok[T](lista: List[T], p: T => Boolean): List[T] = {
    if (lista.prazna) throw new IndexOutOfBoundsException
    else if ( !p(lista.glava)) lista
    else izbaciDok(lista.rep, p)
  }

  class Nil[T] extends List[T] {
    def prazna = true
    def glava = throw new NoSuchElementException("Nil.glava")
    def rep = throw new NoSuchElementException("Nil.rep")
    override def printList():Unit = {
      println('#')
    }
  }

  class Elem[T](val glava : T, val rep : List[T]) extends List[T] {
    def prazna = false
    override def printList(): Unit = {
      print(this.glava+",")
      this.rep.printList()
    }
  }

  val testList = new Elem(1, new Elem(2, new Elem(3, new Nil)))

  println(ntiElem(0, testList))
  testList.printList()

}
