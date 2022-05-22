import scala.reflect.ClassTag

object Vezbe4 extends App{

  /*
  1. Napisati for ciklus koji ispisuje sve elemente niza stringova.
2. Napisati for ciklus koji ispisuje sve elemente niza stringova nakon pretvaranja svih slova u
velika.
3. Napisati for ciklus koji od niza stringova formira nov niz stringova čiji su svi znaci velika
slova.
   */

  val niz= Array("Pera", "Mika", "Zika")

  for (elem <- niz ) println(elem)
  for (elem <- niz ) println(elem.toUpperCase)

  val niz_upper= for (elem <- niz ) yield elem.toUpperCase

  /*
  Data je funkcija sumTri koja izračunava sumu elemenata kvadratne matrice koji se nalaze
u gornjem trouglu. Napisati funkciju linTri koja vrši linearizaciju dela matrice koji
odgovara gornjem trouglu:
a) po vrstama
b) po kolonama
type Matrix[T] = Array[Array[T]]
def sumTri[T <% Double](m : Matrix[T]) = {
 val length = m.length
 var value = 0 : Double
 for(row <- 0 until length; col <- row until length)
 value = value + m(row)(col)
 value
}
   */

  type Matrix[T] = Array[Array[T]]
  def sumTri[T <% Double](m : Matrix[T]) = {
    val length = m.length
    var value = 0 : Double
    for(row <- 0 until length; col <- row until length)
      value = value + m(row)(col)
    value
  }

  def linearizeByRows[T: ClassTag](m: Matrix[T])(implicit ev:T => Double): Array[T]={
    val res = for(row <- 0 until m.length; col <- row until m.length) yield m(row)(col)
    res.toArray
  }
  def printArray[T](a:Array[T])={
    for(row <- a) println(row+" ")
  }

  def linearizeByColumns[T: ClassTag](m: Matrix[T])(implicit ev:T => Double): Array[T]={
    val res = for(col <- 0 until m.length; row <- 0 to col) yield m(row)(col)
    res.toArray
  }

  val mat = Array(Array[Int](1,2,3),Array[Int](4,5,6),Array[Int](7,8,9))
  //println(printArray(linearizeByRows(mat)))
  println(printArray(linearizeByColumns(mat)))

  /*
  5. Za spisak knjiga dat u formi liste naslova i autora, napisati for ciklus koji nalazi:
a) sve knjige čiji je jedan od autora zadatog imena
b) sve autore koji su napisali najmanje dve knjige sa spiska
case class Book(title: String, authors: List[String])

   */

  case class Book(title:String, authors:List[String])
  val books: List[Book] = List(
    Book("Structure and Interpretation of Computer Programs",
      List("Abelson, Harold", "Sussman, Gerald J.")),
    Book("Principles of Compiler Design",
      List("Aho, Alfred", "Ullman, Jeffrey")),
    Book("Programming in Modula-2",
      List("Wirth, Niklaus", "Abelson, Harold")),
    Book("Introduction to Functional Programming",
      List("Bird, Richard")),
    Book("The Java Language Specification",
      List("Gosling, James", "Joy, Bill", "Steele, Guy",
        "Bracha, Gilad")))

  def findAuthorsBooks(ciljani:String, books:List[Book])
  = for (knjiga<-books; autor<-knjiga.authors if autor==ciljani) yield knjiga.title

  def allAuthorsTwoBooksOrMore(books:List[Book])={
    val res = for (k1<-books; k2<-books if k1 != k2; a1<-k1.authors; a2<-k2.authors if a1 == a2) yield a1;
    res.distinct.toArray
  }
  //printArray(findAuthorsBooks("Abelson, Harold").toArray)
  printArray(allAuthorsTwoBooksOrMore(books))

/*
6. Napisati funkciju koja sintaksno i semantički odgovara while ciklusu.
sveDok(uslov){ telo }
 */
  def while2(uslov:Boolean)(telo: =>Unit): Unit ={
    if (uslov){
      telo
      while2(uslov)(telo)
    }
  }
  var p=1
  def incr=p=p+1
  while (p<5){
    incr
  }
  println(p)

  /*
  1. Napisati funkciju za nalaženje najvećeg zajedničkog delioca dva cela broja primenom
uparivanja obrazaca.
def gcd(broj1: Int, broj2: Int): Int
   */





}
