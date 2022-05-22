import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

object Vezbe6 extends App {
  //1. Data je mapa koja preslikava nazive regija neke države u liste naziva vrsta stabala koje u
  //njima rastu. Naći jedinstvene nazive vrsta stabala koje rastu u obuhvaćenim regijama.
  //
  val r1 = List("Omorika", "Breza", "Javor", "Hrast")
  val r2 = List("Jasen", "Bukva", "Kesten", "Bor")
  val r3 = List("Lipa", "Omorika", "Topola", "Jablan", "Bukva")
  //val vrste = Map("R1" -> r1, "R2" -> r2, "R3" -> r3)
  val vrste = Map(("R1", r1), ("R2", r2), ("R3", r3))

  def getUniqueTreeSpecies(m:Map[String, List[String]]):List[String] = {
    m.values.flatten.toList.distinct
  }

  val jedinst = getUniqueTreeSpecies(vrste)
  println(jedinst)

  //2. Napisati funkciju koja ispisuje elemente niza zajedno sa njihovim rednim brojevima.

  def printWithIndex[T](a:Array[T]):Unit = {
    for ((e, ind) <- a.zipWithIndex) println(e + ", " + ind)
    //for ( i <- 0 until a.length) println(a(i) + ", " + i)
  }

  printWithIndex(r2.toArray)

  //3. Napisati funkciju koja iz niza izbacuje svaki n-ti element.

  def dropEachNth[T: ClassTag](a : Array[T], n: Int) : Array[T] = {
    for ( (e, ind) <- a.zipWithIndex if (ind + 1)%n != 0 ) yield e
  }

  println("--------------------------------------")
  val samoNeparni = dropEachNth(jedinst.toArray, 2)
  printWithIndex(samoNeparni)

  //4. Napisati funkciju koja za datu listu reči vraća mapu koja preslikava reči u njihove dužine.

  def wordListToLengthMap(l : List[String]):Map[String, Int] = {
    val keyValuePairsList = for (word <- l) yield (word, word.length)
    keyValuePairsList.toMap
  }

  val mapaDuzina = wordListToLengthMap(r3)
  println(mapaDuzina)

  //5. Realizovati for/yield nad listom primenom metode map.

  for ( e <- r1 if e.length > 3) yield e.toUpperCase()

  def forYieldWithMap[T, U]( l :List[T], condition:(T) => Boolean, transformation:(T) => U ):List[U] = {
    l.filter(condition).map(transformation)
  }

  val transformisano = forYieldWithMap(jedinst, (x:String) => (x.length <= 6), (x:String) => x.toUpperCase)
  println(transformisano)

  //6. Implementirati algoritam sortiranja metodom brojanja (counting sort) upotrebom funkcija
  //višeg reda nad kolekcijama.

  val list = List(1, 1, 2, 3, 1, 5)

  def countingSort(l: List[Int]) : List[Int] = {

    val minElem = l.min
    val maxElem = l.max

    var ab = ArrayBuffer[Int]()
    ab.++=(Array.fill(maxElem - minElem + 1){0})
    for (e <- l) ab(e - minElem) += 1

    val cnt = ab.toArray.toList

    var ab2 = ArrayBuffer[Int]()

    for( (e, ind) <- cnt.zipWithIndex) ab2.++=(Array.fill(e){minElem + ind})

    ab2.toArray.toList
  }

  println(countingSort(list))
}
