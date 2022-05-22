import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

// LISTE


object Vezbe5 extends App {

  //1. Napisati implementaciju metode ::: koja vrši konkatenaciju dve generičke liste.


  def :::[B](prefix: List[B], l: List[B]): List[B] = (prefix, l) match {
    case (Nil, l2) => l2
    case (l1, Nil) => l1
    case (h::t, l2) => h::(:::(t,l2))
  }

  val l1 = List(1,2,3)
  val l2 = List(4,5,6,7)

  val l3 = :::(l1, l2)

  println(l3)

  //2. Napisati implementaciju funkcije take, čiji rezultat je nova lista formirana od prvih n
  //elemenata generičke liste. Ako je n veće od dužine liste, rezultat je sama lista.

  def take[T](l : List[T], n : Int) : List[T] = {

    if (n<0) throw new IndexOutOfBoundsException
    else if (n == 0) Nil
    else l match {
      case Nil => throw new IndexOutOfBoundsException
      case h::t => h::take(t, n-1)
    }
  }

  println(take(l3, 4))

  //3. Napisati implementaciju funkcije drop, čiji rezultat je nova lista formirana izostavljajući
  //prvih n elemenata generičke liste. Ako je n veće od dužine liste, rezultat je prazna lista.

  def drop[T](l : List[T], n : Int) : List[T] = {
    if (n<0) throw new IndexOutOfBoundsException
    else if (n == 0) l
    else l match {
      case Nil=> throw new IndexOutOfBoundsException
      case h::t => drop(t, n-1)
    }
  }
  println(drop(l3, 4))

  //4. Napisati implementaciju funkcije splitAt, čiji rezultat je par lista. Prvi element para je
  //  lista formirana izostavljajući prvih n elemenata ulazne liste. Drugi element para je lista koja
  //  se sastoji od preostalih elemenata ulazne liste.

  def splitAt[T](l : List[T], n : Int) : (List[T], List[T]) = {

    @tailrec
    def splitAtTailRec(l : List[T], n: Int, acc: ListBuffer[T]):(List[T], List[T]) = {
      l match {
        case Nil if (n == 0) => (acc.toList, Nil)
        case Nil if (n > 0) => throw new IndexOutOfBoundsException
        case h::t if (n == 0) => (acc.toList, h::t)
        case h::t if (n > 0) => {
          acc += h // acc +=: h ubacuje na pocetak
          splitAtTailRec(t, n-1, acc)
        }
      }
    }

    if (n < 0 ) throw new IndexOutOfBoundsException
    if (n == 0) (Nil, l)
    else splitAtTailRec(l , n, ListBuffer())
  }

  def reverse[T](l:List[T]):List[T] = {
    def reverseTailRec[T](l: List[T], acc: List[T]): List[T] = l match {
      case Nil => acc
      case h :: t => reverseTailRec(t, h :: acc)
    }

    reverseTailRec(l, Nil)
  }

  //val reversed = reverse(l3)
  //println(reversed)
  /*
    val split = splitAt(l3, 3)
    println(split) */

  //5. Implementirati metodu apply koja vraća n-ti element generičke liste.
  /*
  def apply[T](l : List[T], n : Int) : T = {
    require (n >= 0)
    (l, n) match {
      case (Nil, _) => throw new IndexOutOfBoundsException
      case (h::t, 0) => h
      case (h::t, n) => apply(t, n-1)
    }
  }

  println(l3 apply 5)*/

  //6. Napisati funkciju map koja od zadate generičke liste formira novu listu primenom funkcije
  //  preslikavanja na njene elemente.
  def map[S, D](f : S => D)(l : List[S]) : List[D] = {
    val res = for (e <- l) yield f(e)
    res
  }

  def sqrtInt[T](x:T)(implicit ev: T => Double):Double = math.sqrt(x)

  val squares = map(sqrtInt[Int])(l3)
  println(squares)

}

