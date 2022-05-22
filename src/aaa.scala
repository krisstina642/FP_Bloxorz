import scala.annotation.tailrec
object aaa extends App {
  println("hello world")

  def fact(n:Int):Int={
  @tailrec
  def fact2(n:Int, res:Int):Int= if (n==0) res else fact2(n-1, res*n);
    fact2(n,1);
  }
  //println(fact(5));

  def pascal(c:Int, r:Int):Int={
    require(c<=r)
    require(c>=0)
    require(r>=0)

    if (c==0 || c==r) 1
    else pascal(c-1, r-1)+ pascal(c,r-1)
  }
def pascal_tailrec(c:Int, r:Int):Int= {
  def pascal2(c: Int, r: Int, acc_num: Int, acc_den: Int): Int = {
    require(c <= r)
    require(c >= 0)
    require(r >= 0)

    if (c == 0) acc_num/acc_den
    else pascal2(c - 1, r - 1, acc_num * r ,acc_den * c)
  }
  pascal2(c, r, 1,1)
}
  def fibonaci_tail_rec(n:Int):Int={
    require(n>=0)
  def fibonaci(n:Int, a1:Int, a2:Int):Int={
    if (n==0) 1
    else if (n==1) a2
    else fibonaci(n-1,a2,a1+a2 )
  }
    fibonaci(n,1,1)
  }
  //println(fibonaci_tail_rec(6))
  //***************************************** vezbe 2 ***************************

  /* 1. funkcija sum koristi linearnu rekurziju napisati je tako da koristi terminalnu rekurziju
  def sum(f: Int => Double)(a: Int, b:Int): Double = {
    if (a>b) 0 else f(a) + sum(f)(a+1, b)
  }
   */

  def sum(f: Int => Double)(a: Int, b:Int): Double = {

    @tailrec
    def sum_2(a:Int, acc:Double):Double = {
      if (a>b) acc
      else sum_2(a+1, acc+f(a))
    }
    sum_2(a, 0)
  }

  def sameNum(x:Int):Double = x

  val zbir=sum(sameNum)(_,_)

  println(zbir(1,5))

  // 2.  nAPISATI FUNKCIJU KOJA RACUNA PROIZVOD na osnovu prethodne funkcije
  def mul(f: Int => Double)(a: Int, b:Int): Double = {

    @tailrec
    def mul_2(a:Int, acc:Double):Double = {
      if (a>b) acc
      else mul_2(a+1, acc*f(a))
    }
    mul_2(a, 1)
  }

  val proizvod=mul(sameNum)(_,_)

  println(proizvod(1,5))

  // 3.  na osnovu prethodne funkcije napisati funkciju za faktorijel

  val faktorijel=mul(sameNum)(1,_)

  println(faktorijel(6))

  // 4.  Generalizovati pristup

  def generalisation(f:Int=> Double, operation: (Double, Double)=>Double, neutral: Double )(a:Int, b:Int ):Double = {
    @tailrec
    def gen(a:Int, acc:Double):Double = {
      if (a>b) acc
      else gen(a+1, operation(acc, f(a)))
    }
    gen(a, neutral)
  }

  def mnozenje(x:Double, y:Double):Double = {x*y}
  def factorial(n:Int)= generalisation(sameNum, mnozenje,1)(1,n);

  println(factorial(5))

  // 5.  Napisati funkciju koja pravi kerifikovanu verziju funkcije sa dva parametra tipa Int i ciji je rezultat tipa int
  // def carry(f:(Int, Int) =>Int) : Int => Int =>Int

  def curry(f:(Int, Int) =>Int) : Int => Int =>Int = {
    def g (x:Int)(y:Int):Int = f(x,y)
    g
  }

  val cb = curry( (x:Int, y:Int) => x*y )
  println(cb(1)(5))

  // 6. Napisati funkciju koja za dve funkcije f i g koje slikaju int u int vraca funkciju koja
  // komponuje te dve funkcije g(f(x))

  def compose(f:Int=>Int,g:Int=>Int):(Int=>Int)= {
    //x=>g(f(x))
    def k(x:Int)=g(f(x))
    k
  }

  // 7 napraviti komponovanu funkciju koja vraca f(f(f(F(f(f(x)))) (100x f)

  def apply_n(f:Int => Int, n: Int): (Int => Int) = {
    def apply_n_tr(n: Int, acc: (Int => Int)):(Int => Int)  = {

      //if (n == 0) (x:Int) => x
      if (n == 0) acc
      else apply_n_tr(n-1, x => f(acc(x)))
    }
    apply_n_tr(n, (x:Int) => x)
  }
  def pow2(x:Int):Int = x*x
  val pow4 = apply_n(pow2, 2)
  //assert( pow4(2) == 16)
  println(pow4(2) )

}