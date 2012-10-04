package main.scala

import annotation.tailrec


object CommonMathStuff
{
  def gcd(a: Int,b: Int): Int = {
    if(b ==0) a else gcd(b, a%b)
  }
  def factorial(n: Int): Int = {
    if (n==0) 1 else n * factorial(n-1)
  }
  def factorialTailRecursive(n: Int): Int = {
    @tailrec
    def loop(acc: Int,n: Int): Int = {
          if (n==0) {
            acc
          }
          else {
            loop(acc* n,n-1)
          }
    }
    loop(1,n)
  }
  def cube(x: Int): Int = {
    x*x*x
  }

  def sumOfCubes(a:Int, b: Int): Int ={
    if (a>b) 0 else cube(a) + sumOfCubes(a+1,b)
  }
  def sumOfFactorials (a:Int, b: Int): Int = {
      if (a>b) 0 else factorial(a) + sumOfFactorials(a+1,b)
  }
  def sum(f:Int => Int, a:Int, b:Int): Int ={

      if(a>b){
        0
      }else
      {
        f(a) + sum(f,a+1,b)
      }
  }
  def sumTailRecursive(f:Int => Int,a:Int, b:Int): Int ={
    @tailrec
    def loop(a: Int,acc: Int): Int = {
        if (a>b) {
          acc
        }
        else {
          loop(a+ 1 ,f(a) + acc)
        }
    }
    loop(a,0)
  }
  def sumCubes(a:Int,b:Int) = sum(cube,a,b)
  def sumFactorials(a:Int,b:Int) = sum(factorial,a,b)

  def sumCurry(f:Int => Int): (Int,Int) => Int = {
        def sumF(a:Int,b:Int): Int = {
          if(a>b){
            0
          }else
          {
            f(a) + sumF(a+1,b)
          }
        }
        sumF
    }
  def sumCurryCube = sumCurry(x => x*x*x)

  def sumCurryFactorials = sumCurry(factorial)

  // an example of currying
  def product(f: Int => Int)(a:Int,b:Int): Int = {
    if(a>b) 1
    else f(a) + product(f)(a+1,b)
  }
  def productWithMapReduce(f: Int => Int)(a:Int,b:Int): Int = mapReduce(f,(x,y) => x*y,1)(a,b)

  def mapReduce(f:Int =>Int, combine: (Int,Int) => Int, zero: Int)(a:Int,b:Int): Int = {
    if(a>b){
      zero
    }
    else{
      combine(f(a),mapReduce(f,combine,zero)(a+1,b))
    }
  }

  def factWithProduct(n:Int) = product(x => x)(1,n)


  val tolerance = 0.0001

  def isCloseEnough(x: Double,y:Double) = math.abs((x-y)/x)/x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
      def iterate(guess: Double): Double = {
          val next = f(guess)
          if (isCloseEnough(guess,next)) next
          else
          iterate(next)
      }
      iterate(firstGuess)
  }

  def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y))(1)
  def averageDamp(f: Double => Double)(x: Double) = (x + f(x))/2

  def main(args: Array[String]) {
    println(gcd(25,15))
    println(factorial(5))
    println(factorialTailRecursive(5))
    println(sumOfCubes(1,3))
    println(sumCubes(1,3))
    println(sumOfFactorials(1,3))
    println(sumFactorials(1,3))
    println(sum(cube,2,4))
    println(sumTailRecursive(x => x * x, 3 , 5))
    println("sumCurryCube " + sumCurryCube(1,3))
    println("sumCurryFactorials " + sumCurryFactorials(1,3))
    println(factWithProduct(5))
    println("product1,3 " + product(x => x * x)(1,3))
    println("product1,4 " + product(x => x * x)(1,4))
    println("product2,4 " + product(x => x * x)(2,4))
    println("product1,5 " + product(x => x * x)(1,5))
    println("product2,5 " + product(x => x * x)(2,5))
    println("product4,3 " + product(x => x * x)(4,3))
    println("productWithMapReduce2,4 " + productWithMapReduce(x => x * x)(2,4))
    println("productWithMapReduce3,4 " + productWithMapReduce(x => x * x)(3,4))
    println("productWithMapReduce4,4 " + productWithMapReduce(x => x * x)(4,4))
    println("productWithMapReduce4,3 " + productWithMapReduce(x => x * x)(4,3))
    println("sqrt(4) " + sqrt(4))
    println("sqrt(2) " + sqrt(2))
  }
}
//def sqrt(x: Double) = fixedPoint(y => x/y)(1)
//def sqrt(x: Double) = fixedPoint(y => (y + x/y)/2)(1)
