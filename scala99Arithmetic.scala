object scala99Arithmetic {

  def isPrime(num: Int): Boolean = {
    if(num == 1 || num == 0) false
    else  !(2 to num-1).exists(x => num % x == 0)
  }

  def gcd(i: Int, i1: Int): Int = {
    (i, i1) match{
      case (0,a) => a
      case (a,0) => a
      case (a,b) if(a==b) => a
      case (a,b) if(a>b) => gcd(a-b,b)
      case (a,b) => gcd(a,b-a)
    }
  }

  def coprime(i: Int, i1: Int): Boolean = {
    gcd(i,i1) == 1
  }


  def getListOfPrime(from: Int, to: Int): List[Int] = {
    (from to to).toList.filter(e => isPrime(e)==true)
  }

  def main(args: Array[String]) = {

    println(s"check for prime: 97:   "+isPrime(1))  //p31  //false
    println(s"check for prime: 8:   "+isPrime(8))  //p31   //false
    println(s"check for prime: 97:   "+isPrime(97))  //p31  //true

    println(s"GCD for 2 and 0: "+gcd(2,0))   //p32   //2
    println(s"GCD for 0 and 7: "+gcd(7,0))   //p32   //7
    println(s"GCD for 45 and 45: "+gcd(45,45))   //p32   //45
    println(s"GCD for 63 and 36: "+gcd(63,36))   //p32   //9
    println(s"GCD for 36 and 63: "+gcd(36,63))   //p32   //9

    println(s"Check for co-prime: 35 and 64: "+coprime(35,64))  //p33 true
    println(s"Check for co-prime: 35 and 64: "+coprime(3,99))  //p33 false

//    println(s"Prime factors of 10: " +primeFactors(10))  //p34

    println(s"Getting list of prime numbers from 10 to 30: "+getListOfPrime(10,30)) //P39
    println(s"Getting list of prime numbers from 10 to 30: "+getListOfPrime(0,30)) //P39
    println(s"Getting list of prime numbers from 10 to 30: "+getListOfPrime(0,1)) //P39

    
  }

}
