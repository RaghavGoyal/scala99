import scala.util.Random

object Scala99List {

  def lastElemet(list: List[Int]): Int = {
    list match{
      case a :: Nil => a
      case _::b => lastElemet(b)
    }
  }

  def lastSecondElement(list: List[Int]): Int = {
    list match{
      case a::(_::Nil) => a
      case a::b => lastSecondElement(b)
    }
  }

  def findNElement(i: Int, list: List[Int]):Int={
    list match{
      case a::b if i==0 => a
      case a::b => findNElement(i-1,b)
    }
  }

  var count = 0
  def noOfElements(list: List[Int]) :Int ={
    list match{
      case Nil => count
      case a::b => count += 1
                   noOfElements(b)
    }
  }

  def reverseList(list: List[Int]): List[Int] ={
    list match{
      case Nil => Nil
      case a::Nil => List(a)
      case a::b => reverseList(b) :+ a
    }
  }

  def isPalindrome(list: List[Int]): Boolean ={
    reverseList(list)==list
  }

 def flatten[Any](list: List[Any]): List[Any] = {
      var retList = List[Any]()
      list.foreach ( e => e match {
        case a: List[Any] => retList = retList ::: flatten(a)
        case b: Any => retList = retList :+ b
      })
      retList
    }

  def removeConsecutiveDuplicates(list: List[Int]): List[Int] = {
    list match {
      case Nil => List()
      case a :: Nil => List(a)
      case a :: tail => {
        if (a == tail.head) removeConsecutiveDuplicates(tail) else a :: removeConsecutiveDuplicates(tail)
      }
    }
  }


  def packConsecutiveList[T](list: List[T]): List[List[T]] = {
    list match {
      case a :: b =>
        val t = b.span(e => e == a)
        (a +: t._1) +: packConsecutiveList(t._2)
      case Nil => Nil
    }
  }

  def lengthEncode[T](list: List[T]): List[(Int,T)] = {
    list match{
      case a :: b =>
        val t = b.span(e => e == a)
        ((t._1).length+1, a) +: lengthEncode(t._2)
      case Nil => Nil
    }
  }

  def lengthEncodeModified(list: List[Int]): List[Any] = {
    list match{
      case a :: b => {
        val t = b.span(e => e == a)
        if (t._1.length == 0)
          a +: lengthEncodeModified(t._2)
        else
          ((t._1).length + 1, a) +: lengthEncodeModified(t._2)
      }
      case Nil => Nil
    }
  }

  def decode[T](encodedList: List[Any]): List[T] = {

    def decodeR(encoded: List[Any], result: List[T]): List[T] = encoded match {
      case ((n: Int, e: T) :: xs) => decodeR(xs, result ++ List.fill(n)(e))
      case ((e: T) :: xs) => decodeR(xs, result :+ e)
      case Nil => result
    }
    decodeR(encodedList, List())
  }


  def duplicate[T](list: List[T]): List[T] = {
      list match {
        case Nil => List()
        case a::Nil => List(a,a)
        case a::tail => a::a::duplicate(tail)
      }
  }

  def duplicateNTimes[T](k:Int, list: List[T]): List[T] = {
    list match{
      case Nil => List()
      case a::Nil => List.fill(k)(a)
      case a::tail => List.fill(k)(a) ++ duplicateNTimes(k,tail)
    }
  }


//    def removeN[T](i: Int, list: List[T]): List[T] = {
//
//  }

  def splitList[T](list: List[T], i: Int): (List[T],List[T]) = {
    (list,i) match{
      case (Nil, _) => (Nil,Nil)
      case (list,0) => (Nil,list)
      case (head::tail,n) => val(p,q) = splitList(tail, n-1)
                             (head::p , q)
    }
  }

  def sliceList[T](from: Int, to: Int, list: List[T]): List[T] = {
    splitList(splitList(list,to)._1,from)._2
  }


  def rotateLeft[T](list: List[T], i: Int): List[T] = {
    splitList(list, i)._2 ++ splitList(list, i)._1
  }


  def removeOnlyK[T](list: List[T], k:Int): List[T] = {
    splitList(list, k)._1 ++ splitList(list, k)._2.tail
  }


  def insertAt[T](list: List[T], position: Int, value: Int) = {
    (splitList(list, position)._1 :+ value) ++ splitList(list, position)._2
  }

  def listFromRange(from: Int, to: Int): List[Int] = {
    (from,to) match{
      case (0,0) => List();
      case (from,to) if to < from => Nil
      case (from, to) => from :: listFromRange(from+1,to)
    }
  }

  def returnRandomly[T](list: List[Int], i: Int): List[Int] = {
    var tempList=list
//    println(tempList)
    do
      tempList = removeOnlyK(tempList,Random.nextInt(tempList.length))
    while(tempList.length != i)

    tempList
  }

  def lotto(n: Int, m: Int): List[Int] ={
    var list = listFromRange(0,m)
    while (list.length > n)
      list = removeOnlyK(list,Random.nextInt(list.length))
    list
  }

//  def randomPermutation(list: List[Int]): List[Int] = {
//    val tempList = List()
//    while(tempList.length != list.length){
//      println(tempList)
//      tempList ++ returnRandomly(list,1)
//    }
//    tempList
//  }

  def getCombinations[T](list: List[T], i: Int): List[List[T]] = {
    list match{
      case Nil => Nil
      case head::tail if i <= 0 => List()
      case head::tail if i == 1 => list.map(List(_))
      case head::tail => getCombinations(tail, i-1).map(head :: _) ::: getCombinations(tail, i)
    }
  }


  def sortByLength[T](list: List[List[T]]): List[List[T]] = {
    list.sortBy(_.length)
  }

  def main(args:Array[String]): Unit ={
    val list = List(1,2,2,2,3,4,4,4,6,6,4,2,4,4,6,5,5,5,5,3,45,45,45,6,34)
    val list2 = List(1,2,3,4,5,4,3,2,1)
    val list3 = List(List(1,2,3),List(4,5,6),List(7,8,9),10)
    val list4 = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    val list5 = List(List(1,2),List(3),List(2,3,4,5,6), List(2,3,4))
    println(s"last element of list is:"+lastElemet(list))         //p1

    println(s"last second element of list is: " + lastSecondElement(list) )  //p2

    println(s"5th element is: "+findNElement(5,list))    //p3

    println(s"length of list is: "+noOfElements(list))   //p4

    println(s"the original list is: "+ list)         //p5
    println(s"the list after reversal is: "+ reverseList(list))  //p5

    println(s"check for palindrome: " +isPalindrome(list)) //p6 false
    println(s"check for palindrome: " +isPalindrome(list2)) //p6 true

    println(s"List before flatten is: "+list3) //p7
    println(s"List after flatten is: "+flatten(list3)) //p7

    println(s"List after removing consecutive duplicates: "+removeConsecutiveDuplicates(list))  //p8

    println(s"consecutive duplicates packed into list look like: "+packConsecutiveList(list)) //p9

    println(s"length encoded list appears as: "+lengthEncode(list))    //p10

    println(s"Modified length encoded list appears as: "+lengthEncodeModified(list))    //p11

    println(s"Decoded length encoding looks like: "+decode(list4))   //p12

    println(s"duplicated elements of list appear as: " + duplicate(list2))  //p14

    println(s"duplicated elements of list appear as: " + duplicateNTimes(4,list2))    //p15

//    println(s"list after removing every nth element looks like: " + removeN(3,list2))     //p16   //logic -to ask

    println(s"List after splitting into two parts is: "+ splitList(list,5))      //p17

    println(s"Slice of list looks as: "+sliceList(3,7,list))      //p18

    println(s"rotating list elements to left: " +rotateLeft(list2, 4))    //p19

    println(s"removing kth element from list: "+ removeOnlyK(list2, 5))    //p20

    println(s"insert at given position: "+ insertAt(list2,3,50))   //p21

    println(s"List from range:"+ listFromRange(10,20))    //p22

    println(s"randomly printing given no of elements: "+returnRandomly(list,5))   //p23

    println(s"lotto: "+lotto(6,49))   //p24

//    println("check1")

//    println(s"getting random permutation of list: "+ randomPermutation(list2))  //p25

    println(s"list of combinations: " +getCombinations(list2,2))  //p26

    println(s"sorting by length of sublist: "+ sortByLength(list5))   //p28
  }

}
