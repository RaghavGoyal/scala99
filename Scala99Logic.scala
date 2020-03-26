object Scala99Logic {

  def not(bool: Boolean): Boolean = {
    bool match {
      case true => false
      case false => true
    }
  }

  def and(input1: Boolean, input2: Boolean): Boolean = {
    (input1,input2) match {
      case (true,true) => true
      case (_,_) => false
    }
  }

  def or(input1: Boolean, input2: Boolean): Boolean = {
    (input1,input2) match {
      case (false, false) => false
      case (_,_) => true
    }
  }

  def nor(input1: Boolean, input2: Boolean): Boolean = {
    not(or(input1,input2))
  }

  def nand(input1: Boolean, input2: Boolean): Boolean = {
    not(and(input1,input2))
  }

  def xnor(input1: Boolean, input2: Boolean): Boolean = {
    or(and(input1,input2),and(not(input1),not(input2)))
  }

  def xor(input1: Boolean, input2: Boolean): Boolean = {
    not(xnor(input1,input2))
  }

  def main(args: Array[String]) : Unit = {

    println(s"Not operation: "+not(true))   //p46
    println(s"Not operation: "+not(false))   //p46

    println(s"AND operation: " +and(true,true))   //p46
    println(s"AND operation: " +and(true,false))   //p46
    println(s"AND operation: " +and(false,true))   //p46
    println(s"AND operation: " +and(false,false))   //p46

    println(s"OR operation: "+or(true,true))   //p46
    println(s"OR operation: "+or(false,true))   //p46
    println(s"OR operation: "+or(true,false))   //p46
    println(s"OR operation: "+or(false,false))   //p46

    println(s"NOR operation: "+nor(true,true))  //p46
    println(s"NOR operation: "+nor(false,true))  //p46
    println(s"NOR operation: "+nor(true,false))  //p46
    println(s"NOR operation: "+nor(false,false))  //p46

    println(s"NAND operation: "+nand(true,true))  //p46
    println(s"NAND operation: "+nand(false,true))  //p46
    println(s"NAND operation: "+nand(true,false))  //p46
    println(s"NAND operation: "+nand(false,false))  //p46

    println(s"XNOR operation: "+xnor(true,true))  //p46
    println(s"XNOR operation: "+xnor(false,true))  //p46
    println(s"XNOR operation: "+xnor(true,false))  //p46
    println(s"XNOR operation: "+xnor(false,false))  //p46

    println(s"XOR operation: "+xor(true,true))  //p46
    println(s"XOR operation: "+xor(false,true))  //p46
    println(s"XOR operation: "+xor(true,false))  //p46
    println(s"XOR operation: "+xor(false,false))  //p46

  }

}
