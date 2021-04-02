import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object NBD_01 {




    def main(args: Array[String]) = {


      val daysOfWeek = List("Poniedzialek", "Wtorek", "Sroda", "Czwartek", "Piatek", "Sobota", "Niedziela")
      val separator = ","

      val result_1a = concatUsingForLoop(daysOfWeek, separator)
      println("Zadanie 1a")
      println(result_1a)


      val result_1b = concatUsingForLoopWithFilter(daysOfWeek, separator)
      println("\nZadanie 1b")
      println(result_1b)


      val result_1c = concatUsingWhileLoop(daysOfWeek, separator)
      println("\nZadanie 1c")
      println(result_1c)

      val result_2a = concatUsingRecursion(daysOfWeek)
      println("\nZadanie 2a")
      println(result_2a)


      val result_2b = concatUsingRecursionReverse(daysOfWeek)
      println("\nZadanie 2b")
      println(result_2b)


      val result_3 = concatUsingTailRecursion(daysOfWeek)
      println("\nZadanie 3")
      println(result_3)

      val result_4a = concatUsingFoldLeft(daysOfWeek)
      println("\nZadanie 4a")
      println(result_4a)

      val result_4b = concatUsingFoldRight(daysOfWeek)
      println("\nZadanie 4b")
      println(result_4b)

      val result_4c = concatUsingFoldLeftWithFilter(daysOfWeek)
      println("\nZadanie 4c")
      println(result_4c)

      println("\nZadanie 5")
      transformDict()

      println("\nZadanie 6")
      printTuple((3, "Cukier" , 2.5))

      println("\nZadanie 7")
      testOptionUsage()

      println("\nZadanie 8")
      val inputList_8 = List[Int](3,5,0,1,4,0,1)
      val result_8 = removeZerosFromList(inputList_8)
      println("Input List: " + inputList_8)
      println("Output List: " + result_8)

      println("\nZadanie 9")
      val result_9 = incrementListElements(List[Int](3,5,0,1,4,0,1))
      println("Output List")
      println(result_9)

      println("\nZadanie 10")
      val result_10 = getAbsoluteValuesFromListWithFilter(List[Double](-10.2, -6, -5, 0, 8, 5, 12, 13, 55))
      println("Output List: " + result_10)

    }


    // zadanie 1 a)
    def concatUsingForLoop(daysOfWeek: List[String], separator: String): String ={

      var result = ""

      for (item <- daysOfWeek){
        result = result.concat(item).concat(separator)
      }

      result = result.dropRight(1)

      return result
    }


  // zadanie 1 b)
  def concatUsingForLoopWithFilter(daysOfWeek: List[String], separator: String): String ={

    var result = ""

    for (item <- daysOfWeek if item.toUpperCase.startsWith("P")){
      result = result.concat(item).concat(separator)
    }
    result = result.dropRight(1)

    return result
  }



  // zadanie 1 c)
  def concatUsingWhileLoop(daysOfWeek: List[String], separator: String): String ={

    var result = ""

    var idx = 0

    while (idx < daysOfWeek.size) {
      result = result.concat(daysOfWeek(idx).concat(separator))
      idx += 1
    }

    result = result.dropRight(1)

    return result
  }


  // zadanie 2 a)
  def concatUsingRecursion(daysOfWeek: List[String]): String ={

    val separator: String = ","
    if (daysOfWeek.isEmpty){
        return ""
    } else {
      return daysOfWeek.head + separator + concatUsingRecursion(daysOfWeek.tail)
    }

  }


  // zadanie 2 b)
  def concatUsingRecursionReverse(daysOfWeek: List[String]): String ={

    val separator: String = ","

    if (daysOfWeek.isEmpty){
      return ""
    } else {
      return concatUsingRecursionReverse(daysOfWeek.tail) + separator + daysOfWeek.head
    }
  }


  // zadanie 3
  def concatUsingTailRecursion(daysOfWeek: List[String]): String ={

    val separator: String = ","

    @tailrec
    def concat(daysOfWeek: List[String], accumulator: String): String = {
      daysOfWeek match {
        case Nil => accumulator
        case daysOfWeek => concat(daysOfWeek.tail, accumulator + separator + daysOfWeek.head)
      }
    }
    concat(daysOfWeek, "")
  }


  // example how FoldLeft and FoldRight works
  //  List(1, 3, 8).foldLeft(100)(_ - _) == ((100 - 1) - 3) - 8 == 88
  //  List(1, 3, 8).foldRight(100)(_ - _) == 1 - (3 - (8 - 100)) == -94

  // zadanie 4a
  def concatUsingFoldLeft(daysOfWeek: List[String]): String ={

    val separator: String = ","

    return daysOfWeek.foldLeft(""){ (sum, item) => (sum + item + separator)}

  }


  // zadanie 4b
  def concatUsingFoldRight(daysOfWeek: List[String]): String ={

    val separator: String = ","

    return daysOfWeek.foldRight(""){ (item, sum) => (item + separator + sum)}

  }


  // zadanie 4c
  def concatUsingFoldLeftWithFilter(daysOfWeek: List[String]): String ={
    val separator: String = ","

    return daysOfWeek.filter(_.toUpperCase.startsWith("P")).foldLeft(""){ (sum, item) =>  {sum + item + separator}}
  }


  // zadanie 5
  def transformDict(): Unit ={

    val products = Map("mleko" -> 4.00, "chleb" -> 2.5, "cukier" -> 5.25)

    println("Original products map: " + products)

    // val productsAfterDiscount = products.map{ case (key, value) => (key, value * 0.9) }
    // consider mapValues()
    val productsAfterDiscount = products.mapValues{ _ * 0.9 }

    println("Altered products map: " + productsAfterDiscount)
  }

  // zadanie 6
  def printTuple(tuple: (Int, String, Double)): Unit ={

    println("Whole tuple: " + tuple)
    println("First element: " + tuple._1)
    println("Second element: " + tuple._2)
    println("Third element: " + tuple._3)

  }


  // zadanie 7
  def testOptionUsage(): Unit ={
    val products = Map("mleko" -> 4.00, "chleb" -> 2.5, "cukier" -> 5.25)

    println("Operacja get na mapie zwaraca obiekt typu Option, ktora posiada obiekt Some z wartoscia lub None")
    println(products.get("chleb"))
    println("Po odpakowaniu: " + products.get("chleb").get)
    println(products.get("maslo"))
    println(products.getOrElse("maslo", ""))
    println(products.isEmpty)
    println("Czy zwrocona wartosc jest typu Option?: " + products.get("chleb").isInstanceOf[Option[Double]])
    println("Czy zwrocona wartosc jest typu Option?: " + products.get("maslo").isInstanceOf[Option[Double]])

  }


  // zadanie 8
  def removeZerosFromList(inputList: List[Int]): List[Int] ={

    @tailrec
    def removeZeros(inputList: List[Int], accumulatorList : ListBuffer[Int]) : List[Int] = {
      inputList match {
        case Nil => accumulatorList.toList
        case inputList => {
          if (inputList.head != 0) {
            accumulatorList.append(inputList.head)
          }
          removeZeros(inputList.tail, accumulatorList)
        }
      }
    }

    removeZeros(inputList, ListBuffer())
  }


  // zadanie 9
  def incrementListElements(inputList : List[Int]): List[Int] ={

    println("Input List")
    println(inputList)

    return inputList.map(x => x + 1)
  }


  // zadanie 10
  def getAbsoluteValuesFromListWithFilter(inputList : List[Double]) : List[Double] = {
    println("Input List: " + inputList)

    return inputList.filter( x => { x > -6 && x < 13 }).map( x => x.abs)
  }


}
