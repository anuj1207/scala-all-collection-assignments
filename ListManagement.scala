package com.knoldus.list.assignment

/**
  * Created by ashish and anuj on 19/1/17.
  */
class ListManagement {

 // var list:List[List[Any]]=List()
  //var list2:List[Int]=List()

  def findPrimeNumbers(l:List[Int]) = l.flatMap( x => isPrime(x,2) )

  private def isPrime(num:Int,count:Int):Option[Any] = {

    if(count > num/2)
      Some(num)
    else
    if(num % count == 0)
      None
    else isPrime(num, count+1)

  }


  def printTable(l:List[Int]) = l.map(numTable(_))

  private def numTable(num:Int) = {

    for(n:Int <- 1 to 10){

      print(s"${num*n} ")

    }
    println()

  }

  def aggregateList(list1: List[Any],list2: List[Any]) : List[List[Any]] = {
    buildList(list1.size,list1,list2,List())
  }

  private def buildList(listSize:Int,list1:List[Any],list2:List[Any],list:List[List[Any]]): List[List[Any]]={


    if(listSize<=0)
      list.reverse
    else
    {
      val list3 = List(list1(listSize),list2(listSize)) :: list
      buildList(listSize-1,list1,list2,list3)
    }


  }

  def sumOfList(list1:List[Int],list2:List[Int]):List[Int] = {


    sumList(0,list1,list2,List[Int]())
  }

  private def sumList(listSize:Int,list1:List[Int],list2:List[Int],list:List[Int]):List[Int] = {
    if(listSize<=0)
      list.reverse
    else
    {
      val list3 = (list1(listSize)+list2(listSize)) :: list
      sumList(listSize-1,list1,list2,list3)
    }
    //list.reverse
  }
}


//companion object
object ListManagement{
  def main(args: Array[String]) {
    val listManagement = new ListManagement()
    //println(listManagement.findPrimeNumbers(List(3,4,5,6)))
    //    listManagement.printTable(List(3,4,5,6))
    //    println(listManagement.aggregateList(List(3,4,5,6),List(1,2,4,5)))
    println(listManagement.sumOfList(List(3,4,5,6),List(1,2,4,5)))
  }
}