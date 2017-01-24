package com.knoldus.list.student


class StudentMarksManagement {

  val studentList = List(Student(1, "Raman"), Student(2, "Anuj"), Student(3, "Simar"), Student(4, "Jatin"), Student(5, "Gitika"), Student(6, "Prashant"), Student(7, "Kunal"), Student(8, "Sonu"), Student(9, "Anmol"), Student(10, "Rahul"))
  val marksList = List(Marks(1, 1, 90), Marks(1, 2, 89), Marks(1, 3, 70), Marks(1, 4, 72), Marks(1, 5, 59), Marks(1, 6, 90), Marks(1, 7, 74), Marks(1, 8, 60), Marks(1, 9, 56), Marks(1, 10, 70),
    Marks(2, 1, 88), Marks(2, 2, 89), Marks(2, 3, 68), Marks(2, 4, 93), Marks(2, 5, 90), Marks(2, 6, 49), Marks(2, 7, 64), Marks(2, 8, 92), Marks(2, 9, 71), Marks(2, 10, 75),
    Marks(3, 1, 60), Marks(3, 2, 60), Marks(3, 3, 88), Marks(3, 4, 91), Marks(3, 5, 80), Marks(3, 6, 78), Marks(3, 7, 88), Marks(3, 8, 91), Marks(3, 9, 95), Marks(3, 10, 88),
    Marks(4, 1, 60), Marks(4, 2, 60), Marks(4, 3, 88), Marks(4, 4, 89), Marks(4, 5, 95), Marks(4, 6, 92), Marks(4, 7, 69), Marks(4, 8, 85), Marks(4, 9, 80), Marks(4, 10, 60),
    Marks(5, 1, 60), Marks(5, 2, 60), Marks(5, 3, 88), Marks(5, 4, 80), Marks(5, 5, 84), Marks(5, 6, 81), Marks(5, 7, 59), Marks(5, 8, 68), Marks(5, 9, 90), Marks(5, 10, 88))

  // Returning Pass/Fail Count
  def checkStatus(subjectId: Int, percentage: Int, resultType: String): String = {
    resultType.toLowerCase match {
      case "pass" => s"Pass count: ${marksList.flatMap(x => if (x.subId == subjectId && x.marksObtained >= percentage) Some(x) else None).length}"

      case "fail" => s"Fail count: ${marksList.flatMap(x => if (x.subId == subjectId && x.marksObtained < percentage) Some(x) else None).length}"
    }
  }

  // Returning tuple of Top/Bottom List
  def topBottomScorer(subjectId: Int, count: Int, criteria: String): List[(String, Float)] = {

    criteria.toLowerCase match {

      case "top" => {
        marksList.filter(subjectId == _.subId).sortBy(_.studId).zip(studentList.sortBy(_.id)).sortWith(_._1.marksObtained >= _._1.marksObtained).slice(0, count).map(x => (x._2.name, x._1.marksObtained))
      }

      case "bottom" => {
        marksList.filter(subjectId == _.subId).sortBy(_.studId).zip(studentList.sortBy(_.id)).sortWith(_._1.marksObtained < _._1.marksObtained).slice(0, count).map(x => (x._2.name, x._1.marksObtained))
      }

    }
  }

  def OverallTopBottomScorer(criteria: String, count: Int) = {

    criteria.toLowerCase match {

      case "top" => {

        getPercentageWithName().slice(0, count)

      }
      case "bottom" => {
        val tempList = getPercentageWithName()
        tempList.slice(count - 1, tempList.length)
      }
    }

  }

  def getPercentageWithId() = {
    marksList.groupBy(_.studId).toList.sortBy(_._1).map(x => (x._1, getPercent(x._2.map(y => {
      y.marksObtained
    })))).zip(studentList.sortBy(_.id)).map(x => (x._2.id, x._1._2)).sortWith(_._2 > _._2)
  }

  def amountOfScholership(percentage: Float, good_scholarship: Int, normal_scholarship: Int) = {
    getPercentageWithName().map(x => (x._1, if (x._2 >= percentage) good_scholarship else normal_scholarship))
  }

  def PassOrFail(criteria: String, percentage: Float) = {
    criteria.toLowerCase match {

      case "pass" => {
        val passList = getPercentageWithName().filter(x => x._2 >= percentage)
        (passList, s"pass :${passList.length}")
      }
      case "fail" => {
        val failList = getPercentageWithName().filter(x => x._2 < percentage)
        (failList, s"fail :${failList.length}")
      }

    }
  }

  def getPercentageWithName() = {
    marksList.groupBy(_.studId).toList.sortBy(_._1).map(x => (x._1, getPercent(x._2.map(y => {
      y.marksObtained
    })))).zip(studentList.sortBy(_.id)).map(x => (x._2.name, x._1._2)).sortWith(_._2 > _._2)
  }

  def getPercent(list: List[Float]): Float = list.foldLeft(0.0f)(_ + _) / 5

  def getting95Percent() = {
    getPercentageWithName().filter(x => x._2 >= 75)
  }

  def generateReportCard() = {
    marksList.groupBy(_.studId).toList.sortBy(_._1).zip(studentList.sortBy(_.id)).map(x => (x._2.name, x._1._2.map(x => (x.marksObtained, getPercentage()))))
  }

  def getPercentage() = {
    marksList.groupBy(_.studId).toList.sortBy(_._1).map(x => getPercent(x._2.map(y => {
      y.marksObtained
    })))
  }

  case class Student(id: Long, name: String)

  case class Marks(subId: Int, studId: Long, marksObtained: Float)

}

//companion object
object StudentMarksManagement {

  def main(args: Array[String]): Unit = {
    val studentMarksManagement = new StudentMarksManagement()

    println(studentMarksManagement.checkStatus(2, 30, "Fail"))

    println("2:Top/bottom scorer")
    studentMarksManagement.topBottomScorer(1, 3, "top").map(x => println(s"${x._1}  ${x._2}"))

    println("3:OverAll Top/Bottom scorer")
    studentMarksManagement.OverallTopBottomScorer("top", 3).map(x => println(s"${x._1}  ${x._2}"))

    println("4:Amount of scholership")
    studentMarksManagement.amountOfScholership(80, 2000, 500).map(x => println(s"${x._1}  ${x._2}"))

    println("5:Pass Or Fail")
    val temp = studentMarksManagement.PassOrFail("pass", 60)
    println(temp._2)
    temp._1.map(x => println(s"${x._1}  ${x._2}"))

    println("6:95% or above")
    studentMarksManagement.getting95Percent().map(x => println(s"${x._1}  ${x._2}"))

    println("\n7th pending\n")


    println(studentMarksManagement.getPercentageWithId())
    //println(studentMarksManagement.generateReportCard())
    //println(studentMarksManagement.getPercentageWithName())
  }
}