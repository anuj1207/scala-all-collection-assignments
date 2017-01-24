
import com.knoldus.list.student.StudentMarksManagement

/**
  * Created by ANUJ_SAXENA on 1/23/2017.
  */


case class ScoreCard(studentId: Long, marks: Map[Int, Float], percentage: Float)




class StudentManagement {


  object Gender extends Enumeration{
     type Gender = Value
    val Male,Female = Value
  }

  case class Student(id: Long, name: String,gender:Gender.Value)

  val studentList = List(Student(1,"Raman", Gender.Female), Student(2,"Prashant",Gender.Male), Student(3,"Simar",Gender.Female), Student(4, "Jatin",Gender.Male), Student(5,"Gitika",Gender.Female), Student(6,"Anuj",Gender.Male), Student(7,"Kunal",Gender.Male), Student(8,"Sonu",Gender.Male),Student(9,"Anmol",Gender.Male), Student(10,"Rahul",Gender.Male))
  val studentMarksManagement = new StudentMarksManagement()

  val scorecardList = for(i<- 1 to 10) yield{
    ScoreCard(i, generateMarksMap(i), studentMarksManagement.getPercentageWithId().filter(x=> x._1 == i)(0)._2)
  }
  def generateMarksMap(studentId: Int):Map[Int,Float] = {
      studentMarksManagement.marksList.filter(_.studId==studentId).map(x => (x.subId , x.marksObtained)).toMap
    }


  def getScoreMap():Map[String,AnyRef] = {
    studentMarksManagement.studentList.sortBy(_.id).zip(scorecardList.sortBy(_.studentId)).map(x=> (x._1.name,x._2)).groupBy(_._1).mapValues(x=> x.map(x=>x._2))
  }

  def getScoreCard(studName:String) = {
    if(getScoreMap().contains(studName))
      getScoreMap()(studName)
    else
      "No Data Found"
  }

  def getNameById(studId:Long): String ={
    studentList.filter(x=>x.id == studId)(0).name
  }

  def checkGender(id : Long) = {

    studentList.filter(x => x.id == id)(0).gender

  }

  def getScoreCardByGender() = {

    scorecardList.toList.partition(x => checkGender(x.studentId) == Gender.Male)

  }

  def getScoreCardByGenderMoreThanFifty() = {
    (getScoreCardByGender()._1.filter(_.percentage>=50),getScoreCardByGender()._2.filter(_.percentage>=50))
  }

  def findingSimilarPercentage() = {
    //getScoreCardByGender()._1.zip(getScoreCardByGender()._2).filter(x => x._1.percentage == x._2.percentage)
    val genderBasedScoreCard = getScoreCardByGender()
    val lastList = for(maleScore <- genderBasedScoreCard._1; femaleScore <- genderBasedScoreCard._2 if(maleScore.percentage == femaleScore.percentage))yield {
      ((getNameById(maleScore.studentId),maleScore.percentage),(getNameById(femaleScore.studentId),femaleScore.percentage))
    }
    println(lastList)
  }

  def findingUniquePercentage() = {
    //getScoreCardByGender()._1.zip(getScoreCardByGender()._2).filter(x => x._1.percentage != x._2.percentage)
   /* val genderBasedScoreCard = getScoreCardByGender()
    val lastList = for(maleScore <- genderBasedScoreCard._1; femaleScore <- genderBasedScoreCard._2 if(maleScore.percentage != femaleScore.percentage))yield {
      ((getNameById(femaleScore.studentId),femaleScore.percentage))
    }
    println(lastList)*/
   val genderBasedScoreCard = getScoreCardByGender()
    genderBasedScoreCard._2.toSet - findingSimilarPercentage().toSet

  }
}

  object StudentManagement {
     def main(args: Array[String]): Unit = {
      val studentManagement = new StudentManagement

       println("\n generated map name->ScoreCard\n")

       println(studentManagement.getScoreMap())

       println("\n Printing Score Card By Name\n")

       println(studentManagement.getScoreCard("Bhavya"))//no student is with name bhavya

       println(studentManagement.getScoreCard("Anuj"))//duplicate name

       println("\n Printing Score Card By Gender\n")

       println(studentManagement.getScoreCardByGender())

       println("\n Printing Score Card By Gender\n")

       println(studentManagement.getScoreCardByGenderMoreThanFifty())

       println("\n Finding similar percentage\n")

       println(studentManagement.findingSimilarPercentage())

       println("\n Finding Unique percentage\n")

       println(studentManagement.findingUniquePercentage())

       println(studentManagement.getNameById(1))

     }
}