package fr.uge.ia

import com.github.javafaker.Faker
import org.apache.jena.rdf.model.{ModelFactory, ResourceFactory}
import java.io.{File, FileOutputStream, StringWriter}
import java.util.Random

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule



sealed trait Gender {
  val value : Integer
}

case object Male extends Gender{override val value = 0}
case object Femelle extends Gender{override val value = 1}

sealed trait Vaccin {
  val value : Integer
}

case object Pfizer extends Vaccin{override val value = 0}
case object Moderna extends Vaccin{override val value = 1}
case object AstraZeneca extends Vaccin{override val value = 2}
case object SpoutnikV extends Vaccin{override val value = 3}
case object CanSinoBio extends Vaccin {override val value = 4}

class Lubm  (val dbSource: String){
  val faker = new Faker(new Random(24))

  val typeProperty = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
  val teacherObj = "http://swat.cse.lehigh.edu/onto/univ-bench.owl#FullProfessor"
  val model = ModelFactory.createDefaultModel()
  def load() =  model.read(dbSource,"TTL")
  def showModel() : Unit =  println("is empty ? "+model.isEmpty())
  def size() = model.size()
  def listAllTypes() = {
    val rdfType = model.createProperty(typeProperty)
    val it = model.listObjectsOfProperty(rdfType)
    it.toList
  }
  val types = listAllTypes()

  def listAllFullProffessor() = {
    val rdfType = model.createProperty(typeProperty)
    val rdfObj = model.createResource(teacherObj)
    val it = model.listSubjectsWithProperty(rdfType,  rdfObj)
    it.toList
  }



  def generate_gender(): Gender ={
    val rand = new Random()
    rand.nextInt(2)  match {
      case 0 => Male
      case 1 => Femelle
    }
  }

  def generate_vaccin(): Vaccin ={
    val rand = new Random()
    rand.nextInt(5) match {
      case 0 => Pfizer
      case 1 => Moderna
      case 2 => AstraZeneca
      case 3 => SpoutnikV
      case 4 => CanSinoBio
    }
  }

  def generateInfo() ={
    val faker = new Faker(new Random(24))
    val id  = faker.idNumber().valid()
    val fname = faker.name().firstName()
    val lname = faker.name().lastName()
    val birthdate = faker.date().birthday(30, 70)
    val gender = generate_gender()
    val zipcode = faker.address().zipCode()
    val state = faker.address().state()
    val vaccin = generate_vaccin()
    (id,fname, lname,birthdate.toString,gender.toString,zipcode, state, vaccin.toString)
  }

  def addInfoToProfs()={
    val profs = listAllFullProffessor()
    profs.forEach(prof => {
      val info = generateInfo()
      val subject =  prof
      val propertyId = ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#id")
      val propertyFname = ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#firstName")
      val propertyLname = ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#lastName")
      val propertybirthdate = ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#birthdate")
      val propertygender = ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#gender")
      val propertyzipcode = ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#zipcode")
      val propertystate = ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#state")
      val propertyvaccin = ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#vaccin")
      val objectId = ResourceFactory.createStringLiteral(info._1)
      val objectFname = ResourceFactory.createStringLiteral(info._2)
      val objectLname = ResourceFactory.createStringLiteral(info._3)
      val objectbirthdate =ResourceFactory.createStringLiteral(info._4.toString)
      val objectgender =ResourceFactory.createStringLiteral(info._5.toString)
      val objectzip =ResourceFactory.createStringLiteral(info._6)
      val objectstate =ResourceFactory.createStringLiteral(info._7)
      val objectvaccin =ResourceFactory.createStringLiteral(info._8.toString)
      val idStmt = ResourceFactory.createStatement(subject, propertyId, objectId)
      val fnameStmt = ResourceFactory.createStatement(subject, propertyFname, objectFname)
      val lnameStmt = ResourceFactory.createStatement(subject, propertyLname, objectLname)
      val birthdateStmt = ResourceFactory.createStatement(subject, propertybirthdate, objectbirthdate)
      val genderStmt = ResourceFactory.createStatement(subject, propertygender, objectgender)
      val zipStmt = ResourceFactory.createStatement(subject, propertyzipcode, objectzip)
      val stateStmt = ResourceFactory.createStatement(subject, propertystate, objectstate)
      val statevaccin = ResourceFactory.createStatement(subject, propertyvaccin, objectvaccin)
      model.add(idStmt)
      model.add(fnameStmt)
      model.add(lnameStmt)
      model.add(birthdateStmt)
      model.add(genderStmt)
      model.add(zipStmt)
      model.add(stateStmt)
      model.add(statevaccin)
    })
  }

  def toJson() ={
    val mapper = new ObjectMapper()
    mapper.registerModule(DefaultScalaModule)

    val out = new StringWriter()
    mapper.writeValue(new File("src/main/resources/test.json"), generateInfo())
    val json = out.toString()
    System.out.println(json)
  }


  def gen()={
    //System.out.println(model.supportsSetOperations())
    //System.out.println(model.supportsTransactions())
    //val out = new FileOutputStream(new File("C:\\imad\\M2\\IntelligenceA\\Master-IA-Project\\src\\main\\resources\\out.xml"))
    val out = new FileOutputStream(new File("src/main/resources/out.xml"))
    model.write(out,null)
    out.close
  }

}

object Lubm {
  def apply(dbSource : String) = new Lubm(dbSource)
}
