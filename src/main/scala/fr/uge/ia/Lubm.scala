package fr.uge.ia

import org.apache.jena.rdf.model.ModelFactory
class Lubm  (val dbSource: String){
  val model = ModelFactory.createDefaultModel()
  def load() =  model.read(dbSource,"TTL")
  def showModel() : Unit =  println("is empty ? "+model.isEmpty())
  def size() = model.size()
}

object Lubm {
  def apply(dbSource : String) = new Lubm(dbSource)
}
