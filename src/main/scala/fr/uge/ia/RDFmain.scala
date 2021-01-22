package fr.uge.ia

object RDFmain extends App {
  val db = Lubm(LabelBase.INPUT)
  db.load()
  db.showModel()
  println("Size of model ="+db.size())
}
