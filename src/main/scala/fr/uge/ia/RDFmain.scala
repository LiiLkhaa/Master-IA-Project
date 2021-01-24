package fr.uge.ia



object RDFmain extends App {
  val db = Lubm(LabelBase.INPUT)
  db.load()
  db.listAllTypes()
  db.listAllFullProffessor()
  db.generate_gender()
  db.generate_vaccin()
  db.generateInfo()
  db.addInfoToProfs()
  db.gen()
  //db.showModel()

}
