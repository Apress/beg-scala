object MyMules {
  def myMules(name: String) = name match {
    case "Elwood" | "Madeline" => Some("Cat")
    case "Archer" => Some("Dog")
    case "Pumpkin" | "Firetruck" => Some("Fish")
    case _ => None
  }
}
