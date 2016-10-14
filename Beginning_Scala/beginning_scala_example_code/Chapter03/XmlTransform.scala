import scala.xml._
import scala.xml.transform._

val removeIt = new RewriteRule {
    override def transform(n: Node): NodeSeq = n match {
      case e: Elem if (e \ "@instruction").text == "remove" => NodeSeq.Empty
      case n => n
    }
  }

val addIt = new RewriteRule {
    override def transform(n: Node): NodeSeq = n match {
      case e: Elem if (e \ "@instruction").text == "add" =>
        new Elem(e.prefix, e.label, 
          e.attributes.remove("instruction"),
          e.scope, 
          transform(e.child) ++ <added>I added this</added> :_*)
      
      case n => n
    }
  }

val xmlBooks =
        <books instruction="update">
          <book instruction="remove" name="book1" status=""/>
          <book instruction="add" name="book2" status=""/>
        </books>

println(new RuleTransformer(addIt, removeIt).transform(xmlBooks))
