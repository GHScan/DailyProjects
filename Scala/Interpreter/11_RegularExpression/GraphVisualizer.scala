object GraphVisualizer {

  private def escape(s : String) : String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(s)).toString
  }

  trait Node {
    def shape = "circle"
    def label : String
    def edges : List[(String, Node)]
  }

  def saveImage(imgPath : String, startNode : Node) {
    var nodes = Set[Node]()
    val edgeStatements = new StringBuilder()

    def iterate(node : Node) {
      if (nodes(node)) return
      nodes += node
      node.edges.foreach {
        case ((edgeLabel, target)) =>
          edgeStatements ++= s"""${node.label}->${target.label}[label=${escape(edgeLabel)}];\n"""
          iterate(target)
      }
    }
    iterate(startNode)

    val script = s"""
      digraph name {
        $edgeStatements
        ${nodes.map { s => s"""${s.label}[shape=${s.shape}]""" }.mkString("\n")}
      }
      """

    import scala.sys.process._
    s"""dot -Tpng -o $imgPath""" #< new java.io.ByteArrayInputStream(script.getBytes("UTF-8")) !
  }
}