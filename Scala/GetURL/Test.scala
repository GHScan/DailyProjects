import scala.util.Random
import scala.collection.mutable
import scala.collection.immutable

object Helper {
  def loadHtmlFromUrl(url : String) : scala.xml.Node = {
    val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
    val source = new org.xml.sax.InputSource(url)
    new scala.xml.parsing.NoBindingFactoryAdapter().loadXML(source, parser)
  }
}

object Test extends App {
  val url = "http://www.ishuhui.com/archives/4031"

  Utils.timeit("download", 1) {
    val imgs = Helper.loadHtmlFromUrl(url) \\ "img" \\ "@src" map (_.toString)
    imgs foreach println
  }
}
