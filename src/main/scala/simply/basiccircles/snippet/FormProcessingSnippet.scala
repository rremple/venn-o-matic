package simply.basiccircles.snippet

import simply.basiccircles.model._
import net.liftweb._
import http._
import common._
import util.Helpers._

class FormProcessingSnippet extends StatefulSnippet {
  private var aAreaString, bAreaString, cAreaString = "314"
  private var abAreaString, bcAreaString, caAreaString = "100"

  private var aArea, bArea, cArea : Double = 0
  private var abArea, bcArea, caArea : Double = 0
  
  private var diagram : Option[Diagram] = None 
  
  private val whence = S.referer openOr "/"

  def dispatch = {
    case "render" => renderForm & processForm & renderResults
  }
  
  def renderForm = {
    "name=a" #> SHtml.text(aAreaString, aAreaString = _) &
    "name=b" #> SHtml.text(bAreaString, bAreaString = _) &
    "name=c" #> SHtml.text(cAreaString, cAreaString = _) &
    "name=ab" #> SHtml.text(abAreaString, abAreaString = _) &
    "name=bc" #> SHtml.text(bcAreaString, bcAreaString = _) &
    "name=ca" #> SHtml.text(caAreaString, caAreaString = _)
  }

  def processForm = "type=submit" #> SHtml.onSubmitUnit(process)
  
  def renderResults = diagram match {
    case Some(d) => "#intersectionABC" #> d.toSvgIntersectionABC &
      "#svgelem [viewBox]" #> d.toSvgViewBoxAttribute &
      "#grid *+" #> d.toSvgGrid &
      "#axes *+" #> d.toSvgAxes &
      "#circles *+" #> d.toSvgCircles &
      "#centers *+" #> d.toSvgCenters &
      "#chords *+" #> d.toSvgChords
      
    case _ => "#diag" #> "" // remove the diagram
  }
      
  // process the form
  private def process() = {
    asDouble(aAreaString) match {
      case Full(value) if value < 0 => S.error("a", " can't be negative!")
      case Full(value) => {
          aArea = value
        }
      case _ => S.error("a", " doesn't parse as a number!")
    }
    asDouble(bAreaString) match {
      case Full(value) if value < 0 => S.error("b", " can't be negative!")
      case Full(value) => {
          bArea = value
        }
      case _ => S.error("b", " doesn't parse as a number!")
    }
    asDouble(cAreaString) match {
      case Full(value) if value < 0 => S.error("c", " can't be negative!")
      case Full(value) => {
          cArea = value
        }
      case _ => S.error("c", " doesn't parse as a number!")
    }

    asDouble(abAreaString) match {
      case Full(value) if value < 0 => S.error("ab", " can't be negative!")
      case Full(value) => {
          abArea = value
        }
      case _ => S.error("ab", " doesn't parse as a number!")
    }
    asDouble(bcAreaString) match {
      case Full(value) if value < 0 => S.error("bc", " can't be negative!")
      case Full(value) => {
          bcArea = value
        }
      case _ => S.error("bc", " doesn't parse as a number!")
    }
    asDouble(caAreaString) match {
      case Full(value) if value < 0 => S.error("ca", " can't be negative!")
      case Full(value) => {
          caArea = value
        }
      case _ => S.error("ca", " doesn't parse as a number!")
    }
    
    if (abArea > math.min(aArea,bArea)) {S.error("ab", " too large!") 
                                         abArea = math.min(aArea,bArea)}
    if (bcArea > math.min(bArea,cArea)) {S.error("bc", " too large!")
                                         bcArea = math.min(bArea,cArea)}
    if (caArea > math.min(cArea,aArea)) {S.error("ca", " too large!")
                                         caArea = math.min(cArea,aArea)}
  
    try {
      diagram = Some(new Diagram(aArea, bArea, cArea, abArea, bcArea, caArea))
    } catch {
      case e: Exception => S.error(e.getMessage)
    }
  }
}