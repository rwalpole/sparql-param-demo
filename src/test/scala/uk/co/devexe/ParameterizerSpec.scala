package uk.co.devexe

import org.apache.jena.query.ParameterizedSparqlString
import org.apache.jena.rdf.model.ResourceFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import uk.co.devexe.Parameterizer.getClass

import scala.io.Source
import scala.util.{Success, Try, Using}

class ParameterizerSpec extends AnyFreeSpec with Matchers {

  private val sparqlResourceDir = "sparql"
  private val getAgentSummariesResource = s"/$sparqlResourceDir/get-agent-summaries.rq"
  private val getAgentDescriptionsResource = s"/$sparqlResourceDir/get-agent-descriptions.rq"


  "Set" - {
    "values" in {
      val params = ("agentTypeValues", List(createCatResource("corporate-body-concept"),createCatResource("person-concept")))
      val result = for {
        queryText <- getQueryText(getAgentSummariesResource)
        withValues <- Parameterizer.setValues(queryText,params)
      } yield withValues
      result mustBe Success("")
    }
    "boolean" in {
      val valueParams = ("agentTypeValues", List(createCatResource("corporate-body-concept"), createCatResource("person-concept")))
      val result = for {
        queryText <- getQueryText(getAgentSummariesResource)
        withValues <- Parameterizer.setValues(queryText,valueParams)
        withProperty <- Parameterizer.setVarToUri(withValues, "p1", s"${Prefix.todo}/is-place-of-deposit")
        withBoolean <- Parameterizer.setVarToBoolean(withProperty, "o1", true)
        withProperty2 <- Parameterizer.setVarToUri(withBoolean, "p2", s"${Prefix.dct}/type")
        withBoolean2 <- Parameterizer.setVarToUri(withProperty2, "o2" , createCatResource("authority-file").getURI)
      } yield withBoolean2
      result mustBe Success("")
    }
    "dsad" in {
      val result = for {
        queryText <- getQueryText(getAgentDescriptionsResource)
        withObject <- Parameterizer.setVarToUri(queryText, "conceptId", createCatResource("agent.S7").getURI)
        withProperty1 <- Parameterizer.setVarToUri(withObject, "dateFromProperty", s"${Prefix.rdaa}/P50037")
        withProperty2 <- Parameterizer.setVarToUri(withProperty1, "dateToProperty", s"${Prefix.rdaa}/P50038")

      } yield withProperty2
      result mustBe Success("")
    }
  }

  private def createCatResource(name: String) = {
    ResourceFactory.createResource(s"${Prefix.cat}/$name")
  }

  private def getQueryText(queryResource: String): Try[ParameterizedSparqlString] =
    Using(Source.fromInputStream(getClass.getResourceAsStream(queryResource))) { resource =>
      new ParameterizedSparqlString(resource.getLines().mkString("\n"))
    }
}
