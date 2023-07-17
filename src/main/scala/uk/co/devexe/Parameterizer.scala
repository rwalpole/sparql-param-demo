package uk.co.devexe

import org.apache.jena.query.ParameterizedSparqlString
import org.apache.jena.rdf.model.{Resource, ResourceFactory}

import scala.io.Source
import scala.util.{Success, Try, Using}
import scala.jdk.CollectionConverters._


object Parameterizer {

//  def parameterizeThis(queryText: String, params: (String, List[Resource])): Try[String] = {
//    for {
//      //queryText <- getQueryText(sparqlResource)
//      parameterizedQuery <- Try(parameterizeQuery(queryText))
//    } yield parameterizedQuery
//  }

//  def setValues(queryText: String, values: (String,List[Resource])): Try[String] = {
//    for {
//      //queryText <- getQueryText(queryResource)
//      result <- setValues()Try(setTheValues(queryText, values))
//    } yield result
//  }

//  def setBoolean(queryText: String, values: (String,Boolean)): Try[String] = {
//    for {
//      //queryText <- getQueryText(queryResource)
//      result <- Try(setTheBoolean(queryText, values))
//    } yield result
//
//  }

  def setPropertyVal(queryText: String, varName: String, propertyUri:String): String = {
    val pss = new ParameterizedSparqlString(queryText)
    pss.setIri(varName, propertyUri)
    pss.toString
  }

//  private def setTheValues(queryText: String, values: (String,List[Resource])) = {
//    val pss = new ParameterizedSparqlString(queryText)
//    pss.setValues(values._1,values._2.asJava)
//    pss.toString
//  }

//  def doAppend(queryText: String, appendValue: String) = {
//    val pss = new ParameterizedSparqlString(queryText)
//    pss.append(appendValue)
//    pss.toString
//  }

  def setBooleanProperty(queryText: String, propertyUri: String, bool: Boolean): String = {
    val pss = new ParameterizedSparqlString(queryText)
    val result = for {
      sparql1 <- setVarToUri(pss, "p1", propertyUri)
      sparql2 <- setVarToBoolean(sparql1, "o1", bool)
    }  yield sparql2
    result match {
      case Success(query) => query.toString
      case _ => ""
    }
  }

  def setObjectValue(queryText: String, varName: String, resource: Resource): String = {
    val pss = new ParameterizedSparqlString(queryText)
    setVarToUri(pss,varName, resource.getURI) match {
      case Success(query) => query.toString
      case _ => ""
    }
  }

  def setObjectProperty(queryText: String, propertyUri: String, resource: Resource): String = {
    val sparql0 = new ParameterizedSparqlString(queryText)
    val result = for {
      sparql1 <- setVarToUri(sparql0,"p2", propertyUri)
      sparql2 <- setVarToUri(sparql1,"o2",resource.getURI)
    } yield sparql2
    result match {
      case Success(query) => query.toString
      case _ => ""
    }
  }

  def setVarToUri(queryString: ParameterizedSparqlString, varName: String, resourceUri: String): Try[ParameterizedSparqlString] = {
    Try {
      queryString.setIri(varName, resourceUri)
      queryString
    }
  }

  def setVarToBoolean(queryString: ParameterizedSparqlString, varName: String, bool: Boolean): Try[ParameterizedSparqlString] = {
    Try {
      queryString.setLiteral(varName, bool)
      queryString
    }
  }

  def setValues(queryText: ParameterizedSparqlString, values: (String, List[Resource])): Try[ParameterizedSparqlString] = {
    Try {
      queryText.setValues(values._1, values._2.asJava)
      queryText
    }
  }


//  private def parameterizeQuery(queryText: String) = {
//    val pss = new ParameterizedSparqlString(queryText)
//    pss.setValues("agentTypeValues", List(ResourceFactory.createResource("http://cat.nationalarchives.gov.uk/corporate-body-concept"),
//      ResourceFactory.createResource("http://cat.nationalarchives.gov.uk/person-concept")).asJava)
//    pss.toString
//  }



}

