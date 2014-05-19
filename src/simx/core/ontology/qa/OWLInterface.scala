package simx.core.ontology.qa

import simx.core.svaractor.SVarActor


/**
 * User: dwiebusch
 * Date: 31.03.11
 * Time: 15:26
 */


//FIXME: to be implemented
trait OWLInterface{
  /**
   * generates an answer to the asked question
   */
  def ask( q : Question ) : Answer

  /**
   * generates an answer to the asked question and sends it to the asking actor (wrapped in an AnswerMsg)
   */
  def ask( q : Question, sender : SVarActor.Ref )
}

/**
 * wrapper class for answers
 */
case class AnswerMsg( answer : Answer )