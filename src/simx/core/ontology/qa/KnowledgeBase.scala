package simx.core.ontology.qa

import simx.core.svaractor.SVarActor

/**
 * User: dwiebusch
 * Date: 31.03.11
 * Time: 15:33
 */

object KnowledgeBase extends OWLInterface{
  def ask( q : Question ) : Answer =
    new Answer(q, Set())

  def ask( q : Question, sender : SVarActor.Ref ) {
    sender ! AnswerMsg(new Answer(q, Set()))
  }
}