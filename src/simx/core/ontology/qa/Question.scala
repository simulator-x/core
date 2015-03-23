package simx.core.ontology.qa

import simx.core.ontology.SValDescription
import simx.core.svaractor.SVarActor
import simx.core.svaractor.semantictrait.base.{Thing, Base}

/**
 * User: dwiebusch
 * Date: 31.03.11
 * Time: 15:19
 */

/**
 * wrapper for questions (to provide autocompletion)
 */
object Question{
  def isSubclassOf(o : SValDescription[_, _,_ <: Base,_ <: Thing]) =
    new Question("isSubclassOf", Some(o))
}

class Question( s : String, e : Option[SValDescription[_, _,_ <: Base,_ <: Thing]] = None ){
  def ask() =
    KnowledgeBase ask this

  def ask( sender : SVarActor.Ref ){
    KnowledgeBase.ask(this, sender)
  }
}