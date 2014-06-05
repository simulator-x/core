/*
 * Copyright 2012 The SIRIS Project
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * The SIRIS Project is a cooperation between Beuth University, Berlin and the
 * HCI Group at the University of Würzburg. The project is funded by the German
 * Federal Ministry of Education and Research (grant no. 17N4409).
 */

package simx.core.dynamics

import simx.core.ontology.GroundedSymbol
import simx.core.entity.Entity


/**
 * Created by IntelliJ IDEA.
 * User: dwiebusch
 * Date: 18.02.12
 * Time: 17:10
 */

object Method{
  private var registeredMethods = Set[Method]()

  def apply(m : Method){
    registeredMethods = registeredMethods + m
  }

  def getSupportedMethods(e : Entity) : Set[Method] =
    registeredMethods.filter( _ supportedBy e )
}

trait Method {
  protected var _target : Option[Entity] = None
  protected def init ( entity : Entity ) {}
  def supportedBy    ( entity : Entity ) : Boolean
  def apply          ( target : Entity )
  def getInputs                         = inputs.map(_ bind _target.get)
  def getEffects                        = effects.map(_ bind _target.get)
  protected def effects                  : Set[Effect[_]]
  protected def inputs                   : Set[Prerequisite[_]]
  def name                               : GroundedSymbol
  private var done = false

  def isDone =
    done

  final def bind( target  : Entity ) : this.type = {
    _target = Some(target)
    init(target)
    this
  }

  final def isBound =
    _target.isDefined

  final def execute( andThen : () => Unit = () => {} ){
    _target match {
      case None => throw new Exception("Cannot execute unbound method " + name.value)
      case Some(target) =>
        apply(target)
        done = true
        andThen()
    }
  }

  Method(this)
}

