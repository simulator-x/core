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

package simx.core.ontology.entities

import simx.core.entity.Entity

class Arm( e : Entity = new Entity ) extends Entity(e) 

class Barrel( e : Entity = new Entity ) extends Entity(e) 

class Ghost( e : Entity = new Entity ) extends Entity(e) 

class Hand( e : Entity = new Entity ) extends Entity(e) 
class Head( e : Entity = new Entity ) extends Entity(e) 

class Sound( e : Entity = new Entity ) extends Entity(e) 

class User( e : Entity = new Entity ) extends Entity(e) 

class Well( e : Entity = new Entity ) extends Entity(e) {
  println("new Well")
}