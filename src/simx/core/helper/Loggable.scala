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

package simx.core.helper

import org.slf4j.LoggerFactory


trait Loggable {
 private val log = LoggerFactory.getLogger(getClass)

 def trace( message:String, values:Any* ) { log.trace( message, values.map( _.asInstanceOf[Object]).toArray ) }
 def trace( message:String, error:Throwable ) { log.trace( message, error ) }
 def trace( message:String, error:Throwable, values:Any* ) { log.trace(message + " with " + values.mkString(", "), error ) }

 def debug( message:String, values:Any* ) { log.debug( message, values.map( _.asInstanceOf[Object] ).toArray ) }
 def debug( message:String, error:Throwable ) { log.debug (message, error ) }
 def debug( message:String, error:Throwable, values:Any* ) { log.debug( message + " with " + values.mkString(", "), error ) }

 def info( message:String, values:Any* ) { log.info( message, values.map( _.asInstanceOf[Object] ).toArray ) }
 def info( message:String, error:Throwable ) { log.info( message, error ) }
 def info( message:String, error:Throwable, values:Any* ) { log.info( message + " with " + values.mkString(", "), error)  }

 def warn( message:String, values:Any* ) { log.warn( message, values.map( _.asInstanceOf[Object] ).toArray ) }
 def warn( message:String, error:Throwable ) { log.warn( message, error ) }
 def warn( message:String, error:Throwable, values:Any* ) { log.warn( message + " with " + values.mkString(", "), error ) }

 def error( message:String, values:Any* ) { log.error( message, values.map( _.asInstanceOf[Object] ).toArray ) }
 def error( message:String, error:Throwable ) { log.error( message, error ) }
 def error( message:String, error:Throwable, values:Any* ) { log.error( message + " with " + values.mkString(", "), error ) }
}
