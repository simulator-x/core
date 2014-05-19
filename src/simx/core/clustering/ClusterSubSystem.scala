/*
 * Copyright 2013 The SIRIS Project
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

package simx.core.clustering

import com.typesafe.config.ConfigFactory
import akka.actor.{ActorRef, Props, Actor}
import akka.pattern.ask
import akka.util.Timeout
import concurrent.duration._
import simx.core.svaractor.SVarActor
import simx.core.component.{ComponentManagementActorInCluster, ComponentManagementActor}
import scala.concurrent.Await
import simx.core.helper.JVMTools

/**
 * This message is sent to an instance of [[simx.core.clustering.ClusterSubSystem]] to start to connection process.
 *
 * The message should be sent by using the ?-Operator to obtain a Future. After all nodes in the cluster are connected
 * to each other a list of all nodes and their addresses is returned trough the future. The future contains a map from
 * symbol to a tuple of String and Int. The Symbol is the name of the node. The String in the tuple is the IP-Adress of
 * the target, while the Int the the port.
 *
 * {{{
     val subSystemActor = system.actorOf( Props[ClusterSubSystem], "clusterSubSystem" )
     val result = subSystemActor ? StartConnection( nodeName, interface, port, allClusterNodes, seedNode )
      known = Some( Await.result( result, Duration.Inf ).asInstanceOf[Map[Symbol,(String,Int)]] )
 * }}}
 *
 * @author Stephan Rehfeld
 *
 * @param nodeName The name of this node. Must not be null!
 * @param interface The interface where this node listens for incoming connections. Must not be null!
 * @param port The port where this node listens for incoming connections. Must not be null!
 * @param allClusterNodes A set that contains the name of all nodes in the cluster. Must not be null. Must contain at least 2 entries.
 * @param seedNode An optional seed node where this node connects to. The format for the parameter is "interface:port", e.g. "127.0.0.1:9001". Must not be null!
 */
case class StartConnection( nodeName: Symbol, interface: String, port : Int, allClusterNodes : Set[Symbol], seedNode : Option[String] ) {
  require( nodeName != null, "The parameter 'nodeName' must not be null!" )
  require( interface != null, "The parameter 'interface' must not be null!" )
  require( port > 0 && port < 65536, "The parameter 'port' must contain a valid port!" )
  require( allClusterNodes != null, "The parameter 'allClusterNodes' must not be null!" )
  require( allClusterNodes.size > 1, "The parameter 'allClusterNodes' must contain at least 2 nodes!" )
  require( seedNode != null, "The parameter 'seedNode' must not be null!" )
}

/**
 * The [[simx.core.clustering.ClusterSubSystem]] actors of different nodes send Hi-Messages to each other to announce
 * them self and to publish known nodes the new nodes that has joined the cluster.
 *
 * @author Stephan Rehfeld
 *
 * @param nodeName The name of this node. Must not be null!
 * @param interface The interface where this node listens for incoming connections. Must not be null!
 * @param port The port where this node listens for incoming connections. Must not be null!
 * @param known A map that contains all nodes that this nodes knows. Must not be null! Can be empty.
 */
case class Hi( nodeName: Symbol, interface: String, port : Int, known : Map[Symbol,(String,Int)] ) {
  require( nodeName != null, "The parameter 'nodeName' must not be null!" )
  require( interface != null, "The parameter 'interface' must not be null!" )
  require( port > 0 && port < 65536, "The parameter 'port' must contain a valid port!" )
  require( known != null, "The parameter 'known' must not be null!" )
}

/**
 * This actor manages the connection process between the nodes in the cluster during the bootstrap process. It connects
 * to the [[simx.core.clustering.ClusterSubSystem]] of other processes. After all nodes are connected to each other
 * this actor sends back a map that contains the information on how to connect to other nodes via a future.
 *
 * {{{
     val subSystemActor = system.actorOf( Props[ClusterSubSystem], "clusterSubSystem" )
     val result = subSystemActor ? StartConnection( nodeName, interface, port, allClusterNodes, seedNode )
      known = Some( Await.result( result, Duration.Inf ).asInstanceOf[Map[Symbol,(String,Int)]] )
 * }}}
 *
 * @author Stephan Rehfeld
 *
 */
class ClusterSubSystem extends Actor {

  /**
   * The actor that sent the [[simx.core.clustering.StartConnection]] message and will reveice the map with all nodes
   * in the lcuster.
   */
  var origin : Option[ActorRef] = None

  /**
   * The map that contains all nodes in the cluster.
   */
  var known = Map[Symbol,(String,Int)]()

  /**
   * The name of this node.
   */
  var myName : Option[Symbol] = None
  var allClusterNodes : Option[Set[Symbol]] = None

  final def receive = {
    case msg : StartConnection =>
      origin = Some( sender )
      known = known + (msg.nodeName -> (msg.interface,msg.port) )
      myName = Some( msg.nodeName )
      if( msg.seedNode.isDefined ) {
        val seedNodeClusterSubSystem = context.actorSelection( "akka://"+SVarActor.getSystemName+"@" + msg.seedNode.get + "/user/clusterSubSystem" )
        seedNodeClusterSubSystem ! Hi( msg.nodeName, msg.interface, msg.port, known )
      }
      allClusterNodes = Some( msg.allClusterNodes )

    case msg : Hi =>
      if( !known.contains( msg.nodeName ) ) {
        sender ! Hi( myName.get, known( myName.get )._1, known( myName.get )._2, known )
        val a = context.actorSelection( "akka://"+SVarActor.getSystemName+"@" + msg.interface + ":" + msg.port + "/user/componentManagementActor")
        ComponentManagementActor ! ComponentManagementActorInCluster( a )
        known = known + (msg.nodeName -> (msg.interface,msg.port) )
      }
      for( (nodeName,connectionData) <- msg.known ) {
        if( !known.contains( nodeName ) ) {
          val targetClusterSubSystem = context.actorSelection( "akka://"+SVarActor.getSystemName+"@" + connectionData._1 + ":" + connectionData._2 + "/user/clusterSubSystem")
          targetClusterSubSystem ! Hi( myName.get, known( myName.get )._1, known( myName.get )._2, known )
          known = known + (nodeName -> (connectionData._1, connectionData._2) )
        }
      }

      if( known.keySet == allClusterNodes.get ) {
        origin.get ! (known - myName.get)
      }


  }
}

/**
 * The companion object that manages the connection process. It provides one message to start the connection process.
 *
 * @author Stephan Rehfeld
 */
object ClusterSubSystem {

  /**
   * A map that contains all other nodes of the cluster.
   */
  private var known : Option[Map[Symbol,(String,Int)]] = None

  /**
   * This method returns a map that contains all known nodes in the cluster and information about how to connect to
   * actors on this node.
   *
   * @return A map that contains information about how to connecnt to actors on other nodes. Never returns null.
   */
  def getKnown = known.get

  def configureSubSystem( nodeName: Symbol, interface: String, port : Int, allClusterNodes : Set[Symbol], seedNode : Option[String] ) {

    require( nodeName != null, "The parameter 'nodeName' must not be null!" )
    require( interface != null, "The parameter 'interface' must not be null!" )
    require( port > 0 && port < 65536, "The parameter 'port' must contain a valid port!" )
    require( allClusterNodes != null, "The parameter 'allClusterNodes' must not be null!" )
    require( allClusterNodes.size > 1, "The parameter 'allClusterNodes' must contain at least 2 nodes!" )
    require( seedNode != null, "The parameter 'seedNode' must not be null!" )

    val config = ConfigFactory.parseString("""
      akka {
        scheduler {
          tick-duration=""" + JVMTools.minTickDuration + """
        }
        actor {
          provider = "akka.remote.RemoteActorRefProvider"
        }
        remote {
          transport = "akka.remote.netty.NettyRemoteTransport"
          netty {
            hostname = """" + interface + """"
            port = """ + port + """
          }
        }
      }
    """).withFallback(ConfigFactory.load())

    SVarActor.setSystemConfig(config)
    val subSystemActor = SVarActor.createActor( Props[ClusterSubSystem], Some("clusterSubSystem") )
    ComponentManagementActor.self

    implicit val timeout = Timeout( 500 seconds )
    val result = subSystemActor ? StartConnection( nodeName, interface, port, allClusterNodes, seedNode )
    known = Some( Await.result( result, Duration.Inf ).asInstanceOf[Map[Symbol,(String,Int)]] )

  }
}

