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

package simx.core

import clustering.ClusterSubSystem


/**
 * This trait represents an easy foundation for clustered applications in Simulator X.
 *
 * @author Stephan Rehfeld
 *
 * @param bootstrapApp Pass 'true' if this instance should run the bootstrap code. Typically the return value of
 *                     [[simx.core.SimXClusterApplication.startClusterSubsystem]].
 */
abstract class SimXClusterApplication( bootstrapApp : Boolean ) extends SimXApplication {

  override def startUp() {
    if( bootstrapApp ) {
      println( "Running bootstrap code for the application" )
      createComponents()
    } else {
      println( "Waiting for other node to run the bootstrap code" )
    }
  }
}


/**
 * The companion object offers a function to bootstrap the cluster It interprets several command line arguments to
 * configure to cluster subsystem.
 *
 * @author Stephan Rehfeld
 */
object SimXClusterApplication {

  /**
   * This method boots the cluster. It blocks until all nodes are connected to each other. It interprets the following
   * command line argument:
   *   - --no-cluster               This parameter disables to cluster subsystem.  All actors are creates on the local
   *                                node.
   *   - --name <name>              The name of this node.
   *   - --bootstrap-app            This node executes to bootstrap code the the application. Only one node in the
   *                                cluster should have this parameter set.
   *   - --seed-node <address:port> The address and port of another node in  cluster. This parameter is optional.
   *                                Typically one node is the seed node for all other nodes in the cluster.
   *   - --interface <port>         The interface where this node listens for connections
   *   - --port <port>              The port on which the remote actor service of this node is running.
   *
   *
   * @param args The command line arguments. Must not be null.
   * @param clusterNodes The list of all cluster nodes. Mut not be null. Must not be empty.
   * @return Returns true if this node should run the application bootstrap code.
   */
  def startClusterSubsystem( args : Array[String], clusterNodes : Set[Symbol] ) : Boolean = {
    require( args != null, "The parameter 'args' must not be 'null'!" )
    require( clusterNodes != null, "The parameter 'clusterNodes' must not be 'null'!" )
    require( !clusterNodes.isEmpty, "The parameter 'clusterNodes' must not contain an empty set!" )

    if( !args.contains( "--no-cluster" ) && (!args.contains( "--name" ) || !args.contains( "--port" ) || !args.contains( "--interface" ) ) ) {
      println( "------------------------- SimulatorX: Clust subsystem --------------------------" )
      println( "| One or more command lime parameter are missing.                              |" )
      println( "|                                                                              |" )
      println( "| Usage:                                                                       |" )
      println( "|   --no-cluster               This parameter disables to cluster subsystem.   |" )
      println( "|                              All actors are creates on the local node.       |" )
      println( "|                              Useful for debugging and development.           |" )
      println( "|   --name <name>              The name of this node. Must be one of           |" )
      println( "|                              " + clusterNodes + " |" )
      println( "|   --bootstrap-app            This node executes to bootstrap code the the    |" )
      println( "|                              application. Only one node in the cluster       |" )
      println( "|                              should have this parameter set.                 |" )
      println( "|   --seed-node <address:port> The address and port of another node in         |" )
      println( "|                              cluster. This parameter is optional. Typically  |" )
      println( "|                              one node is the seed node for all other nodes   |" )
      println( "|                              in the cluster.                                 |" )
      println( "|   --interface <port>         The interface where this node listens for       |" )
      println( "|                              connections                                     |" )
      println( "|   --port <port>              The port on which the remote actor service of   |" )
      println( "|                              this node is running.                           |" )
      println( "--------------------------------------------------------------------------------" )
      System.exit( -42 )
    }

    val noCluster = args.foldLeft( false )( (b,a) => b || (a == "--no-cluster") )
    val bootstrapApp = args.foldLeft( false )( (b,a) => b || (a == "--bootstrap-app") ) | args.contains( "--no-cluster" )
    val nodeName = Symbol( args( args.indexWhere( (s ) => s == "--name" ) + 1 ) )
    val seedNode = if( args.contains( "--seed-node" ) ) Some( args( args.indexWhere( (s ) => s == "--seed-node" ) + 1 ) ) else None
    val interface = args( args.indexWhere( (s ) => s == "--interface" ) + 1 )
    val port = if( args.contains( "--port" ) ) args( args.indexWhere( (s ) => s == "--port" ) + 1 ).toInt else 0

    if( !noCluster ) ClusterSubSystem.configureSubSystem( nodeName, interface, port, clusterNodes, seedNode )

    bootstrapApp
  }
}