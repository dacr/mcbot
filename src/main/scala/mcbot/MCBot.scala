/*
 * Copyright 2012 David Crosson
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package mcbot

import org.spacehq.mc.auth.GameProfile
import org.spacehq.mc.auth.exception.AuthenticationException
import org.spacehq.mc.protocol._
import org.spacehq.mc.protocol.data.game.values.entity.player.GameMode
import org.spacehq.mc.protocol.data.game.values.setting.Difficulty
import org.spacehq.mc.protocol.data.game.values.world.WorldType
import org.spacehq.mc.protocol.data.status._
import org.spacehq.mc.protocol.data.status.handler._
import org.spacehq.mc.protocol.data.message._
import org.spacehq.mc.protocol.packet.ingame.client.ClientChatPacket
import org.spacehq.mc.protocol.packet.ingame.server._
import org.spacehq.packetlib._
import org.spacehq.packetlib.event.server._
import org.spacehq.packetlib.event.session._
import org.spacehq.packetlib.tcp.TcpSessionFactory
import org.spacehq.packetlib.packet.Packet;

import java.net.Proxy
import scala.collection.JavaConversions._


case class MCBotConfig(username:String, password:String, port:Int, host:String="localhost")

object MCBotConfig {
  val parser = new scopt.OptionParser[MCBotConfig]("mcbot") {
    head("mcbot", "0.0.1")
    opt[String]('u',"username")  required() action{(v,c)=>c.copy(username=v)}  text("username to connect with")
    opt[String]('w',"password")  required() action{(v,c)=>c.copy(password=v)}  text("username password")
    opt[Int]('p',"port")         required() action{(v,c)=>c.copy(port=v)}  text("port of the minecraft server")
    opt[String]('h',"host")                 action{(v,c)=>c.copy(host=v)}  text("ip/dns of the minecraft server")
  }
}


object MCBot {
  val VERIFY_USERS = true;
  val PROXY = Proxy.NO_PROXY;

  def main(args: Array[String]) {
    val defaultConfig = MCBotConfig(username = "", password = "", port = 0)
    MCBotConfig.parser.parse(args, defaultConfig) match {
      case Some(config) =>
        status(config)
        login(config)
      case None =>
        println("Some issue with the config")
    }

  }

  def status(config:MCBotConfig) {
    val protocol = new MinecraftProtocol(ProtocolMode.STATUS)
    val client = new Client(config.host, config.port, protocol, new TcpSessionFactory(PROXY))
    client.getSession().setFlag(ProtocolConstants.SERVER_INFO_HANDLER_KEY, new ServerInfoHandler() {
      override def handle(session: Session, info: ServerStatusInfo) {
        println("Version: " + info.getVersionInfo().getVersionName() + ", " + info.getVersionInfo().getProtocolVersion())
        println("Player Count: " + info.getPlayerInfo().getOnlinePlayers() + " / " + info.getPlayerInfo().getMaxPlayers())
        println("Players: " + info.getPlayerInfo().getPlayers().mkString(", "))
        println("Description: " + info.getDescription().getFullText())
        println("Icon: " + info.getIcon())
      }
    })

    client.getSession().setFlag(ProtocolConstants.SERVER_PING_TIME_HANDLER_KEY, new ServerPingTimeHandler() {
      override def handle(session: Session, pingTime: Long) { println("Server ping took " + pingTime + "ms") }
    });

    client.getSession().connect();
    while (client.getSession().isConnected()) {
      try {
        Thread.sleep(5);
      } catch {
        case ex: InterruptedException => ex.printStackTrace();
      }
    }
  }

  def login(config:MCBotConfig) {
    val protocol = if (VERIFY_USERS) {
      val prot = new MinecraftProtocol(config.username, config.password, false)
      System.out.println("Successfully authenticated user.");
      prot
    } else new MinecraftProtocol(config.username);

    val client = new Client(config.host, config.port, protocol, new TcpSessionFactory(PROXY));
    client.getSession().addListener(new SessionAdapter() {

      def processPacket[T <: Packet](packet: T, session: Session) {
        packet match {
          case null =>
          case e: ServerJoinGamePacket =>
            session.send(new ClientChatPacket("Hello, this is a test of MCProtocolLib."))
          case sc: ServerChatPacket =>
            val message = sc.getMessage() // Why asInstanceOf is needed here ??
            System.out.println("Received Message: " + message.getFullText());
            message match {
              case m: TranslationMessage =>
                println("Received Translation Components: " + m.getTranslationParams().mkString(", "));
              case _ =>
            }
          //event.getSession().disconnect("Finished");
          case p:org.spacehq.mc.protocol.packet.ingame.server.entity.ServerEntityMetadataPacket =>
          case p:org.spacehq.mc.protocol.packet.ingame.server.entity.ServerEntityPropertiesPacket =>
          case p:org.spacehq.mc.protocol.packet.ingame.server.entity.spawn.ServerSpawnMobPacket =>
            
          case p:org.spacehq.mc.protocol.packet.ingame.server.world.ServerMultiChunkDataPacket =>
            val cols = p.getColumns
            println(s"CHUNK : cols=$cols")
            for{
              col <- 0 to cols-1
              chunks = p.getChunks(col)
              x = p.getX(col)
              z = p.getZ(col)
              chunk <- chunks if chunk != null
              } {
                // Chunks store the terrain and entities within a 16×256×16 area.
              val blocks = chunk.getBlocks
              val data:Array[Short] = blocks.getData // Always 4096 blocks
              
            }
          case p:org.spacehq.mc.protocol.packet.ingame.server.world.ServerSpawnPositionPacket =>
            val x = p.getPosition.getX
            val y =  p.getPosition.getY
            val z =  p.getPosition.getZ
            println(s"POS : x=$x y=$y z=$z")
          case p:org.spacehq.mc.protocol.packet.ingame.server.world.ServerWorldBorderPacket =>
            val xc = p.getCenterX
            val yc = p.getCenterY
            val r =  p.getRadius
            val s = p.getSpeed
            println(s"BORDER : xc=$xc yc=$yc radius=$r speed=$s")
          case p:org.spacehq.mc.protocol.packet.ingame.server.world.ServerUpdateTimePacket =>
          case p:org.spacehq.mc.protocol.packet.ingame.server.world.ServerUpdateTileEntityPacket =>
          case p:org.spacehq.mc.protocol.packet.ingame.server.world.ServerBlockChangePacket =>
          case x =>
            if (x.getClass.getName contains "world")
               println(x.getClass.getName)
        }
      }

      override def packetReceived(event: PacketReceivedEvent) {
        processPacket(event.getPacket(), event.getSession())
      }

      override def disconnected(event: DisconnectedEvent) {
        System.out.println("Disconnected: " + Message.fromString(event.getReason()).getFullText());
      }
    });

    client.getSession().connect();
  }
}
