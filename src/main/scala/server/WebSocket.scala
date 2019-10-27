/*
** Author:  Massimo Angelillo
*/

package raytracing.server

import java.io.{InputStream, OutputStream}
import java.net.{ServerSocket,Socket}
import java.io.{InputStream, OutputStream}
import java.security.MessageDigest;
import java.util.{Base64, Scanner, regex},regex.Matcher,regex.Pattern

trait Message
case class Command(command: String) extends Message
case class Response(response: String) extends Message
case object NoMessage extends Message
case object Quit extends Message
case object Render extends Message

object WebSocket {
	private val PORT = 80
	private val HOST = "127.0.0.1"
	
	private def read(is: InputStream): Array[Byte] = {
		var arr = new Array[Byte](is.available())
		for(i <- 0 until arr.length) {
			arr(i) = is.read().asInstanceOf[Byte]
		}
		return arr
	}
	
	def decode(bytes: Array[Byte]): String = {
		if(bytes.length <= 0) return ""
		val dataLength = bytes(1) & 127
		val indexFirstMask = {
			if (dataLength == 126) {
				4
			} else if (dataLength == 127) {
				10
			} else {
				2
			}
		}

		var keys = bytes.drop(indexFirstMask).take(4)
		var indexFirstDataByte = indexFirstMask + 4

		var decoded = new Array[Byte](bytes.length - indexFirstDataByte)
		var j = 0
		for(i <- indexFirstDataByte until bytes.length) {
			decoded(j) = (bytes(i) ^ keys(j % 4)).asInstanceOf[Byte]
			j += 1
		}
		return new String(decoded, "UTF-8")
    }
	
	def handshake(in: InputStream, out: OutputStream): Boolean = {
		val s = new Scanner(in, "UTF-8");
		val data = s.useDelimiter("\\r\\n\\r\\n").next()
		val get = Pattern.compile("^GET").matcher(data)
		if (get.find()) {
			val m = Pattern.compile("Sec-WebSocket-Key: (.*)").matcher(data);
			m.find();
			val response = ("HTTP/1.1 101 Switching Protocols\r\n"
				+ "Connection: Upgrade\r\n"
				+ "Upgrade: websocket\r\n"
				+ "Sec-WebSocket-Accept: "
				+ Base64.getEncoder().encodeToString(MessageDigest.getInstance("SHA-1").digest((m.group(1)
				+ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").getBytes("UTF-8")))
				+ "\r\n\r\n").getBytes("UTF-8")
			out.write(response, 0, response.length)
			return true
		} else {
			return false
		}
	}
	
	def getMessage(in: InputStream): Message = {
		decode(read(in)) match {
			case "quit" => Quit
			case "" => NoMessage
			case w: String => Command(w)
		}
	}
	
	def printMsg(c: Message): Unit = {
		c match {
			case Command(str) => println(str)
			case NoMessage => println("No message was sent")
			case Quit => println("Goodbye!")
			case Render => println("Begun rendering!")
		}
	}
	
	def open(host: String, port: Int): (InputStream, OutputStream) = {
		val server = new ServerSocket(port)
		System.out.println("Server has started on " + host + ":" + port)
		val client = server.accept()
		return (client.getInputStream(), client.getOutputStream())
	}
	
	def loop(in: InputStream, out: OutputStream): Unit = {
		var a = getMessage(in)
		while(a != Quit) {
			while(a == NoMessage) {
				a = getMessage(in)
			}
			printMsg(a)
			a = if(a == Quit) a else NoMessage
		}
	}
	
	def run(host: String, port: Int): Unit = {
		val (in, out) = open(host, port)
		val connSuccess = handshake(in, out);
		if(connSuccess) {
			System.out.println("A client connected.")
			loop(in, out)
		}
	}
}