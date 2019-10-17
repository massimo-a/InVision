package raytracing.server

import java.io.{InputStream, OutputStream}
import java.net.{ServerSocket,Socket}
import java.security.MessageDigest;
import java.util.{Base64, Scanner, regex},regex.Matcher,regex.Pattern

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
	private def decode(bytes: Array[Byte]): String = {
		if(bytes.length > 0) {
			var dataLength = bytes(1) & 127
			var indexFirstMask = {
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
		} else return ""
    }
	private def handshake(in: InputStream, out: OutputStream): Unit = {
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
		}
	}
	def getMessage(in: InputStream): Unit = {
		var message = ""
		while(message != "quit") {
			message = decode(read(in))
			if(message != "") {
				System.out.println("RECEIVED : " + message)
			}
		}
	}
	def main(args: Array[String]): Unit = {
		val server = new ServerSocket(PORT)
		System.out.println("Server has started on " + HOST + ":" + PORT)
		val client = server.accept()
		val in = client.getInputStream()
		val out = client.getOutputStream()
		System.out.println("A client connected.")
		handshake(in, out);
		getMessage(in)
	}
}