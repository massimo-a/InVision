/*
** Author:  Massimo Angelillo
**
** A utility class to handle displaying time
** and calculating program runtime
** NOTE: this is a class with mutable states and side effects
*/

package invision.util

import scala.math.{floor,ceil}

class Timer() {
	private var starttime: Long = 0
	private var endtime: Long = 0

	def start() {
		starttime = System.currentTimeMillis
	}
	def end() {
		endtime = System.currentTimeMillis
	}
	def reset() {
		starttime = 0
		endtime = 0
	}
	
	def getRunTime: Long = {
		(endtime - starttime)/1000.0.toLong
	}
	def getTimeSinceStart: Long = {
		System.currentTimeMillis - starttime
	}
	def getTimeSinceEnd: Long = {
		System.currentTimeMillis - endtime
	}
	
	def formatTimeSinceStart(): String = {
		formatTime(getTimeSinceStart)
	}
	def formatTimeSinceEnd(): String = {
		formatTime(getTimeSinceEnd)
	}
	def formatRunTime(): String = {
		formatTime(endtime - starttime)
	}
	
	def formatTime(s: Long): String = {
		val hr = floor(s/(60*60*1000)).toInt
		val min = floor(s/(1000*60)).toInt %60
		val sec = (s/1000)%60
		val msec = s.toInt%1000
		val stringHour = if(hr >= 10) hr.toString else "0" + hr.toString
		val stringMin = if(min >= 10) min.toString else "0" + min.toString
		val stringSec = if(sec >= 10) sec.toString else "0" + sec.toString
		val stringMilli = if(msec >= 100) msec.toString else if(msec >= 10) "0" + msec.toString else "00" + msec.toString
		stringHour + " : " + stringMin + " : " +  stringSec + "." + stringMilli
	}
	def formatTime(s: Double): String = {
		formatTime(s.toLong)
	}
}