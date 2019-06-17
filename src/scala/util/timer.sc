package raytracing.util;
import scala.math.{floor,ceil};

class Timer() {
	private var starttime: Long = 0;
	private var endtime: Long = 0;
	
	def start() {
		starttime = System.currentTimeMillis;
	}
	def end() {
		endtime = System.currentTimeMillis;
	}
	def reset() {
		starttime = 0;
		endtime = 0;
	}
	def getTimeSinceStart(): Long = {
		return System.currentTimeMillis - starttime;
	}
	def formatTimeSinceStart(): String = {
		return formatTime(getTimeSinceStart);
	}
	def getRunTime(): Long = {
		return (endtime - starttime)/1000.0.toLong;
	}
	def formatTime(): String = {
		return formatTime(endtime - starttime);
	}
	def formatTime(s: Long): String = {
		val hr = floor(s/(60*60*1000)).toInt;
		val min = (floor(s/(1000*60)).toInt)%60;
		val sec = (s/1000)%60;
		val msec = s.toInt%1000
		val stringHour = if(hr >= 10) hr.toString else "0" + hr.toString;
		val stringMin = if(min >= 10) min.toString else "0" + min.toString;
		val stringSec = if(sec >= 10) sec.toString else "0" + sec.toString;
		val stringMilli = if(msec >= 100) msec.toString else if(msec >= 10) "0" + msec.toString else "00" + msec.toString;
		return stringHour + " : " + stringMin + " : " +  stringSec + "." + stringMilli;
	}
	def formatTime(s: Double): String = {
		return formatTime(s.toLong);
	}
}