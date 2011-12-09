object p19 {
	def daysinmonth(month:Int, year:Int):Int = {
		if(month!=2)
			30+(if(month>7) (month+1)%2 else (month)%2)
		else {
			if(year%4==0 && year%400!=0) 29	else 28
		}
	}
	
	def dayname(days:Int):String = days%7 match {
	  case 0 => "Sun"
	  case 1 => "Mon"
	  case 2 => "Tue"
	  case 3 => "Wed"
	  case 4 => "Thu"
	  case 5 => "Fri"
	  case 6 => "Sat"
	}
	
	
    def main(args:Array[String]) {
	    val offset = 2 //1.jan 1901 is Tuesday
	    var days = 0
	    var cnt = 0

	    for (year <- 1901 to 2000)
		    for (month <- 1 to 12) {			
			    if((days+offset)%7==0){
				    cnt+=1
				    println(month, year, daysinmonth(month, year));				
			    }
			    println(dayname(days+offset), month, year, daysinmonth(month, year));
			    days += daysinmonth(month, year)
		    }
	
	    println(cnt)
    }
}
