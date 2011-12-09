package euler

object P17 {
	def main(args: Array[String]) {
		var sum=0;
		for (i <- 1 to 10000) {
			var n = num(i, i.toString.length);
			println(n)			
			sum+=n.length
		}
			
		println(sum)
	}

	def num(i: Int, lvl: Int): String = (i, lvl) match {
		case (1,1)=>"one"
			case (10,2)=>"ten"
			case (11,2)=>"eleven"
			case (12,2)=>"twelve"
			case (13,2)=>"thirteen"
			case (14,2)=>"fourteen"
			case (15,2)=>"fifteen"
			case (16,2)=>"sixteen"
			case (17,2)=>"seventeen"
			case (18,2)=>"eighteen"
			case (19,2)=>"nineteen"
		case (2,1)=>"two"
			case (2,2)=>"twenty"+num(i/10,lvl-1)
		case (3,1)=>"three"
			case (3,2)=>"thirty"+num(i/10,lvl-1)
		case (4,1)=>"four"
			case (4,2)=>"forty"+num(i/10,lvl-1)
		case (5,1)=>"five"
			case (5,2)=>"fifty"+num(i/10,lvl-1)
		case (6,1)=>"six"
			case (6,2)=>"sixty"+num(i/10,lvl-1)
		case (7,1)=>"seven"
			case (7,2)=>"seventy"+num(i/10,lvl-1)
		case (8,1)=>"eight"
			case (8,2)=>"eighty"+num(i/10,lvl-1)
		case (9,1)=>"nine"
			case (9,2)=>"ninety"+num(i/10,lvl-1)
		case (_,3)=>num(i/100,1)+"hundred"+{if(i%100>0)"and"+num(i%100,(i%100).toString.length)else""} 
		case (_,4)=>num(i/1000,1)+"thousand"+num(i%1000,(i%1000).toString.length) 
		case _ =>
			if (i/10 != 0)
				num(i/10, lvl) + num(i%10, lvl-1)				
			else
				""
	}
	
}
