/*package euler

object P16 extends Application {
	var res = "2"
	var partLen = 9;//32bit vedno shendla 10^9
	for(i <- 2 to 1000) {
		var carry = 0
		var sb = new StringBuilder
		for(j <- res.length-partLen until -partLen by -partLen) {
			val (len, pos) = if(j>=0) (partLen, j) else (partLen+j, 0)
			val part = res.substring(pos, pos+len);
			//println("\t-->"+part);
			var curr = part.toInt*2 + carry;
			carry = curr/Math.pow(10, partLen).toInt;
			curr = curr%Math.pow(10, partLen).toInt;
			
			sb.insert(0,curr);
			while(sb.length()%partLen!=0) 
				sb.insert(0,0);
		}
		if(carry>0) 
			sb.insert(0,carry);
		
		res = sb.toString();
		//println(res+"\t"+i);
	}
	var sum=0;
	for(i <- 0 until res.length) 
		sum+=res.charAt(i).toString.toInt;
	
	println(sum);
}*/

object p16 extends App {
    println(BigInt(2).pow(1000).toString.toCharArray.foldLeft(0)((sum,n)=>sum+n-'0'))
}