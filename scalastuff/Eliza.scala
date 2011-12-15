// an implemetation of the Eliza chatbot, maybe with some twists in the future :P
// initial code copied from: http://www.slideshare.net/vsssuresh/scala-as-a-declarative-language slide 38
object Eliza extends App {
    import scala.util.Random.{nextInt=>randomInt}

    def clean(s:String):String = s.replaceAll("[,]"," ").replaceAll("[\\?\\.!]", "").replaceAll("[\\s]+", " ").toLowerCase;
    def preprocess(s:String):String = s.replaceAll("i'm","i am");
    def randomString(s:String*):String = s(randomInt(s.length))
    
    def response(stim:String):String = preprocess(clean(stim)).split(" ").toList match {
        case List("hello") | List("hi") => randomString(
                "How do you do?",
                "Hi.")
        case "i"::"am"::x => randomString(
                "How long have you been "+x.mkString(" ")+"?",
                "How does being "+x.mkString(" ")+" make you feel?")
        case "i"::"feel"::"like"::x => randomString(
                "Why do you think that is?")
        case "i"::verb::x => randomString(
                "Tell me more about "+x.mkString(" ")+".",
                "Does anyone else you know "+verb+" "+x.mkString(" ")+"?")
        case w1::"you"::x::"me"::_ => randomString(
                "What makes you think I "+x+" you?",
                "Why do you think I "+x+" you?")
        case "they're"::x::_ => randomString(
                "Why do you think they're "+x+"?")
        case w1::w2::"you"::x::"me"::_ =>
            "What makes you think I "+x+" you?"
        case _ => randomString(
                "Please go on.",
                "Tell me more.")
    }
    
    var input = "";
    do {
        input = readLine()
        println(response(input))
    } while(input!="")
}
