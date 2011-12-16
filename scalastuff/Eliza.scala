// an implemetation of the Eliza chatbot, maybe with some twists in the future :P
// initial code copied from: http://www.slideshare.net/vsssuresh/scala-as-a-declarative-language slide 38
object Eliza extends App {
    import scala.util.Random.{nextInt=>randomInt}
    import scala.collection.mutable._

    def clean(s:String):String = s.replaceAll(","," ").replaceAll("[\\?\\.!]", "").toLowerCase;
    
    def preprocess(s:String):String = Map(
            "i'm"->"i am", 
            "they're"->"they are", 
            "we're"->"we are",
            "don't"->"dont").foldLeft(s)((acc,map) => acc.replaceAll(map._1, map._2));
            
    def preprocessEnd(s:String):String = 
        List("now").find(s.endsWith(_)) match {
            case Some(rep) => s.substring(0, s.length-rep.length)
            case None => s
        }

    def randomString(s:String*):String = s(randomInt(s.length))
    
    def response(stim:String):String = preprocessEnd(preprocess(clean(stim))).replaceAll("[\\s]+", " ").split(" ").toList match {
        case List("hello") | List("hi") => randomString(
                "How do you do?",
                "Hi.")
        case "i"::"am"::x => randomString(
                "How long have you been "+x.mkString(" ")+"?",
                "How does being "+x.mkString(" ")+" make you feel?")
        case "i"::"feel"::"like"::x => randomString(
                "Why is that?",
                "Why do you think that is?")
        case "i"::"dont"::x => randomString(
                "Why don't you "+x.mkString(" ")+"?")
        case "i"::verb::x => randomString(
                "Tell me more about "+x.mkString(" ")+".",
                "Does anyone else you know "+verb+" "+x.mkString(" ")+"?")
        case w1::"you"::x::"me"::_ => randomString(
                "What makes you think I "+x+" you?",
                "Why do you think I "+x+" you?")
        case "they"::"are"::x::_ => randomString(
                "Why do you think they're "+x+"?")
        case w1::w2::"you"::x::"me"::_ =>
            "What makes you think I "+x+" you?"
        case "because"::_ => randomString(
                "I understand... Can you tell me more?",
                "I understand... How does that make you feel?"
            )
        case _ => randomString(
                "Why is that?",
                "Please go on.",
                "Please tell me more.")
    }
    
    var input = "";
    do {
        input = readLine()
        println(response(input))
    } while(input!="")
}
