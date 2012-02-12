// an implemetation of the Eliza chatbot, maybe with some twists in the future :P
// initial code copied from: http://www.slideshare.net/vsssuresh/scala-as-a-declarative-language slide 38

//TODO:
//sound less like a therapist :)
//time (x ago, right now, etc)
//probabalistic state machine (if something just said don't repeat it, remember some verbs and nouns)
object Eliza extends App {
    import scala.util.Random.{nextInt=>randomInt}
    import scala.collection.mutable._
    
    //TODO start a utils file
    class PimpedString(s:String) { //expand replaceAll and list compare
        def replaceAll(m:(String,String)*):String = 
            m.foldLeft(s)((out,rep)=> out.replaceAll(rep._1,rep._2))
    }
    object PimpedString {
        implicit def String2PimpedString(s:String):PimpedString = new PimpedString(s)
    }
    import PimpedString._
    
    def randomString(s:String*):String = s(randomInt(s.length))

    def clean(s:String):String = 
        s.replaceAll(
            ","->" ",
            "[\\?\\.!]"->"",
            "[\\s]+"->" "
        ).toLowerCase

    def preprocessEnd(s:String):String = 
        List(
            " now",
            " then"
        ).find(pre=> s.endsWith(pre) && s.length >= 2*pre.length) match {
            case Some(rep) => s.substring(0, s.length-rep.length)
            case None => s
        }

    def preprocessStart(s:String):String = 
        List(
            "dsfdsfdsaacwe"
        ).find(pre=> s.startsWith(pre) && s.length >= 2*pre.length) match {
            case Some(rep) => s.substring(s.length-rep.length)
            case None => s
        }
    
    def preprocess(s:String):String = 
        clean(preprocessEnd(preprocessStart(clean(s).replaceAll(
            "i'm"->"i am", 
            "i've"->"i have", 
            "i'll"->"i will", 
            "i'd"->"i would",
            "i was"->"i am", //TODO - seems to work
            "gonna"->"going to", 
            "they're"->"they are", 
            "we're"->"we are",
            "don't"->"dont"
        ))))
            

    def postprocess(s:String):String = s.replaceAll("[\\s]+"-> " ");
    
    abstract class MemoryType
    object IsVerbing extends MemoryType
    object IsNoun extends MemoryType
    object Memory {
        //TODO: manipulate memory "don't want to talk about it", etc
        //TODO save to file
        var memories = HashMap[MemoryType, LinkedHashSet[String]]()
        def <=(memory:MemoryType,s:String) = memories.getOrElseUpdate(memory, new LinkedHashSet[String]) += s        
        def ->(mtype:MemoryType, sentence:Option[String]=None):Option[String] = {
            //TODO analyze sentence to retrieve apropriate memory
            val mems = memories.get(mtype)
            if(mems.isDefined)
                Some(randomString(mems.get.toSeq:_*))
            else 
                None
        }
    }
    
    def response(input:String):String = 
        postprocess(preprocess(input).split(" ").toList match {
            case List("hello") | List("hi") => randomString(
                    "How do you do?",
                    "Hi. How are you?")
            case "i"::"am"::x => 
                if(x.length>0 && x(0).endsWith("ing")) { // doING something
                    val x2 = x.map(w=> if(List("my","mine").contains(w)) "your" else w)
                    Memory <= (IsVerbing, x2.mkString(" "))
                    randomString(
                        "How does "+x2.mkString(" ")+" make you feel?",
                        "How long have you been "+x2.mkString(" ")+"?")
                } else if(x.length>0 && List("a","an","the").contains(x(0))) { // being A something
                    Memory <= (IsNoun, x.mkString(" "));
                    randomString(
                        "How long have you been "+x.mkString(" ")+"?",
                        "How does being "+x.mkString(" ")+" make you feel?")
                } else if(x.length==1) {
                    Memory <= (IsNoun, x.mkString(" "));
                    randomString(
                        "How long have you been "+x.mkString(" ")+"?")
                } else {
                    randomString(
                        "How does that make you feel?",
                        "How long have you been "+x.mkString(" ")+"?")
                }
            case "i"::"feel"::"like"::x => 
                    if(x.length>0 && x(0)=="my") 
                        randomString(
                            "Why do you think your "+x.mkString(" ")+"?")
                    else 
                        randomString(
                            "What makes you think that?",
                            "Why do you think that is?")
            case "i"::"feel"::x => 
                    randomString(
                        "How long have you been feeling"+x.mkString(" ")+"?",
                        if(x.length>1) "Does anyone else you know "+x(0)+" "+x.tail.mkString(" ")+"?" else
                        "Why do you feel that way?")
            case "i"::"dont"::x => 
                randomString(
                    "Why don't you "+x.mkString(" ")+"?")
            case "i"::"would"::x => 
                randomString(
                    "Why don't you?")
            case "i"::verb::x =>
                randomString(
                    "Tell me more about "+x.mkString(" ")+".",
                    "Does anyone else you know "+verb+" "+x.mkString(" ")+"?")
            case w1::"you"::x::"me"::_ => 
                randomString(
                    "What makes you think I "+x+" you?",
                    "Why do you think I "+x+" you?")
            case w1::w2::"you"::x::"me"::_ => 
                randomString(
                    "What makes you think I "+x+" you?")
            case "they"::"are"::x::_ => randomString(
                    "Why do you think they're "+x+"?")
            case "because"::_ => 
                val mem = Memory->(IsVerbing)
                randomString(
                    if(mem.isDefined) {
                        "OK. would you like to talk about "+mem.get+"?"
                    } else
                    "I understand... would you like to talk about something else?",
                    "OK... but how does that make you feel?"
                )
            case "since"::x => 
                randomString(
                    "What did you do before that?"
                )
            case "for"::"instance"::x => 
                    if(x.length > 1) 
                        randomString("Can you think of any other examples?") //for instance, bla bla bla
                    else 
                        randomString( //"lets talk about something else" "for instance?"
                            "Let's talk about what you're doing",
                            "How are your life plans progressing",
                            "What can you tell me about yourself?",
                            {
                                val memNoun = Memory->(IsNoun)
                                val memVerb = Memory->(IsVerbing)

                                if(memNoun.isDefined) "Let's talk more about you being "+memNoun.get+"." else
                                if(memVerb.isDefined) "Let's talk about "+memVerb.get+" some more." else
                                "Please tell me more."
                            }
                        )
            case "for"::x => 
                randomString(
                    if(x.contains("years")||x.contains("long")||x.contains("while")) 
                        "What can you recall from before that?"
                    else 
                        "What did you think before that?"
                )
            case "yes"::x => 
                randomString(
                    "You seem sure...",
                    "Are you certain?",
                    "Are you sure?")
            case "no"::x => 
                randomString(
                    "Why not?")
            case x => 
                if(x.contains("you")) randomString(
                    "Lets talk about something else...",
                    "Do you really think that about me?") //TODO
                else {
                    val memNoun = Memory->(IsNoun)
                    val memVerb = Memory->(IsVerbing)
                    randomString(
                    if(memNoun.isDefined) {
                        randomString(
                            "Let's talk more about you being "+memNoun.get+".")
                    } else
                    "Lets change the topic",
                    if(memVerb.isDefined) {
                        randomString(
                            "Let's talk about "+memVerb.get+" some more.")
                    } else
                    "Why is that?",
                    "Please tell me more.")
                }
        })
    
    var input = "";
    do {
        input = readLine()
        println(preprocess(input))
        println(Memory.memories)
        println(response(input))
    } while(input!="")
}
