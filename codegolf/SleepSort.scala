//usage: SleepSort 1 3 2 5 4
object SleepSort extends App {
    args.foreach(arg=>
        scala.concurrent.ops.spawn {
            Thread.sleep(arg.toInt * args.length+10)
            print(arg+" ")
        }
    )
}