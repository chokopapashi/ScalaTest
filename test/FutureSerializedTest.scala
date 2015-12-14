:reset

/*
 * http://stackoverflow.com/questions/20414500/how-to-do-sequential-execution-of-futures-in-scala
 */

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

val fSerialized = {
    var fAccum: Future[List[Int]] = Future{List.empty}
    for(item <- 1 to 10000) {
        println(f"Processing ${item}%d")
        fAccum = fAccum flatMap(l => Future{item :: l})
    }
    fAccum
}

fSerialized.onComplete{case resTry => println("All Done.")}


