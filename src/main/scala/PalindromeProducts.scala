case class PalindromeProducts(i: Int, j: Int):
   lazy val l: Seq[(Int, Set[(Int, Int)])] = for
      a <- i to j
      b <- a to j
      if (b * a).toString == (b * a).toString.reverse
   yield (a * b, Set((a, b)))

   val smallest: Option[(Int, Set[(Int, Int)])] =
      if l.nonEmpty then Some(l.minBy(_._1)) else None

   val largest: Option[(Int, Set[(Int, Int)])] = {
      println(l.groupBy(_._1).values)
      if l.nonEmpty then Some(l.maxBy(_._1)) else None
   }
