/*
 * Advent of Code
 * Day 13
*/

import scala.io.Source


enum Packet {
  case Packets(packets: Seq[Packet])
  case Number(value: Int)
}

object PacketOrder extends Ordering[Packet] {
  def compare(p1: Packet, p2: Packet) = {
    import Packet._
    (p1, p2) match {
      case (Number(i1), Number(i2)) => i1.compare(i2)
      case (Packets(children1), Packets(children2)) => {
        // compare element wise
        children1.zip(children2).map((p1, p2) => compare(p1, p2))
          .filter(_ != 0)
          .lift(0) // get Option from index 0
          // otherwise compare length
          .getOrElse(children1.length.compare(children2.length))
      }
      // one list, one number: wrap number in list & compare
      case (n: Number, l: Packets) => compare(Packets(List(n)), l)
      case (l: Packets, n: Number) => compare(l, Packets(List(n)))
    }
  }
}

def parse(s: String): Packet = {
  val token = s.trim
  token(0) match {
    case '[' => {
      // unwrap list content
      val contents = token.slice(1, token.length - 1)
      // detect nesting level of each character
      val levels = contents.foldLeft(List[Int](0)) {
        case (levels, '[') => levels :+ (levels.last + 1)
        case (levels, ']') => levels :+ (levels.last - 1)
        case (levels, _) => levels :+ levels.last
      }
      // parse list tokens
      val tokens = levels.zip(contents).foldLeft(List[String]("")) {
        // comma delimits new token the first level
        case (tokens, (0, ',')) => "" :: tokens
        case (head :: tail, (_, c)) => (head + c) :: tail
        case _ => throw new RuntimeException("Expected tokens list to be non empty")
      }.reverse.filter(_.length > 0)

      Packet.Packets(tokens.map(parse))
    }
    case _ => Packet.Number(token.toInt)
  }
}

@main
def solve(path: String) = {
  // setup implicits so that we can use < operator on packets
  import math.Ordered.orderingToOrdered
  given Ordering[Packet] = PacketOrder

  // part 2
  // additional packets
  val additional = List(parse("[[2]]"), parse("[[6]]"))
  val packets = (Source.fromFile(path)
    .getLines
    .filter(_.length > 0)
    .map(parse)
    .toList :++ additional)
  
  println(packets
    .sorted
    .zipWithIndex
    .filter((p, _) => additional.contains(p))
    .map((_, i) => i + 1)
    .product)
}
