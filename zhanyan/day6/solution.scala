/*
 * Advent of Code
 * Day 6 solution
*/

import scala.io.Source;

def main(args: Array[String]) = {
  val nMarker = 14;
  println(Source.fromFile(args.head) // read input file
    .sliding(nMarker) // iterate over sliding windows groups of marker sized chars
    .map(_.distinct.size) // count no. of distinct elements in each group
    .indexWhere(_ == nMarker) // index of first unique group, signalling market
  + nMarker); // to adjust for characters processed in the unique group.
}
