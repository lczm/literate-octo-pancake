/*
 * Advent of Code
 * Day 4 Solution
 */

import java.nio.file.Files;
import java.nio.file.Path;
import java.io.IOException;
import java.lang.Exception;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

class Solution {
  /**
   * Range of section assignment of supplies to elfs.
   */
  public static class Range {
    private int begin;
    private int end;

    public static Range parse(String text) {
      String[] components = text.split("-");
      if (components.length != 2) {
        throw new IllegalArgumentException(
            "Expected exactly two components in range string in format: <BEGIN>-<END>");
      }

      return new Range(Integer.parseInt(components[0]), Integer.parseInt(components[1]));
    }

    /**
     * Construct a new range between {@code begin} &amp; {@code end} inclusive.
     */
    public Range(int begin, int end) {
      if (begin > end) {
        throw new IllegalArgumentException("Begin of range has to be lower than End of range.");
      }
      this.begin = begin;
      this.end = end;
    }

    public boolean isOverlapping(Range other) {
      if (this.begin <= other.begin) { // left of other range on the number line
        return this.end >= other.begin;
      } else {
        // right of other range on the number line
        return other.end >= this.begin;
      }
    }

    public int size() {
      // +1 as range is inclusive.
      return this.end - this.begin + 1;
    }
  }

  public static void main(String[] args) {
    // read input from file passed via cmd args
    if (args.length <= 0) {
      throw new RuntimeException("Expected path to input file in given arguments.");
    }
    String input;
    try {
      input = Files.readString(Path.of(args[0]));
    } catch (IOException e) {
      throw new RuntimeException("Failed to open file", e);
    }

    long nOverlapping = input.lines()
        .map((line) -> {
          // parse section assignments to elfs
          String[] assignments = line.split(",");
          if (assignments.length != 2) {
            throw new RuntimeException("Expected 2 section assignments per line");
          }
          Range first = Range.parse(assignments[0]);
          Range second = Range.parse(assignments[1]);

          return first.isOverlapping(second);
        })
        .filter((isOverlapping) -> isOverlapping)
        .count();

    System.out.println(nOverlapping);
  }
}
