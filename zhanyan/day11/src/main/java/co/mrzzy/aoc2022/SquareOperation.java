/**
 * Advent of Code
 * Day 11
 */

package co.mrzzy.aoc2022;

/* Operation: old * old */
public class SquareOperation implements Operation {
    @Override
    public long interact(long item) {
        return item * item;
    }
}
