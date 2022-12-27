/**
 * Advent of Code
 * Day 11
 */

package co.mrzzy.aoc2022;

/** Mulitply operation: old * value */
public record MultiplyOperation(int value) implements Operation {
    @Override
    public long interact(long item) {
        return item * this.value;
    }
}
