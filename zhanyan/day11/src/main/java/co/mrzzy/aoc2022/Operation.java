/**
 * Advent of Code
 * Day 11
 */

package co.mrzzy.aoc2022;

/** Operation performed by a Monkey when interacting with an item */
public interface Operation {
    /** Returns the worry level after interacting with the item */
    public long interact(long item);
}
