/**
 * Advent of Code
 * Day 11
 */

package co.mrzzy.aoc2022;

import java.util.List;

import org.junit.Test;


public class MonkeyTest {
    @Test
    public void testParse() throws Exception {
        Monkey.parse(
            "Monkey 0:\n" +
            "   Starting items: 79, 98\n" +
            "   Operation: new = old * 19\n" +
            "   Test: divisible by 23\n" +
            "       If true: throw to monkey 2\n" +
            "       If false: throw to monkey 3\n"
        ).equals(new Monkey(0, List.of(79, 98), new MultiplyOperation(19), 23, 2, 3));
    }
}
