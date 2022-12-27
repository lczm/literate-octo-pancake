/*k
 * Advent of Code
 * Day 11
 */
package co.mrzzy.aoc2022;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class Solution {
    public static void main(String[] args) {
        // read input file into string
        if (args.length < 1) {
            throw new IllegalArgumentException("Expected input file to be passed as argument.");
        }
        String input = "";
        try {
            input = Files.readString(FileSystems.getDefault().getPath(args[0]));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        // Parse inputs into monkeys
        List<Monkey> monkeys = Arrays.asList(input.split("\n\n")).stream()
                .map(Monkey::parse)
                .sorted(Comparator.comparing(Monkey::getId))
                .collect(Collectors.toList());

        int commonMultiple = monkeys.stream()
                .map(Monkey::getTestDivisor)
                .reduce(1, (acc, next) -> acc * next);

        // Simulate monkeys
        for (int i = 1; i <= 10000; i++) {
            for (Monkey monkey : monkeys) {
                for (long itemWorry : monkey.getItems()) {
                    int newWorryItem = (int) (monkey.interact(itemWorry) % commonMultiple);
                    // monkey throws its item away to monkeys depending on its divisor test
                    if (newWorryItem % monkey.getTestDivisor() == 0) {
                        monkeys.get(monkey.getTrueTargetId()).getItems().add(newWorryItem);
                    } else {
                        monkeys.get(monkey.getFalseTargetId()).getItems().add(newWorryItem);
                    }
                }
                monkey.getItems().clear();
            }
        }

        // Part 2
        long monkeyBusiness = monkeys.stream()
                .map(Monkey::getnInteractions)
                .sorted(Comparator.reverseOrder())
                .limit(2)
                .map(Long::valueOf)
                .reduce((first, second) -> first * second)
                .get();
        System.out.println(String.format("Monkey business: %d", monkeyBusiness));
    }
}
