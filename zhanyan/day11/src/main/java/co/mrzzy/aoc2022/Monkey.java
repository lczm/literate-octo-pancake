/**
 * Advent of Code
 * Day 11
 */

package co.mrzzy.aoc2022;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Monkey {
    private final int id;
    private final List<Integer> items;
    private final Operation operation;
    private final int testDivisor;
    private final int trueTargetId;
    private final int falseTargetId;
    private int nInteractions = 0;

    public Monkey(int id,
            List<Integer> items,
            Operation operation,
            int testDivisor,
            int trueTargetId,
            int falseTargetId) {
        this.id = id;
        this.items = items;
        this.operation = operation;
        this.testDivisor = testDivisor;
        this.trueTargetId = trueTargetId;
        this.falseTargetId = falseTargetId;
    }

    /**
     * Parse a Monkey from a input specification in the format:
     *
     * <code>
     * Monkey 0:
     *  Starting items: 62, 92, 50, 63, 62, 93, 73, 50
     *  Operation: new = old * 7
     *  Test: divisible by 2
     *      If true: throw to monkey 7
     *      If false: throw to monkey 1
     * </code>
     */
    public static Monkey parse(String specification) {
        Matcher matches = Pattern.compile(
                "Monkey (\\d+):\n" +
                        "\\s*Starting items: ([\\d, ]+)\n" +
                        "\\s*Operation: new = (.+)\n" +
                        "\\s*Test: divisible by (\\d+)\n" +
                        "\\s*If true: throw to monkey (\\d+)\n" +
                        "\\s*If false: throw to monkey (\\d+)")
                .matcher(specification);
        if (!matches.find()) {
            throw new IllegalArgumentException(String.format(
                    "Specification in unknown format: \n%s", specification));
        }

        // parse items on the monkey
        List<Integer> items = Arrays.stream(matches.group(2).split(","))
                .map((itemStr) -> Integer.parseInt(itemStr.strip()))
                .collect(Collectors.toList());

        // parse monkey's interaction operation
        String operationStr = matches.group(3);
        Matcher addResult = Pattern.compile("\\s*old \\+ (\\d+)").matcher(operationStr);
        Optional<Operation> operation = Optional.empty();
        if (addResult.find()) {
            operation = Optional.of(new AddOperation(Integer.parseInt(addResult.group(1))));
        }
        Matcher multiplyResult = Pattern.compile("\\s*old \\* (\\d+)").matcher(operationStr);
        if (multiplyResult.find()) {
            operation = Optional.of(new MultiplyOperation(Integer.parseInt(multiplyResult.group(1))));
        }
        Matcher squareResult = Pattern.compile("\\s*old \\* old").matcher(operationStr);
        if (squareResult.find()) {
            operation = Optional.of(new SquareOperation());
        }
        return new Monkey(
                Integer.parseInt(matches.group(1)),
                items,
                operation.get(),
                Integer.parseInt(matches.group(4)),
                Integer.parseInt(matches.group(5)),
                Integer.parseInt(matches.group(6)));
    }

    /* Whether this monkey is still activeloy throwing your items */
    public boolean isActive() {
        return this.items.size() > 0;
    }

    /* Calculate the item's worry after this monkey interacts with it */
    public long interact(long itemWorry) {
        this.nInteractions += 1;
        return this.operation.interact(itemWorry);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(this instanceof Monkey)) {
            return false;
        }

        Monkey other = (Monkey) obj;
        return (this.id == other.id &&
                this.items.equals(other.items) &&
                this.operation.equals(other.operation) &&
                this.testDivisor == other.testDivisor &&
                this.trueTargetId == other.trueTargetId &&
                this.falseTargetId == other.falseTargetId);
    }

    public int getId() {
        return id;
    }

    public List<Integer> getItems() {
        return items;
    }

    public int getTestDivisor() {
        return testDivisor;
    }

    public int getTrueTargetId() {
        return trueTargetId;
    }

    public int getFalseTargetId() {
        return falseTargetId;
    }

    public int getnInteractions() {
        return nInteractions;
    }
}
