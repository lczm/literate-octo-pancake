package co.mrzzy.aoc2022;


import org.junit.Test;

public class SolutionTest {
    @Test
    public void testInput() {
        Solution.main(new String[] {
                Solution.class.getResource("input.txt").getPath()
        });
    }
}
