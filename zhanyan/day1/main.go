/*
Advent of Code 2022
Day 1: Calorie Counting
*/
package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

// Find & return max given value lower than given limit
func maxLower(values []uint, limit uint) uint {
	maxValue := uint(0)
	for _, value := range values {
		if value < limit && value > maxValue {
			maxValue = value
		}
	}
	return maxValue
}

func main() {
	// parse input as calories per elf
	input, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Failed to read input:", err)
	}
	lines := strings.Split(string(input), "\n")

	// tabulate total calories per elf
	elvesCalories, calories := make([]uint, 0), uint(0)
	for _, line := range lines {
		if len(line) == 0 {
			// empty line signals next elf's calories
			elvesCalories = append(elvesCalories, calories)
			calories = 0
			continue
		}
		calorie, err := strconv.ParseUint(line, 10, 32)
		if err != nil {
			fmt.Println("Failed to parse calorie as unsigned int: ", err)
		}
		calories += uint(calorie)
	}

	top3Calories, prevMaxCalories := uint(0), uint(math.MaxUint)
	for topN := 1; topN <= 3; topN++ {
		prevMaxCalories = maxLower(elvesCalories, prevMaxCalories)
		top3Calories += prevMaxCalories
	}
	fmt.Printf("Top 3 sum calories: %d", top3Calories)
}
