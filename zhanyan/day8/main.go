//
// Advent of Code
// Day 8
//

package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

// Direction to move on a 2D grid
type Direction int8

const (
	Up Direction = iota
	Down
	Left
	Right
)

// Point on a 2D grid
type Point struct {
	x, y int8
}

// Move the point in the given direction.
func (pt Point) move(direction Direction) Point {
	if direction == Up {
		return Point{pt.x, pt.y - 1}
	}
	if direction == Down {
		return Point{pt.x, pt.y + 1}
	}
	if direction == Left {
		return Point{pt.x - 1, pt.y}
	}
	// Right
	return Point{pt.x + 1, pt.y}
}

// Bounds on a 2D grid
type Bounds struct {
	endX, endY int8
}

// Check whether given point is within the current points
func (b Bounds) within(pt Point) bool {
	return pt.x >= 0 && pt.y >= 0 && pt.x < b.endX && pt.y < b.endY
}

func main() {
	// read input from file
	if len(os.Args) <= 1 {
		fmt.Println("Expected input file to be passed as argument")
		os.Exit(1)
	}
	inBytes, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Printf("Failed to read input file at path: %e", err)
		os.Exit(1)
	}
	lines := strings.Split(string(inBytes), "\n")
	// - 1 skip last empty line
	lines = lines[:len(lines)-1]

	// parse square grid of tree heights from input
	grid := make([][]int8, len(lines))
	for r, line := range lines {
		grid[r] = make([]int8, len(lines[0]))
		for c, char := range line {
			height, err := strconv.ParseInt(string(char), 10, 32)
			if err != nil {
				fmt.Printf("Failed to parse: %e", err)
				os.Exit(1)
			}
			grid[r][c] = int8(height)
		}
	}
	bounds := Bounds{int8(len(grid[0])), int8(len(grid))}

	maxScenicDist := 0
	for y := int8(0); y < bounds.endY; y++ {
		for x := int8(0); x < bounds.endX; x++ {
			// tabulate scenic distance of the tree at this point against all directions
			scenicDist := 1
			for _, direction := range []Direction{Up, Left, Down, Right} {
				// advance in direction to measure scenic distance (ie. distance to taller tree)
				startPt, pt, ourHeight, distance := Point{x, y}, Point{x, y}, grid[y][x], 0
				for bounds.within(pt) {
					height := grid[pt.y][pt.x]
					if pt != startPt && height >= ourHeight {
						// bound taller tree. stop.
						break
					}
					pt = pt.move(direction)
					if bounds.within(pt) {
						distance += 1
					}
				}
				scenicDist *= distance
			}

			if scenicDist >= maxScenicDist {
				maxScenicDist = scenicDist
			}
		}
	}
	fmt.Println(maxScenicDist)
}
