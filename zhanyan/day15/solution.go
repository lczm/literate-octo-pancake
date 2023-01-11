//
// Advent of Code
// Day 15 solution
//

package main

import (
	"fmt"
	"log"
	"math"
	"os"
	"regexp"
	"sort"
	"strconv"
)

type Point struct {
	x, y int
}

type Segment struct {
	begin, end int
}

func (s Segment) distance() int {
	return s.end - s.begin
}

type Sensor struct {
	// position of the sensor itself
	position Point
	// position of the closest beacon to the sensor
	closestBeacon Point
}

// Compute manhattan distance between sensor's current point & closest beacon
func (s Sensor) distance() int {
	return abs(s.position.x-s.closestBeacon.x) + abs(s.position.y-s.closestBeacon.y)
}

// Returns the absolute value
func abs(x int) int {
	if x > 0 {
		return x
	} else {
		return -x
	}
}

// Clips the given value between upper & lower bounds (inclusive)
func clip(x int, lower int, upper int) int {
	if x < lower {
		return lower
	} else if x > upper {
		return upper
	} else {
		return x
	}
}

func parseSensors(input string) []Sensor {
	pointRegex := regexp.MustCompile(`x=(-?\d+), y=(-?\d+)`)
	matches := pointRegex.FindAllStringSubmatch(input, -1)
	sensors := make([]Sensor, len(matches)/2)
	max := math.MinInt
	for i := range sensors {
		// parse sensor position
		var err error
		sensors[i].position.x, err = strconv.Atoi(matches[2*i][1])
		if err != nil {
			log.Fatalln(err)
		}
		sensors[i].position.y, err = strconv.Atoi(matches[2*i][2])
		if err != nil {
			log.Fatalln(err)
		}

		// parse closest beacon position
		sensors[i].closestBeacon.x, err = strconv.Atoi(matches[2*i+1][1])
		if err != nil {
			log.Fatalln(err)
		}
		sensors[i].closestBeacon.y, err = strconv.Atoi(matches[2*i+1][2])
		if err != nil {
			log.Fatalln(err)
		}
		if sensors[i].distance() > max {
			max = sensors[i].distance()
		}
	}

	return sensors
}

// Merge all given line segments
func mergeAllSegments(segments []Segment) []Segment {
	if len(segments) < 1 {
		// no segments: no merging needs to be done
		return segments
	}

	// sort segments in the order of increasing begin
	sort.Slice(segments, func(i, j int) bool {
		return segments[i].begin < segments[j].begin
	})

	merged := []Segment{segments[0]}
	for _, segment := range segments {
		last := &merged[len(merged)-1]
		if last.end >= segment.end {
			// last segment overlaps current segment
			continue
		} else if last.end >= segment.begin {
			// segment when merged is a contination of last segment
			last.end = segment.end
		} else {
			merged = append(merged, segment)
		}
	}
	return merged
}

func main() {
	// parse locations of sensors & becons from input
	if len(os.Args) < 2 {
		log.Fatalln("Expected input file to be passed as first argument")
	}
	inputBytes, err := os.ReadFile(os.Args[1])
	if err != nil {
		log.Fatalf("Failed to read input file: %s\n", err)
	}
	sensors := parseSensors(string(inputBytes))

	for y := 0; y < 4000000; y++ {
		// collate line semgents on horizontal line y considered empty
		segments := make([]Segment, 0)
		for _, sensor := range sensors {
			// locate sensors with scanned areas that intersect y
			if deltaY := abs(sensor.position.y - y); deltaY <= sensor.distance() {
				deltaX := sensor.distance() - deltaY
				segments = append(segments, Segment{
					begin: clip(sensor.position.x-deltaX, 0, 4000000),
					end:   clip(sensor.position.x+deltaX, 0, 4000000),
				})
			}
		}
		// merge overlaps on line segments
		segments = mergeAllSegments(segments)

		// check for the gap in scanned area
		if len(segments) > 1 {
			x := segments[0].end + 1
			frequency := uint64(x)*4000000 + uint64(y)
			fmt.Println(frequency)
			return
		}
	}
	log.Fatalln("Gap not found")
}
