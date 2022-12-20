#
# Advent of Code
# Day 9
#

import sys
from enum import Enum
from typing import Tuple
from math import copysign


def parse_command(line: str) -> Tuple[str, int]:
    """Parse movement command the format: '<DIRECTION> <STEPS>'"""
    direction, n_moves = line.split()
    return direction, int(n_moves)


Point = Tuple[int, int]


def distance(head_pos: Point, tail_pos: Point) -> int:
    """Calculate the distance (no. moves needed to catch up) between head & tail."""
    head_x, head_y = head_pos
    tail_x, tail_y = tail_pos
    return max(abs(tail_x - head_x), abs(tail_y - head_y))


def normalise(x: int) -> int:
    if x > 0:
        return 1
    elif x < 0:
        return -1
    else:
        return 0


def move(direction: str, head_pos: Point) -> Point:
    """Simulate a move of the rope with head positions in the given direction."""
    head_x, head_y = head_pos

    # calculate next head position based on command position
    if direction == "U":
        return (head_x, head_y - 1)
    elif direction == "D":
        return (head_x, head_y + 1)
    elif direction == "L":
        return (head_x - 1, head_y)
    elif direction == "R":
        return (head_x + 1, head_y)
    else:
        raise ValueError(f"Unknown direction {direction}")


def follow(follow_pos: Point, lead_pos: Point) -> Point:
    """Update the follower's position after leader moves into this position."""
    lead_x, lead_y = lead_pos
    follow_x, follow_y = follow_pos
    delta_x, delta_y = lead_x - follow_x, lead_y - follow_y

    if max(abs(delta_x), abs(delta_y)) > 1:
        heading_x, heading_y = normalise(delta_x), normalise(delta_y)
        # leader is too far ahead: move to catch up
        return follow_x + heading_x, follow_y + heading_y
    return follow_pos


if __name__ == "__main__":
    # parse head movement commands
    if len(sys.argv) < 2:
        raise ValueError("Expected to given input file path as arg.")
    with open(sys.argv[1], "r") as f:
        commands = [parse_command(line) for line in f.readlines()]

    # all knots start at the same point
    unique_tail_pos = set()
    knots = [(int(0), int(0)) for _ in range(10)]
    for direction, n_times in commands:
        for _ in range(n_times):
            # move head knot according to movement commands
            knots[0] = move(direction, knots[0])
            for i in range(1, len(knots)):
                knots[i] = follow(knots[i], knots[i - 1])
            # track unique positions of tail knot
            unique_tail_pos.add(knots[-1])

    print(f"Unique positions: {len(unique_tail_pos)}")
