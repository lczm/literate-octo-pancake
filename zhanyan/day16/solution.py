#
# Advent of Code
# Day 16
#

from collections import defaultdict
import sys
import re
from typing import Dict, ForwardRef, FrozenSet, List, Set, Tuple
from itertools import chain, combinations


class Valve:
    PARSE_REGEX = re.compile(
        r"Valve (?P<name>[A-Z]{2}) has flow rate=(?P<flowrate>\d+); tunnels? leads? to valves? (?P<connected>[A-Z, ]+[A-Z])"
    )

    def __init__(self, name: str, flowrate: int, connected: List[str]):
        self.name = name
        self.flowrate = flowrate
        self.connected = connected

    @classmethod
    def parse(cls, valve_str: str) -> "Valve":
        """Parse a valve from the question's input format."""
        match = cls.PARSE_REGEX.match(valve_str)
        if match is None:
            raise ValueError(f"Unable to parse as Valve: {valve_str}")
        return cls(
            name=match.group("name"),
            flowrate=int(match.group("flowrate")),
            connected=match.group("connected").split(", "),
        )

def measure_times(valves: Dict[str, Valve]) -> Dict[Tuple[str, str], int]:
    """
    Compute the minimum travel time between all pairs of valve with Floyd-Washall.
    Travel time assumes that valves are not opened, only traversed.
    """
    min_times = {} # type: Dict[Tuple[str, str], int]
    names = list(valves.keys())
    for name in names:
        # remaining at the same vertex incurs no travel time
        min_times[(name, name)] = 0
        # initalise travel times for directly connected valves
        for adjacent in valves[name].connected:
            min_times[(name, adjacent)] = 1

    # shortest path is the shorter path of 
    for k in names:
        for src in names:
            for dest in names:
                min_times[(src, dest)] = min(
                    # 1. a path that passes through path valve k (if it exists)
                    (
                        min_times.get((src, k), sys.maxsize) +
                        min_times.get((k, dest), sys.maxsize)
                    ),
                    # 2. a path that does not pass through path valve k (if it exists)
                    min_times.get((src, dest), sys.maxsize),
                )

    return min_times

def find_max_pressure(
    valves: Dict[str, Valve],
    min_times: Dict[Tuple[str, str], int],
    openable: FrozenSet[str],
    current: str,
    time_left: int,
    memo: Dict[Tuple[FrozenSet[str], str, int], int] = {},
) -> int:
    """Find the max pressure that can be released by traversing the graph & open valves"""
    # base case: no more pressure can be released once time or openable valves runs out
    if len(openable) <= 0 or time_left <= 0:
        return 0

    memo_key = (openable, current, time_left) 
    if  memo_key in memo:
        return memo[memo_key]

    # calculate pressure release from opening current valve
    open_pressure = 0
    if current in openable:
        time_left -= 1
        open_pressure = valves[current].flowrate  * time_left
        openable = openable - {current}

    # recursively explore opening other valves
    max_pressure =  open_pressure + max([
        find_max_pressure(
            valves,
            min_times,
            openable,
            current=next_valve,
            # deduct time to travel to next valve
            time_left=time_left - min_times[(current, next_valve)],
        )
        for next_valve in openable
    ]) if len(openable) > 0 else 0

    memo[memo_key] = max_pressure
    return max_pressure

if __name__ == "__main__":
    if len(sys.argv) < 2:
        raise FileNotFoundError(
            "Expected input file to be passed via command line arg."
        )
    with open(sys.argv[1]) as f:
        valves = {v.name: v for v in [Valve.parse(l) for l in f.readlines()]}

    # optimization: skip with opening valves with zero-flowrate
    openable = frozenset([v.name for v in valves.values() if v.flowrate > 0])
    # optimization: precompute smallest distances between all valves
    min_times = measure_times(valves)

    # in theory, the best strategy between you & the elephant is to divide the openable
    # valves into 2 halfs, each party working on half.
    # try all combinations of splitting into two halfs between you & the elephant to find the best split
    max_pressure = 0
    for you_open in combinations(openable, len(openable)//2):
        you_open = frozenset(you_open)
        elephant_open = openable - you_open
        args = {
            "valves": valves,
            "min_times": min_times,
            "current": "AA",
            "time_left": 26,
        }
        max_pressure = max(max_pressure, (
            find_max_pressure(openable=you_open, **args) +
            find_max_pressure(openable=elephant_open, **args)
        ))
    print(max_pressure)
