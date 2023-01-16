#
# Advent of Code
# Day 16
#

from collections import defaultdict
import sys
import re
from typing import Dict, FrozenSet, List, Set, Tuple


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

def compute_min_times(valves: Dict[str, Valve]) -> Dict[Tuple[str, str], int]:
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

# TODO(mrzzy): try rewriting DP optimization algorithm but on min_times graph

if __name__ == "__main__":
    if len(sys.argv) < 2:
        raise FileNotFoundError(
            "Expected input file to be passed via command line arg."
        )
    with open(sys.argv[1]) as f:
        valves = {v.name: v for v in [Valve.parse(l) for l in f.readlines()]}

    # optimization: skip with opening valves with zero-flowrate
    open_valves = [v for v in valves.values() if v.flowrate > 0]
    min_times = compute_min_times(valves)
