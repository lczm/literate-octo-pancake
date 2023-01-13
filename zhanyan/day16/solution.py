#
# Advent of Code
# Day 16
#

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


def optimize(
    valves: Dict[str, Valve],
    current: str,
    on_valves: FrozenSet[str] = frozenset(),
    time_left: int = 30,
    memo: Dict[Tuple[str, FrozenSet[str], int], int] = {},
) -> int:
    """Traverse the given valves to find optimal order to toggle valves & release the most pressure."""
    # base case: no more time or all valves on
    if time_left <= 0 or len(on_valves) >= len(valves):
        return 0
    # use cached result if present
    memo_key = (current, on_valves, time_left)
    if memo_key in memo:
        return memo[memo_key]

    # recursively explore opening & not opening the valve
    max_pressure = 0
    for adjacent in valves[current].connected:
        open_pressure = 0
        # optimization: skip opening valves that have no flowrate
        if current not in on_valves and valves[current].flowrate > 0:
            # explore opening the valve. T-2 needed to open & travel to next valves
            open_pressure = (
                optimize(
                    valves,
                    adjacent,
                    on_valves | frozenset((current,)),
                    time_left - 2,
                    memo,
                )
                # calculate pressure relief brought on by current valve.
                # T-1 to account for time need to open valve
                + valves[current].flowrate * (time_left - 1)
            )

        max_pressure = max(
            max_pressure,
            open_pressure,
            # explore leaving the valve untouched. T-1 needed to travel to next valve
            optimize(valves, adjacent, on_valves, time_left - 1, memo),
        )

    # cache result for future calls
    memo[memo_key] = max_pressure
    return max_pressure


if __name__ == "__main__":
    if len(sys.argv) < 2:
        raise FileNotFoundError(
            "Expected input file to be passed via command line arg."
        )
    with open(sys.argv[1]) as f:
        valves = {v.name: v for v in [Valve.parse(l) for l in f.readlines()]}

    # part 1
    print(optimize(valves, "AA", time_left=30))
