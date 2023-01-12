#
# Advent of Code
# Day 16
#

import sys
import re
from typing import Dict, List, Set


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
    on_valves: Set[str] = set(),
    time_left: int = 30,
) -> int:
    """Traverse the given valves to find optimal order to toggle valves & release the most pressure."""
    # base case: no more time
    if time_left <= 0:
        return 0

    # recursively explore opening & not opening the valve
    max_pressure = 0
    for adjacent in valves[current].connected:
        # skip already on valves
        if adjacent in on_valves:
            continue

        # pressure released if the valve is open
        current_release = valves[current].flowrate * (time_left - 1)

        max_pressure = max(
            max_pressure,
            # explore opening the valve. T-2 needed to open & travel to next valve
            optimize(valves, adjacent, set(current, *on_valves), time_left - 2)
            + current_release,
            # explore opening the valve. T-1 needed to travel to next valve
            optimize(valves, adjacent, on_valves.copy(), time_left - 1),
        )

    return max_pressure


if __name__ == "__main__":
    if len(sys.argv) < 2:
        raise FileNotFoundError(
            "Expected input file to be passed via command line arg."
        )
    with open(sys.argv[1]) as f:
        valves = {v.name: v for v in [Valve.parse(l) for l in f.readlines()]}
