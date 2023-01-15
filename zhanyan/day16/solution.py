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
    current: Tuple[str, str],
    on_valves: FrozenSet[str] = frozenset(),
    time_left: int = 30,
    memo: Dict[Tuple[Tuple[str, str], FrozenSet[str], int], int] = {},
) -> int:
    """Traverse the given valves to find optimal order to toggle valves & release the most pressure."""
    # base case: no more time or all valves on
    if time_left <= 0 or len(on_valves) >= len(valves):
        return 0
    # use cached result if present
    memo_key = (current, on_valves, time_left)
    if memo_key in memo:
        return memo[memo_key]

    max_pressure = 0
    you, elephant  = current
    for you_next in valves[you].connected:
        for elephant_next in valves[elephant].connected:
            # optimization: don't visit the same valve at the same time remainding
            if you_next == elephant_next:
                continue
            # optimization: skip opening valves that have no flowrate or already open
            you_options = (
                [you, None] if you not in on_valves and valves[you].flowrate > 0 else [None]
            )
            elephant_options = (
                [elephant, None] 
                    if elephant not in on_valves and valves[elephant].flowrate > 0 
                    else [None]
            )

            # explore opening or closing valves
            for you_open in you_options:
                for elephant_open in elephant_options:
                    # compute release from current valves
                    newly_opened = []
                    if you_open is not None:
                        newly_opened.append(you_open)
                    if elephant_open is not None:
                        newly_opened.append(elephant_open)

                    new_release = (
                        sum([valves[v].flowrate for v in newly_opened]) * (time_left - 1)
                    )

                    # determine position of you & elephant in next minute
                    # if you did not open a valve, you would have moved to the next valve
                    next_valves = (
                        you_next if you_open is None else you,
                        elephant_next if elephant_open is None else elephant,
                    )

                    max_pressure = max(
                        max_pressure,
                        # recursively explore opening & not opening the valve
                        new_release + optimize(
                            valves=valves, 
                            current=next_valves,
                            on_valves=on_valves | frozenset(newly_opened), 
                            time_left=time_left - 1
                        ),
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
    print(optimize(valves, ("AA", "AA"), time_left=26))
