#
# Advent of Code
# Day 2
#

from enum import Enum


class Result(Enum):
    Win = 1
    Lose = 2
    Draw = 3


class Move(Enum):
    """Encodes a Rock Paper Scissors move its inherent score."""

    Rock = 1
    Paper = 2
    Scissors = 3


def match(you: Move, opponent: Move) -> Result:
    """Evaluate the result of match between you & opponent's moves."""
    if you == opponent:
        return Result.Draw
    if you == Move.Rock and opponent == Move.Scissors:
        return Result.Win
    if you == Move.Paper and opponent == Move.Rock:
        return Result.Win
    if you == Move.Scissors and opponent == Move.Paper:
        return Result.Win
    return Result.Lose


def move(opponent: Move, expected: Result) -> Move:
    """Determine the move needed to attain the expected result"""
    move_results = {
        match(you, opponent): you for you in [Move.Scissors, Move.Paper, Move.Rock]
    }
    return move_results[expected]


def score(you: Move, opponent: Move) -> int:
    """Calculate the score of a match between you & opponent's moves."""
    result = match(you, opponent)
    if result == Result.Win:
        return you.value + 6
    if result == Result.Draw:
        return you.value + 3
    return you.value


if __name__ == "__main__":
    # read strategy guide fom input
    with open("input.txt", "r") as f:
        codes = [line.split() for line in f.readlines()]

    # decode strategy guide
    code_map = {
        "A": Move.Rock,
        "B": Move.Paper,
        "C": Move.Scissors,
        "X": Result.Lose,
        "Y": Result.Draw,
        "Z": Result.Win,
    }

    games = []
    for opponent_key, result_key in codes:
        opponent, expected = code_map[opponent_key], code_map[result_key]
        # calculate our move to achieve desired result
        you = move(opponent, expected)
        games.append((you, opponent))

    print("score: ", sum([score(*g) for g in games]))
