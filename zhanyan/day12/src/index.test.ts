/**
 * Advent of Code
 * Day 11
 * Tests
 */

import { describe, expect, test } from '@jest/globals';
import solve, { Graph } from './index'


describe("Graph", () => {
  test("constructor()", () => {
    const g = new Graph([
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 9],
    ]);
    expect(g.at({ row: 1, column: 1 }).height).toBe(5);
    expect(g.at({ row: 0, column: 1 }).incoming.map(p => g.at(p).height).sort())
      .toEqual([1, 3, 5]);
    expect(g.at({ row: 1, column: 1 }).incoming.map(p => g.at(p).height).sort())
      .toEqual([4, 6, 8]);
    expect(g.at({ row: 2, column: 2 }).incoming.map(p => g.at(p).height).sort())
      .toEqual([8]);
  });

  test("distances()", () => {
    const g = new Graph([
      [1, 2, 1],
      [2, 3, 4],
      [1, 2, 5],
    ]);
    expect(g.distances([
      { row: 0, column: 0 },
      { row: 0, column: 2 },
      { row: 2, column: 1 },
      { row: 1, column: 2 },
    ], { row: 2, column: 2 })).toBe(1);
  });
});

describe("solve()", () => {
  ["input.txt"].map((path) => {
    test(`solve(${path})`, async () => {
      await solve(path);
    });
  })
});
