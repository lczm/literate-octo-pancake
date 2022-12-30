/*
 * Advent of Code
 * Day 12 
*/

import fs from 'node:fs/promises';

type Point = {
  row: number,
  column: number
}

type Vertex = {
  height: number,
  // locations of incoming vertices
  incoming: Point[];
}

export class Graph {
  vertices: Vertex[][];

  constructor(heights: number[][]) {
    const [maxRow, maxColumn] = [heights.length, heights[0].length];
    this.vertices = heights.map((row, r) =>
      row.map((height, c) => {
        const incoming = [
          // top
          (r > 0) ? { row: r - 1, column: c } : null,
          // right
          (c + 1 < maxColumn) ? { row: r, column: c + 1 } : null,
          // bottom
          (r + 1 < maxRow) ? { row: r + 1, column: c } : null,
          // left
          (c > 0) ? { row: r, column: c - 1 } : null,
        ].filter(point => {
          if (point != null) {
            const { row, column } = point;
            // max +1 one elevation for connected vertices
            return height - heights[row][column] <= 1;
          }
          return false;
        }) as Point[];

        return { height, incoming };
      })
    );
  }

  at({ row, column }: Point): Vertex {
    return this.vertices[row][column];
  }

  /**
   * Find shortest distance between starting & end points
   * Returns Infinity if no path connects start & end points
   */
  distances(starts: Point[], end: Point): number {
    // breath first search from end -> start
    const traversed = new Set<Point>();
    const unexplored: [Point, number][] = [[end, 0]];
    const pathDistances: number[] = [];

    while (unexplored.length > 0) {
      const [current, nMoves] = unexplored.shift() as [Point, number];
      // skip if alredy traversed
      if (traversed.has(current)) {
        continue;
      }

      traversed.add(current);
      for (const start of starts) {
        if (current.row === start.row && current.column === start.column) {
          // found path
          pathDistances.push(nMoves);
        }
      }

      unexplored.push(...this.at(current).incoming
        .map(p => [p, nMoves + 1] as [Point, number]));
    }

    return pathDistances.reduce((min, distance) => (distance < min) ? distance : min, Infinity);
  }
}

export default async function solve(inputPath: string) {
  const input = await fs.readFile(inputPath)
  // -1 - skip last line
  // treat "S" as lowest point
  const lines = input.toString().replace("S", "a").split("\n").slice(0, -1);
  const aCharCode = "a".charCodeAt(0);

  // scan input for heights
  const starts: Point[] = [];
  const end = { row: Infinity, column: Infinity };
  const heights = lines.map((line, r) =>
    [...line].map((char, c) => {
      // return start (if any), end (if any), height.
      switch (char) {
        case "E":
          [end.row, end.column] = [r, c];
          return "z".charCodeAt(0) - aCharCode;
        case "a":
          starts.push({ row: r, column: c });
          return 0;
        default:
          return char.charCodeAt(0) - aCharCode;
      }
    })
  );

  // parse graph from heights
  const graph = new Graph(heights);
  console.log(graph.distances(starts, end));
}
