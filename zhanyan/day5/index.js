/**
 * AOC Day 5
 * Solution
 */

import * as fs from 'node:fs/promises';

// 2-index: path to node is first argument, path to this file second & finally the input file third
if (process.argv.length <= 2) {
  console.log("Expected to be passed input file as first command line arg.");
  process.exit(1);
}

// split input into crate drawing & move commands
const lines = (await fs.readFile(process.argv[2])).toString().split("\n");
const splitAt = lines.indexOf("");
// -1: discard last commands line as it is empty
const [drawing, commands] = [lines.slice(0, splitAt), lines.slice(splitAt + 1, -1)];

// parse drawing into initial stack state
const columnPositions = [...drawing.at(-1)].map((key, idx) => [key, idx]).filter(([key, _]) => key !== " ");
const stacks = Object.fromEntries((columnPositions.map(([key, _]) => [key, []])));

for (const line of drawing.slice(0, -1).reverse()) {
  for (const [key, idx] of columnPositions) {
    if (line[idx] !== " ") {
      stacks[key].push(line[idx]);
    }
  }
}

// execute commmands on stacks
commands.map((cmd) => {
  // parse command
  const tokens = cmd.split(" ");
  return {
    count: Number.parseInt(tokens[1]),
    from: tokens[3],
    to: tokens[5],
  }
}).forEach(({ count, from, to }) => {
  stacks[to].push(...stacks[from].slice(-count));
  stacks[from] = stacks[from].slice(0, -count);
})

// print the top of stacks
console.log(Object.values(stacks).map(s => s.at(-1)).join(""));
