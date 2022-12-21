/*
 * Advent of Code
 * Day 10
*/

use std::{env::args, fs::File, io::Read};

// Instructions the CPU can execute
enum Instruction {
    Addx(i32),
    Noop,
}

// Snapshot of CPU's state at a specific time on the CPU's monotonic clock
#[derive(Debug)]
struct CPU {
    register_x: i32,
    time: i32,
}
impl CPU {
    // Execute the given instruction on the given CPU & return the next CPU state.
    fn execute(&self, instruction: &Instruction) -> Self {
        let &Self { register_x, time } = self;
        use Instruction::*;
        match instruction {
            // advances 2 clock cycles
            Addx(amount) => Self {
                register_x: register_x + amount,
                time: time + 2,
            },
            // advances 1 clock cycle
            Noop => Self {
                register_x,
                time: time + 1,
            },
        }
    }
}

// Find & return the active state at the given point in time
fn find_active(states: &[CPU], time: i32) -> &CPU {
    states
        .into_iter()
        .filter(|&state| state.time < time)
        .max_by_key(|&state| state.time)
        .expect(&format!("No state found at time: {}ms", time))
}

fn main() {
    // parse input file
    let argv: Vec<_> = args().collect();
    if argv.len() < 2 {
        panic!("Expected input file to be given with command line arg.");
    }

    let mut input = String::new();
    File::open(&argv[1])
        .expect("Failed to open input file.")
        .read_to_string(&mut input)
        .expect("Failed to read input file.");

    // parse input lines as instructions
    let instructions = input.lines().map(|line| {
        let tokens: Vec<&str> = line.split_whitespace().collect();
        use Instruction::*;
        match tokens[0] {
            "addx" => Addx(
                tokens[1]
                    .parse()
                    .expect("'addx' instruction expects to be given integer to add."),
            ),
            "noop" => Noop,
            _ => panic!("Unsuported instruction: {}", tokens[0]),
        }
    });

    // execute instructions and track CPU state
    let cpu_states = instructions.fold(
        vec![CPU {
            register_x: 1,
            time: 0,
        }],
        |mut states, instruction| {
            let previous = states.last().unwrap();
            states.push(previous.execute(&instruction));
            return states;
        },
    );

    // render pixels on the CRT
    let pixels = (1..240)
        .into_iter()
        .map(|draw_time| {
            // calculate pixel's horizontal position
            let pixel = (draw_time - 1) % 40;
            // sprite spans register_x - 1 to register_x + 1
            // check for intersection to determine if pixel has been drawn
            let register_x = find_active(&cpu_states, draw_time).register_x;
            if pixel >= (register_x - 1) && pixel <= (register_x + 1) {
                "#"
            } else {
                "."
            }
        })
        .collect::<Vec<_>>();
    // chunk pixels into rows & draw!
    println!(
        "{}",
        pixels
            .chunks(40)
            .map(|chunk| chunk.concat())
            .collect::<Vec<_>>()
            .join("\n")
    );
}
