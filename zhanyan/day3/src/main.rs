/*
 * Advent of Code
 * Day 3
*/

use std::collections::HashSet;
use std::error::Error;
use std::fmt::Display;
use std::io::{self, Read};
use std::{fs::File, path::Path};

fn read_string(path: &Path) -> Result<String, io::Error> {
    let mut input_txt = File::open(path)?;
    let mut input = String::new();
    input_txt.read_to_string(&mut input)?;
    return Ok(input);
}

#[derive(Debug)]
struct UnknownItemError {
    message: String,
}
impl UnknownItemError {
    pub fn new(item: char) -> Self {
        Self {
            message: format!("No such item: {}", item),
        }
    }
}
impl Display for UnknownItemError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}
impl Error for UnknownItemError {
    fn description(&self) -> &str {
        &self.message
    }
}

fn priority(item: char) -> Result<i32, UnknownItemError> {
    match item {
        // a-z is represented in ascii as 97-122, shift to 1-26
        item if item.is_ascii_lowercase() => Ok((item as i32) - 96),
        // A-Z is represented in ascii as 65-90, shift to 27-52
        item if item.is_ascii_uppercase() => Ok((item as i32) - 38),
        _ => Err(UnknownItemError::new(item)),
    }
}

fn main() {
    let priority_sum: i32 = read_string(Path::new("input.txt"))
        .expect("Failed to read input.txt")
        .lines()
        .collect::<Vec<_>>()
        // inspect chunks of 3 lines for common item type per elf group
        .chunks(3)
        .map(|lines| {
            let duplicate = lines
                .to_vec()
                .into_iter()
                .map(|line| line.chars().collect::<HashSet<char>>())
                .reduce(|acc, next| acc.intersection(&next).map(|&c| c).collect::<HashSet<_>>())
                .unwrap();
            duplicate
                .into_iter()
                .next()
                .map_or(0, |c| priority(c).unwrap())
        })
        .sum();
    println!("Priority sum: {}", priority_sum)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn priority_test() {
        assert_eq!(priority('a').unwrap(), 1);
        assert_eq!(priority('b').unwrap(), 2);
        assert_eq!(priority('z').unwrap(), 26);
        assert_eq!(priority('A').unwrap(), 27);
        assert_eq!(priority('B').unwrap(), 28);
        assert_eq!(priority('Z').unwrap(), 52);
    }
}
