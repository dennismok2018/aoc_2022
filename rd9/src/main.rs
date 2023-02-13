use std::collections::HashSet;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
struct Position {
    x: i32,
    y: i32,
}

impl Position {
    fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}

struct Tangle {
    tail: Position,
    relative_head: (i8, i8),
}

struct Instruction {
    direction: UnitVector,
    amount: usize,
}

impl Instruction {
    fn new(line: &str) -> Self {
        let binding = line.to_owned();
        let (left, right) = binding.split_once(' ').unwrap();
        let amount: usize = right.parse().unwrap();
        if left == "R" {
            Instruction {
                direction: UnitVector {
                    dimension: Dimension::X,
                    sign: Sign::Plus,
                },
                amount,
            }
        } else if left == "L" {
            Instruction {
                direction: UnitVector {
                    dimension: Dimension::X,
                    sign: Sign::Minus,
                },
                amount,
            }
        } else if left == "U" {
            Instruction {
                direction: UnitVector {
                    dimension: Dimension::Y,
                    sign: Sign::Plus,
                },
                amount,
            }
        } else {
            Instruction {
                direction: UnitVector {
                    dimension: Dimension::Y,
                    sign: Sign::Minus,
                },
                amount,
            }
        }
    }
}

#[derive(PartialEq, Eq, Clone)]
enum Sign {
    Plus,
    Minus,
}

#[derive(Clone)]
struct UnitVector {
    dimension: Dimension,
    sign: Sign,
}

#[derive(Clone)]
enum Dimension {
    X,
    Y,
}

impl Tangle {
    fn start() -> Self {
        Self {
            tail: Position::new(0, 0),
            relative_head: (0, 0),
        }
    }

    fn move_head(&mut self, unit_move: &UnitVector) -> Position {
        let xy = &unit_move.dimension;
        let sign = &unit_move.sign;
        match self.relative_head {
            (0, 0) | (0, -1) | (0, 1) | (1, 0) | (-1, 0) => match xy {
                Dimension::X => match sign {
                    Sign::Plus => {
                        self.relative_head.0 += 1;
                        if self.relative_head.0 == 2 {
                            self.tail.x += 1;
                            self.relative_head.0 -= 1;
                        }
                    }
                    Sign::Minus => {
                        self.relative_head.0 -= 1;
                        if self.relative_head.0 == -2 {
                            self.tail.x -= 1;
                            self.relative_head.0 += 1;
                        }
                    }
                },
                Dimension::Y => match sign {
                    Sign::Plus => {
                        self.relative_head.1 += 1;
                        if self.relative_head.1 == 2 {
                            self.tail.y += 1;
                            self.relative_head.1 -= 1;
                        }

                    }
                    Sign::Minus => {
                        self.relative_head.1 -= 1;
                        if self.relative_head.1 == -2 {
                            self.tail.y -= 1;
                            self.relative_head.1 += 1;
                        }
                    }
                },
            },
            (1, 1) | (1, -1) | (-1, -1) | (-1, 1) => match xy {
                Dimension::X => match sign {
                    Sign::Plus => {
                        self.relative_head.0 += 1;
                        if self.relative_head.0 == 2 {
                            self.tail.x += 1;
                            self.relative_head.0 -= 1;
                            if self.relative_head.1 == 1 {
                                self.tail.y += 1;
                            } else {
                                self.tail.y -= 1;
                            };
                            self.relative_head.1 = 0;
                        }
                    }
                    Sign::Minus => {
                        self.relative_head.0 -= 1;
                        if self.relative_head.0 == -2 {
                            self.tail.x -= 1;
                            self.relative_head.0 += 1;
                            if self.relative_head.1 == 1 {
                                self.tail.y += 1
                            } else {
                                self.tail.y -= 1
                            };
                            self.relative_head.1 = 0;
                        }
                    }
                },
                Dimension::Y => match sign {
                    Sign::Plus => {
                        self.relative_head.1 += 1;
                        if self.relative_head.1 == 2 {
                            self.tail.y += 1;
                            self.relative_head.1 -= 1;
                            if self.relative_head.0 == 1 {
                                self.tail.x += 1;
                            } else {
                                self.tail.x -= 1;
                            };
                            self.relative_head.0 = 0;
                        }

                    }
                    Sign::Minus => {
                        self.relative_head.1 -= 1;
                        if self.relative_head.1 == -2 {
                            self.tail.y -= 1;
                            self.relative_head.1 += 1;
                            if self.relative_head.0 == 1 {
                               self.tail.x += 1;

                            } else {
                               self.tail.x -= 1;
                            };
                            self.relative_head.0 = 0;
                        }
                    }
                },
            },
            _ => {
                panic!("Should not reach here")
            }
        }
        self.tail.clone()
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path;
    match args.get(1) {
        Some(arg) => {
            file_path = arg.to_string();
        }
        None => panic!("expect path for input file"),
    }
    let mut game = Tangle::start();
    let mut visited: HashSet<Position> = HashSet::new();
    visited.insert(Position { x: 0, y: 0 });
    let mut buf = String::new();
    if let Ok(mut file) = File::open(file_path) {
        file.read_to_string(&mut buf)
            .expect("Read txt file into String should be successful");
        buf.split('\n')
            .filter(|line| !line.is_empty())
            .map(|line| Instruction::new(line))
            .for_each(|instruction| {
                let unit = instruction.direction;
                for _ in 0..(instruction.amount) {
                    let new_tail = game.move_head(&unit);
                    let new = visited.insert(new_tail);
                    if !new {
                        println!("duplicated {:?}", game.tail.clone());
                    } else {
                        println!("new {:?}", game.tail.clone());
                    };
                }
            });
    }
    println!(
        "visited={:#?}",
        &visited.iter().take(10).collect::<Vec<_>>()
    );
    println!("Count is {:?}", visited.len());
}
