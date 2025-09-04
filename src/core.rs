use rustc_hash::FxHashMap as HashMap;

pub const RIGHT: &str = "R";
pub const LEFT: &str = "L";

/// Represents the direction the tape head can move.
#[derive(Clone, Debug, PartialEq)]
pub enum MoveDirection {
    Left,
    Right,
}

impl<'a> From<&'a str> for MoveDirection {
    fn from(s: &'a str) -> Self {
        match s {
            LEFT => MoveDirection::Left,
            RIGHT => MoveDirection::Right,
            _ => unreachable!(), // Validation should prevent this.
        }
    }
}

/// Custom error types for the Turing machine logic.
#[derive(Debug)]
pub enum Error {
    InvalidTransition(String),
    MissingTransition(String),
    InvalidSymbol(String),
    MaxStepsReached(u64),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidTransition(s) => write!(f, "InvalidTransition: {}", s),
            Error::MissingTransition(s) => write!(f, "MissingTransition: {}", s),
            Error::InvalidSymbol(s) => write!(f, "InvalidSymbol: {}", s),
            Error::MaxStepsReached(s) => write!(f, "MaxStepsReached: {}", s),
        }
    }
}

impl std::error::Error for Error {}

/// A high-performance Turing machine implementation in Rust.
pub struct LogicMill {
    pub transitions: HashMap<String, HashMap<char, (String, char, MoveDirection)>>,
    pub initial_state: String,
    pub halt_state: String,
    pub blank_symbol: char,
    pub rule_frequency: HashMap<(String, char), u64>,
    pub tape: HashMap<i64, char>,
    pub head_position: i64,
    pub current_state: String,
}

impl LogicMill {
    /// Initialize the Turing Machine.
    pub fn new(
        transitions_list: Vec<(String, String, String, String, String)>,
        initial_state: &str,
        halt_state: &str,
        blank_symbol: char,
    ) -> Result<Self, Error> {
        let mut machine = LogicMill {
            transitions: HashMap::default(),
            initial_state: initial_state.to_string(),
            halt_state: halt_state.to_string(),
            blank_symbol,
            rule_frequency: HashMap::default(),
            tape: HashMap::default(),
            head_position: 0,
            current_state: "".to_string(),
        };
        machine.transitions = machine.parse_transitions_list(transitions_list)?;
        machine.set_tape("")?;
        Ok(machine)
    }

    /// Perform a single step of the Turing machine's execution.
    pub fn step(&mut self) -> Result<(), Error> {
        let current_symbol = self
            .tape
            .get(&self.head_position)
            .cloned()
            .unwrap_or(self.blank_symbol);

        let state_transitions = self.transitions.get(&self.current_state).ok_or_else(|| {
            Error::MissingTransition(format!("No transitions for state {}", self.current_state))
        })?;

        let transition = state_transitions.get(&current_symbol).ok_or_else(|| {
            Error::MissingTransition(format!(
                "No transition for symbol '{}' in state {}",
                current_symbol, self.current_state
            ))
        })?;

        *self
            .rule_frequency
            .entry((self.current_state.clone(), current_symbol))
            .or_insert(0) += 1;

        let (new_state, new_symbol, move_direction) = transition;

        if *new_symbol == self.blank_symbol {
            self.tape.remove(&self.head_position);
        } else {
            self.tape.insert(self.head_position, *new_symbol);
        }

        self.current_state = new_state.clone();
        match move_direction {
            MoveDirection::Left => self.head_position -= 1,
            MoveDirection::Right => self.head_position += 1,
        }

        Ok(())
    }

    /// Reset the tape and state of the machine.
    pub fn set_tape(&mut self, input_tape: &str) -> Result<(), Error> {
        if input_tape.contains(' ') {
            return Err(Error::InvalidSymbol(
                "Input tape must not contain spaces".to_string(),
            ));
        }
        self.tape.clear();
        for (i, symbol) in input_tape.char_indices() {
            if symbol != self.blank_symbol {
                self.tape.insert(i as i64, symbol);
            }
        }
        self.head_position = 0;
        self.current_state = self.initial_state.clone();
        self.rule_frequency.clear();
        Ok(())
    }

    /// Render the significant part of the tape as a string.
    pub fn render_tape(&self) -> String {
        if self.tape.is_empty() {
            return "".to_string();
        }

        let min_pos = self.tape.keys().min().copied().unwrap_or(0);
        let max_pos = self.tape.keys().max().copied().unwrap_or(0);

        let tape_str: String = (min_pos..=max_pos)
            .map(|i| self.tape.get(&i).copied().unwrap_or(self.blank_symbol))
            .collect();

        tape_str.trim_matches(self.blank_symbol).to_string()
    }

    /// Return a list of unused transition rules.
    pub fn unused_rules(&self) -> Vec<(String, char)> {
        let mut unused = Vec::new();
        for (state, symbols) in &self.transitions {
            for symbol in symbols.keys() {
                if !self.rule_frequency.contains_key(&(state.clone(), *symbol)) {
                    unused.push((state.clone(), *symbol));
                }
            }
        }
        unused
    }
    
    /// Print the tape for debugging purposes.
    pub fn print_tape(&self) {
        let window = 20;
        let min_pos = self.head_position - window;
        let max_pos = self.head_position + window;

        let tape_view: String = (min_pos..=max_pos)
            .map(|i| self.tape.get(&i).copied().unwrap_or(self.blank_symbol))
            .collect();
        
        println!("{} \x1b[1m{}\x1b[0m", tape_view, self.current_state);
        println!("{}^", " ".repeat(window as usize));
    }


    fn validate_transition(
        &self,
        transition: &(String, String, String, String, String),
    ) -> Result<(String, char, String, char, MoveDirection), Error> {
        let (current_state, current_symbol_str, new_state, new_symbol_str, move_direction_str) =
            transition;

        if move_direction_str != LEFT && move_direction_str != RIGHT {
            return Err(Error::InvalidTransition(format!(
                "Invalid moveDirection: {}. Must be L or R",
                move_direction_str
            )));
        }

        let current_symbol = current_symbol_str.chars().next().ok_or_else(|| {
            Error::InvalidSymbol("Invalid current symbol: Must be a single character.".to_string())
        })?;
        if current_symbol_str.chars().count() != 1 {
            return Err(Error::InvalidSymbol(format!(
                "Invalid current symbol '{}'. Must be a single character.",
                current_symbol_str
            )));
        }

        let new_symbol = new_symbol_str.chars().next().ok_or_else(|| {
            Error::InvalidSymbol("Invalid new symbol: Must be a single character.".to_string())
        })?;
        if new_symbol_str.chars().count() != 1 {
            return Err(Error::InvalidSymbol(format!(
                "Invalid new symbol '{}'. Must be a single character.",
                new_symbol_str
            )));
        }

        Ok((
            current_state.clone(),
            current_symbol,
            new_state.clone(),
            new_symbol,
            move_direction_str.as_str().into(),
        ))
    }

    fn parse_transitions_list(
        &self,
        transitions_list: Vec<(String, String, String, String, String)>,
    ) -> Result<HashMap<String, HashMap<char, (String, char, MoveDirection)>>, Error> {
        let mut transitions = HashMap::default();
        let mut has_halt_state = false;

        for transition_tuple in transitions_list {
            let (current_state, current_symbol, new_state, new_symbol, move_direction) =
                self.validate_transition(&transition_tuple)?;

            let state_map: &mut HashMap<char, (String, char, MoveDirection)> =
                transitions.entry(current_state.clone()).or_default();

            if state_map.contains_key(&current_symbol) {
                return Err(Error::InvalidTransition(format!(
                    "Duplicate transition for state {} and symbol {}",
                    current_state, current_symbol
                )));
            }

            state_map.insert(
                current_symbol,
                (new_state.clone(), new_symbol, move_direction),
            );

            if new_state == self.halt_state {
                has_halt_state = true;
            }
        }

        if !transitions.contains_key(&self.initial_state) {
            return Err(Error::InvalidTransition(format!(
                "Initial state {} not found in the transitions",
                self.initial_state
            )));
        }

        if !has_halt_state {
            return Err(Error::InvalidTransition(format!(
                "Halt state {} not found in the transitions",
                self.halt_state
            )));
        }

        Ok(transitions)
    }

    
    pub fn run(
        &mut self,
        input_tape: String,
        max_steps: u64,
        verbose: bool,
    ) -> Result<(String, u64), Error> {
        self.set_tape(&input_tape)?;

        if verbose {
            self.print_tape();
        }

        for steps_count in 0..max_steps {
            if self.current_state == self.halt_state {
                if verbose {
                    println!("HALTED after {} steps", steps_count);
                }
                return Ok((self.render_tape(), steps_count));
            }

            self.step()?;

            if verbose {
                self.print_tape();
            }
        }

        Err(Error::MaxStepsReached(max_steps))
    }
}


pub fn parse_transition_rules(
    transition_rules_str: &str,
) -> Result<Vec<(String, String, String, String, String)>, Error> {
    const COMMENT_PREFIX: &str = "//";
    let mut transitions_list = Vec::new();

    for raw_line in transition_rules_str.lines() {
        let line = raw_line.trim();
        if line.is_empty() || line.starts_with(COMMENT_PREFIX) {
            continue;
        }

        let line_without_comment = line.split(COMMENT_PREFIX).next().unwrap_or("").trim();

        let values: Vec<String> = line_without_comment
            .split_whitespace()
            .map(String::from)
            .collect();

        if values.len() == 5 {
            transitions_list.push((
                values[0].clone(),
                values[1].clone(),
                values[2].clone(),
                values[3].clone(),
                values[4].clone(),
            ));
        } else if !values.is_empty() {
            return Err(Error::InvalidTransition(format!(
                "Invalid transition format: expected 5 parts, found {}",
                values.len()
            )));
        }
    }
    Ok(transitions_list)
}
