use rustc_hash::FxHashMap as HashMap;

pub const RIGHT: &str = "R";
pub const LEFT: &str = "L";

/// Represents the direction the tape head can move. (No changes)
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
            _ => unreachable!(),
        }
    }
}

/// Custom error types for the Turing machine logic. (No changes)
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
///
/// ### Optimization Notes
/// The fields of this struct have been made private to enable crucial internal optimizations.
/// The primary optimization is "string interning," where string-based states are mapped to
/// integer IDs (`usize`). This eliminates expensive string hashing and cloning in the main
/// execution loop, providing a significant performance boost. The public API through methods
/// remains 100% compatible.
pub struct LogicMill {
    // OPTIMIZATION: Internal data structures now use `usize` for states.
    transitions: HashMap<usize, HashMap<char, (usize, char, MoveDirection)>>,
    initial_state: usize,
    halt_state: usize,
    blank_symbol: char,
    rule_frequency: HashMap<(usize, char), u64>,
    tape: HashMap<i64, char>,
    head_position: i64,
    current_state: usize,

    // OPTIMIZATION: Added interning structures to map state strings to usize IDs.
    state_interner: Vec<String>,
    state_map: HashMap<String, usize>,
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
            initial_state: 0, // Placeholder
            halt_state: 0,    // Placeholder
            blank_symbol,
            rule_frequency: HashMap::default(),
            tape: HashMap::default(),
            head_position: 0,
            current_state: 0, // Placeholder
            state_interner: Vec::new(),
            state_map: HashMap::default(),
        };

        // OPTIMIZATION: Parsing now populates the interner and uses IDs.
        machine.transitions = machine.parse_transitions_list(transitions_list)?;

        machine.initial_state = *machine.state_map.get(initial_state).ok_or_else(|| {
            Error::InvalidTransition(format!("Initial state '{}' not found in transitions", initial_state))
        })?;
        machine.halt_state = *machine.state_map.get(halt_state).ok_or_else(|| {
            Error::InvalidTransition(format!(
                "Halt state '{}' not found as a destination state in transitions",
                halt_state
            ))
        })?;

        machine.set_tape("")?;
        Ok(machine)
    }

    /// Perform a single step of the Turing machine's execution.
    pub fn step(&mut self) -> Result<(), Error> {
        let current_symbol = self.tape.get(&self.head_position).copied().unwrap_or(self.blank_symbol);

        // OPTIMIZATION: Lightning-fast lookup using a `usize` key. No string hashing.
        let state_transitions = self.transitions.get(&self.current_state).ok_or_else(|| {
            // Get the string representation for a user-friendly error message.
            let state_name = &self.state_interner[self.current_state];
            Error::MissingTransition(format!("No transitions for state {}", state_name))
        })?;

        let transition = state_transitions.get(&current_symbol).ok_or_else(|| {
            let state_name = &self.state_interner[self.current_state];
            Error::MissingTransition(format!(
                "No transition for symbol '{}' in state {}",
                current_symbol, state_name
            ))
        })?;

        let (new_state_id, new_symbol, move_direction) = transition;

        // OPTIMIZATION: No `String::clone()`. The key is a simple, copyable `(usize, char)`.
        *self
            .rule_frequency
            .entry((self.current_state, current_symbol))
            .or_insert(0) += 1;

        if *new_symbol == self.blank_symbol {
            self.tape.remove(&self.head_position);
        } else {
            self.tape.insert(self.head_position, *new_symbol);
        }

        // OPTIMIZATION: No `String::clone()`. This is a cheap integer copy.
        self.current_state = *new_state_id;

        match move_direction {
            MoveDirection::Left => self.head_position -= 1,
            MoveDirection::Right => self.head_position += 1,
        }

        Ok(())
    }

    /// Reset the tape and state of the machine.
    pub fn set_tape(&mut self, input_tape: &str) -> Result<(), Error> {
        if input_tape.contains(' ') {
            return Err(Error::InvalidSymbol("Input tape must not contain spaces".to_string()));
        }
        self.tape.clear();
        for (i, symbol) in input_tape.char_indices() {
            if symbol != self.blank_symbol {
                self.tape.insert(i as i64, symbol);
            }
        }
        self.head_position = 0;
        // OPTIMIZATION: Reset state using the stored initial state ID.
        self.current_state = self.initial_state;
        self.rule_frequency.clear();
        Ok(())
    }

    /// Render the significant part of the tape as a string.
    pub fn render_tape(&self) -> String {
        if self.tape.is_empty() {
            return String::new();
        }

        // OPTIMIZATION: Find min and max in a single pass.
        let (min_pos, max_pos) = self
            .tape
            .keys()
            .fold((i64::MAX, i64::MIN), |(min, max), &val| (min.min(val), max.max(val)));

        if min_pos > max_pos {
            return String::new();
        }

        let tape_str: String = (min_pos..=max_pos)
            .map(|i| self.tape.get(&i).copied().unwrap_or(self.blank_symbol))
            .collect();

        tape_str.trim_matches(self.blank_symbol).to_string()
    }

    /// Return a list of unused transition rules.
    pub fn unused_rules(&self) -> Vec<(String, char)> {
        let mut unused = Vec::new();
        // OPTIMIZATION: Iterate over integer IDs, not strings.
        for (state_id, symbols) in &self.transitions {
            for symbol in symbols.keys() {
                // OPTIMIZATION: Fast lookup using a `(usize, char)` key. No string cloning needed.
                if !self.rule_frequency.contains_key(&(*state_id, *symbol)) {
                    // Convert ID back to String only for the final output, preserving the API.
                    unused.push((self.state_interner[*state_id].clone(), *symbol));
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

        // Convert state ID back to string for printing.
        println!(
            "{} \x1b[1m{}\x1b[0m",
            tape_view, self.state_interner[self.current_state]
        );
        println!("{}^", " ".repeat(window as usize));
    }

    // This helper function is not part of the public API.
    fn get_or_intern_state(&mut self, state: &str) -> usize {
        if let Some(id) = self.state_map.get(state) {
            *id
        } else {
            let id = self.state_interner.len();
            self.state_interner.push(state.to_string());
            self.state_map.insert(state.to_string(), id);
            id
        }
    }

    // Validation is now part of the parsing process and can be private.
    fn validate_and_parse_transition(
        &mut self,
        transition: &(String, String, String, String, String),
    ) -> Result<(usize, char, usize, char, MoveDirection), Error> {
        let (current_state, current_symbol_str, new_state, new_symbol_str, move_direction_str) = transition;

        if move_direction_str != LEFT && move_direction_str != RIGHT {
            return Err(Error::InvalidTransition(format!(
                "Invalid moveDirection: {}. Must be L or R",
                move_direction_str
            )));
        }

        let current_symbol = current_symbol_str
            .chars()
            .next()
            .ok_or_else(|| Error::InvalidSymbol("Invalid current symbol: Must be a single character.".to_string()))?;
        if current_symbol_str.chars().count() != 1 {
            return Err(Error::InvalidSymbol(format!(
                "Invalid current symbol '{}'. Must be a single character.",
                current_symbol_str
            )));
        }

        let new_symbol = new_symbol_str
            .chars()
            .next()
            .ok_or_else(|| Error::InvalidSymbol("Invalid new symbol: Must be a single character.".to_string()))?;
        if new_symbol_str.chars().count() != 1 {
            return Err(Error::InvalidSymbol(format!(
                "Invalid new symbol '{}'. Must be a single character.",
                new_symbol_str
            )));
        }

        // OPTIMIZATION: Intern the state strings into usize IDs.
        let current_state_id = self.get_or_intern_state(current_state);
        let new_state_id = self.get_or_intern_state(new_state);

        Ok((
            current_state_id,
            current_symbol,
            new_state_id,
            new_symbol,
            move_direction_str.as_str().into(),
        ))
    }

    fn parse_transitions_list(
        &mut self,
        transitions_list: Vec<(String, String, String, String, String)>,
    ) -> Result<HashMap<usize, HashMap<char, (usize, char, MoveDirection)>>, Error> {
        let mut transitions = HashMap::default();

        for transition_tuple in transitions_list {
            let (current_state_id, current_symbol, new_state_id, new_symbol, move_direction) =
                self.validate_and_parse_transition(&transition_tuple)?;

            let state_map: &mut HashMap<char, (usize, char, MoveDirection)> =
                transitions.entry(current_state_id).or_default();

            if state_map.contains_key(&current_symbol) {
                return Err(Error::InvalidTransition(format!(
                    "Duplicate transition for state {} and symbol {}",
                    self.state_interner[current_state_id], current_symbol
                )));
            }

            state_map.insert(current_symbol, (new_state_id, new_symbol, move_direction));
        }

        Ok(transitions)
    }

    pub fn run(&mut self, input_tape: String, max_steps: u64, verbose: bool) -> Result<(String, u64), Error> {
        self.set_tape(&input_tape)?;

        if verbose {
            self.print_tape();
        }

        for steps_count in 0..max_steps {
            // OPTIMIZATION: Compare integer IDs, which is much faster than comparing strings.
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

/// Parses transition rules from a string into a structured Vec.
/// (No significant changes needed, this is not performance-critical)
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

        let values: Vec<&str> = line_without_comment.split_whitespace().collect();

        if values.len() == 5 {
            // Using `to_owned` or `to_string` is fine here as it's part of the initial setup.
            transitions_list.push((
                values[0].to_owned(),
                values[1].to_owned(),
                values[2].to_owned(),
                values[3].to_owned(),
                values[4].to_owned(),
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
