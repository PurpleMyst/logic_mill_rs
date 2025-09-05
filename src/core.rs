use rustc_hash::FxHashMap as HashMap;

pub const RIGHT: &str = "R";
pub const LEFT: &str = "L";

#[derive(Clone, Copy, Debug, PartialEq)]
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

// No changes to Error enum
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
            Error::InvalidTransition(s) => write!(f, "InvalidTransition: {s}"),
            Error::MissingTransition(s) => write!(f, "MissingTransition: {s}"),
            Error::InvalidSymbol(s) => write!(f, "InvalidSymbol: {s}"),
            Error::MaxStepsReached(s) => write!(f, "MaxStepsReached: {s}"),
        }
    }
}

impl std::error::Error for Error {}

type StateId = u16;
type SymbolId = u16;

const BLANK_SYMBOL_ID: SymbolId = 0;

/// A high-performance Turing machine implementation in Rust.
///
/// ### Further Optimization Notes
/// Knowing the max number of states is 1024 allows us to replace the main HashMap
/// with a Vec, using the state ID as a direct index. This is the fastest possible
/// lookup method, eliminating integer hashing and improving cache locality.
pub struct LogicMill {
    transitions: Vec<Vec<Option<(StateId, SymbolId, MoveDirection)>>>,
    initial_state: StateId,
    halt_state: StateId,
    rules_used: Vec<Vec<bool>>,
    tape: HashMap<i64, SymbolId>,
    head_position: i64,
    current_state: StateId,

    state_interner: Vec<String>,
    state_map: HashMap<String, StateId>,
    symbol_interner: Vec<char>,
    symbol_map: HashMap<char, SymbolId>,
}

impl LogicMill {
    /// Initialize the Turing Machine.
    pub fn new(
        transitions_list: Vec<(String, String, String, String, String)>,
        initial_state: &str,
        halt_state: &str,
        blank_symbol: char,
    ) -> Result<Self, Error> {
        let mut machine = Self {
            transitions: Default::default(),
            initial_state: 0,
            halt_state: 0,
            rules_used: Default::default(),
            tape: Default::default(),
            head_position: 0,
            current_state: 0,
            state_interner: Default::default(),
            state_map: Default::default(),
            symbol_interner: vec![blank_symbol],
            symbol_map: {
                let mut map = HashMap::default();
                map.insert(blank_symbol, 0);
                map
            },
        };

        machine.parse_transitions_list(transitions_list)?;

        // let num_states = machine.state_interner.len();
        // machine.rules_used = vec![Default::default(); num_states];
        machine.rules_used = vec![Vec::new(); machine.state_interner.len()];
        for (state_id, symbols) in machine.transitions.iter().enumerate() {
            machine.rules_used[state_id].resize(symbols.len(), false);
        }

        machine.initial_state = *machine.state_map.get(initial_state).ok_or_else(|| {
            Error::InvalidTransition(format!("Initial state '{initial_state}' not found in transitions"))
        })?;
        machine.halt_state = *machine.state_map.get(halt_state).ok_or_else(|| {
            Error::InvalidTransition(format!(
                "Halt state '{halt_state}' not found as a destination state in transitions"
            ))
        })?;

        machine.set_tape("")?;
        Ok(machine)
    }

    /// Perform a single step of the Turing machine's execution.
    #[inline] // Suggest inlining this critical function.
    pub fn step(&mut self) -> Result<(), Error> {
        let current_symbol = self.tape.get(&self.head_position).copied().unwrap_or(BLANK_SYMBOL_ID);

        // OPTIMIZATION: Direct slice indexing. This is faster than any hash map lookup.
        // It's safe because all state IDs are guaranteed to be valid indices.
        let state_transitions = &self.transitions[self.current_state as usize];

        let transition = state_transitions
            .get(current_symbol as usize)
            .copied()
            .flatten()
            .ok_or_else(|| {
                let state_name = &self.state_interner[self.current_state as usize];
                Error::MissingTransition(format!(
                    "No transition for symbol '{current_symbol}' in state {state_name}"
                ))
            })?;

        let (new_state_id, new_symbol, move_direction) = transition;

        self.rules_used[self.current_state as usize][current_symbol as usize] = true;

        if new_symbol == BLANK_SYMBOL_ID {
            self.tape.remove(&self.head_position);
        } else {
            self.tape.insert(self.head_position, new_symbol);
        }

        self.current_state = new_state_id;
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
            let symbol = self.get_or_intern_symbol(symbol)?;
            if symbol != BLANK_SYMBOL_ID {
                self.tape.insert(i as i64, symbol);
            }
        }
        self.head_position = 0;
        self.current_state = self.initial_state;

        for freq_map in &mut self.rules_used {
            freq_map.iter_mut().for_each(|b| *b = false);
        }
        Ok(())
    }

    // Unchanged public methods like render_tape, unused_rules, print_tape, run
    // remain compatible but benefit from the faster internal structures.

    pub fn render_tape(&self) -> String {
        if self.tape.is_empty() {
            return String::new();
        }
        let (min_pos, max_pos) = self
            .tape
            .keys()
            .fold((i64::MAX, i64::MIN), |(min, max), &val| (min.min(val), max.max(val)));
        if min_pos > max_pos {
            return String::new();
        }
        let tape_str: String = (min_pos..=max_pos)
            .map(|i| self.tape.get(&i).copied().unwrap_or(BLANK_SYMBOL_ID))
            .map(|symbol_id| self.symbol_interner[symbol_id as usize])
            .collect();
        tape_str
            .trim_matches(self.symbol_interner[BLANK_SYMBOL_ID as usize])
            .to_string()
    }

    pub fn unused_rules(&self) -> Vec<(String, char)> {
        let mut unused = Vec::new();
        for (state_id, symbols) in self.transitions.iter().enumerate() {
            for &(_, symbol, _) in symbols.iter().flatten() {
                if !self.rules_used[state_id][symbol as usize] {
                    unused.push((
                        self.state_interner[state_id].clone(),
                        self.symbol_interner[symbol as usize],
                    ));
                }
            }
        }
        unused
    }

    pub fn print_tape(&self) {
        let window = 20;
        let min_pos = self.head_position - window;
        let max_pos = self.head_position + window;
        let tape_view: String = (min_pos..=max_pos)
            .map(|i| self.tape.get(&i).copied().unwrap_or(BLANK_SYMBOL_ID))
            .map(|symbol_id| self.symbol_interner[symbol_id as usize])
            .collect();
        println!(
            "{} \x1b[1m{}\x1b[0m",
            tape_view, self.state_interner[self.current_state as usize]
        );
        println!("{}^", " ".repeat(window as usize));
    }

    pub fn run(&mut self, input_tape: String, max_steps: u64, verbose: bool) -> Result<(String, u64), Error> {
        self.set_tape(&input_tape)?;
        if verbose {
            self.print_tape();
        }
        for steps_count in 0..max_steps {
            if self.current_state == self.halt_state {
                if verbose {
                    println!("HALTED after {steps_count} steps");
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

    // --- Private Helper Methods ---

    fn get_or_intern_state(&mut self, state: &str) -> Result<StateId, Error> {
        if let Some(&id) = self.state_map.get(state) {
            Ok(id)
        } else {
            let id = self.state_interner.len() as u16;
            if id >= 1024 {
                return Err(Error::InvalidTransition(format!(
                    "Exceeded the maximum of 1024 unique states. State '{state}' is the 1025th."
                )));
            }
            self.state_interner.push(state.to_string());
            self.state_map.insert(state.to_string(), id);
            Ok(id)
        }
    }

    fn get_or_intern_symbol(&mut self, symbol: char) -> Result<SymbolId, Error> {
        if let Some(&id) = self.symbol_map.get(&symbol) {
            Ok(id)
        } else {
            let id = u16::try_from(self.symbol_interner.len())
                .map_err(|_| Error::InvalidSymbol("Exceeded the maximum of 65536 unique symbols.".to_string()))?;
            self.symbol_interner.push(symbol);
            self.symbol_map.insert(symbol, id);
            Ok(id)
        }
    }

    fn validate_and_parse_transition(
        &mut self,
        transition: &(String, String, String, String, String),
    ) -> Result<(u16, char, u16, char, MoveDirection), Error> {
        let (current_state, current_symbol_str, new_state, new_symbol_str, move_direction_str) = transition;
        if move_direction_str != LEFT && move_direction_str != RIGHT {
            return Err(Error::InvalidTransition(format!(
                "Invalid moveDirection: {move_direction_str}"
            )));
        }
        let current_symbol = current_symbol_str
            .chars()
            .next()
            .ok_or_else(|| Error::InvalidSymbol("Current symbol must be a single character.".to_string()))?;
        if current_symbol_str.chars().count() != 1 {
            return Err(Error::InvalidSymbol(format!(
                "Invalid current symbol '{current_symbol_str}'"
            )));
        }
        let new_symbol = new_symbol_str
            .chars()
            .next()
            .ok_or_else(|| Error::InvalidSymbol("New symbol must be a single character.".to_string()))?;
        if new_symbol_str.chars().count() != 1 {
            return Err(Error::InvalidSymbol(format!("Invalid new symbol '{new_symbol_str}'")));
        }

        let current_state_id = self.get_or_intern_state(current_state)?;
        let new_state_id = self.get_or_intern_state(new_state)?;

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
    ) -> Result<(), Error> {
        // Use a temporary map during parsing because we don't know the final number of states yet.
        let mut temp_transitions = HashMap::<u16, Vec<Option<(StateId, SymbolId, MoveDirection)>>>::default();

        for transition_tuple in transitions_list {
            let (current_state_id, current_symbol, new_state_id, new_symbol, move_direction) =
                self.validate_and_parse_transition(&transition_tuple)?;
            let current_symbol = self.get_or_intern_symbol(current_symbol)?;
            let new_symbol = self.get_or_intern_symbol(new_symbol)?;

            let state_map = temp_transitions.entry(current_state_id).or_default();

            if state_map.get(current_symbol as usize).copied().flatten().is_some() {
                return Err(Error::InvalidTransition(format!(
                    "Duplicate transition for state {} and symbol {}",
                    self.state_interner[current_state_id as usize], current_symbol
                )));
            }

            // state_map.insert(current_symbol, (new_state_id, new_symbol, move_direction));
            if (state_map.len() as u16) <= current_symbol {
                state_map.resize((current_symbol + 1) as usize, None);
            }
            state_map[current_symbol as usize] = Some((new_state_id, new_symbol, move_direction));
        }

        // OPTIMIZATION: Convert the temporary map to the final, faster Vec structure.
        let num_states = self.state_interner.len();
        self.transitions = vec![Default::default(); num_states];
        for (state_id, transition_map) in temp_transitions {
            self.transitions[state_id as usize] = transition_map;
        }

        Ok(())
    }
}

// No changes to parse_transition_rules
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_totally_unknown_symbol_in_tape() {
        let transitions = vec![(
            "INIT".to_string(),
            "a".to_string(),
            "HALT".to_string(),
            "b".to_string(),
            "R".to_string(),
        )];
        let mut machine = LogicMill::new(transitions, "INIT", "HALT", '_').unwrap();
        let result = machine.run("x".to_string(), 100, false);
        assert!(matches!(result, Err(Error::MissingTransition(_))));
    }
}
