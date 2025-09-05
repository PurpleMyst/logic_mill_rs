use slab::Slab;

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
pub struct LogicMill {
    transitions: Vec<Vec<Option<(StateId, SymbolId, MoveDirection)>>>,
    initial_state: StateId,
    halt_state: StateId,
    rules_used: Vec<Vec<bool>>,

    right_tape: Vec<SymbolId>,
    left_tape: Vec<SymbolId>,

    head_position: i64,
    current_state: StateId,

    state_interner: Slab<String>,
    symbol_interner: Slab<char>,
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
            left_tape: Default::default(),
            right_tape: Default::default(),
            head_position: 0,
            current_state: 0,
            state_interner: Default::default(),
            symbol_interner: {
                let mut slab = Slab::new();
                // Reserve ID 0 for the blank symbol
                let blank_symbol_id = slab.insert(blank_symbol) as SymbolId;
                debug_assert_eq!(blank_symbol_id, BLANK_SYMBOL_ID);
                slab
            },
        };

        machine.parse_transitions_list(transitions_list)?;

        // let num_states = machine.state_interner.len();
        // machine.rules_used = vec![Default::default(); num_states];
        machine.rules_used = vec![Vec::new(); machine.state_interner.len()];
        for (state_id, symbols) in machine.transitions.iter().enumerate() {
            machine.rules_used[state_id].resize(symbols.len(), false);
        }

        machine.initial_state = machine
            .state_interner
            .iter()
            .find_map(|(id, name)| {
                if name == initial_state {
                    Some(id as StateId)
                } else {
                    None
                }
            })
            .ok_or_else(|| {
                Error::InvalidTransition(format!("Initial state '{initial_state}' not found in transitions"))
            })?;
        machine.halt_state = machine
            .state_interner
            .iter()
            .find_map(|(id, name)| if name == halt_state { Some(id as StateId) } else { None })
            .ok_or_else(|| {
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
        let (idx, current_symbol) = if self.right_tape.is_empty() && self.left_tape.is_empty() {
            (0, BLANK_SYMBOL_ID)
        } else if self.head_position < 0 {
            let idx = (-self.head_position - 1) as usize;
            (idx, self.left_tape[idx])
        } else {
            let idx = self.head_position as usize;
            (idx, self.right_tape[idx])
        };

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

        if self.head_position < 0 {
            self.left_tape[idx] = new_symbol;
        } else {
            if self.right_tape.is_empty() {
                assert_eq!(idx, 0, "Right tape is empty but idx is not 0");
                self.right_tape.push(BLANK_SYMBOL_ID);
            }
            self.right_tape[idx] = new_symbol;
        }

        self.current_state = new_state_id;
        match move_direction {
            MoveDirection::Left => self.head_position -= 1,
            MoveDirection::Right => self.head_position += 1,
        }
        if self.head_position < 0 {
            let left_idx = (-self.head_position - 1) as usize;
            if left_idx >= self.left_tape.len() {
                self.left_tape.push(BLANK_SYMBOL_ID);
            }
        } else {
            let right_idx = self.head_position as usize;
            if right_idx >= self.right_tape.len() {
                self.right_tape.push(BLANK_SYMBOL_ID);
            }
        }

        Ok(())
    }

    /// Reset the tape and state of the machine.
    pub fn set_tape(&mut self, input_tape: &str) -> Result<(), Error> {
        if input_tape.contains(' ') {
            return Err(Error::InvalidSymbol("Input tape must not contain spaces".to_string()));
        }
        self.left_tape.clear();
        self.right_tape = input_tape
            .chars()
            .map(|c| self.get_or_intern_symbol(c))
            .collect::<Result<Vec<_>, _>>()?;
        self.head_position = 0;
        self.current_state = self.initial_state;

        for freq_map in &mut self.rules_used {
            freq_map.iter_mut().for_each(|b| *b = false);
        }
        Ok(())
    }

    /// Get the number of states in the machine.
    pub fn state_count(&self) -> usize {
        self.state_interner.len()
    }

    pub fn render_tape(&self) -> String {
        if self.left_tape.is_empty() && self.right_tape.is_empty() {
            return String::new();
        }

        let mut tape_view = String::with_capacity(self.left_tape.len() + self.right_tape.len());
        for &symbol_id in self.left_tape.iter().rev() {
            tape_view.push(self.symbol_interner[symbol_id as usize]);
        }
        for &symbol_id in &self.right_tape {
            tape_view.push(self.symbol_interner[symbol_id as usize]);
        }

        tape_view
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
        let mut tape_view = String::with_capacity((max_pos - min_pos + 1) as usize);
        for i in min_pos..=max_pos {
            let symbol_id = if i < 0 {
                let idx = (-i - 1) as usize;
                if idx < self.left_tape.len() {
                    self.left_tape[idx]
                } else {
                    BLANK_SYMBOL_ID
                }
            } else {
                let idx = i as usize;
                if idx < self.right_tape.len() {
                    self.right_tape[idx]
                } else {
                    BLANK_SYMBOL_ID
                }
            };
            tape_view.push(self.symbol_interner[symbol_id as usize]);
        }
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
        if let Some((id, _)) = self.state_interner.iter().find(|&(_, s)| s == state) {
            Ok(id as StateId)
        } else {
            let id = u16::try_from(self.state_interner.len())
                .map_err(|_| Error::InvalidTransition("Exceeded the maximum of 65536 unique states.".to_string()))?;
            self.state_interner.insert(state.to_string());
            Ok(id)
        }
    }

    fn get_or_intern_symbol(&mut self, symbol: char) -> Result<SymbolId, Error> {
        if let Some((id, _)) = self.symbol_interner.iter().find(|&(_, &s)| s == symbol) {
            Ok(id as SymbolId)
        } else {
            let id = u16::try_from(self.symbol_interner.len())
                .map_err(|_| Error::InvalidSymbol("Exceeded the maximum of 65536 unique symbols.".to_string()))?;
            self.symbol_interner.insert(symbol);
            Ok(id)
        }
    }

    fn validate_and_parse_transition(
        &mut self,
        transition: &(String, String, String, String, String),
    ) -> Result<(StateId, SymbolId, StateId, SymbolId, MoveDirection), Error> {
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
        let current_symbol = self.get_or_intern_symbol(current_symbol)?;
        let new_symbol = self.get_or_intern_symbol(new_symbol)?;

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
        for transition_tuple in transitions_list {
            let (current_state_id, current_symbol, new_state_id, new_symbol, move_direction) =
                self.validate_and_parse_transition(&transition_tuple)?;

            if self.transitions.len() <= current_state_id as usize {
                self.transitions.resize(current_state_id as usize + 1, Vec::new());
            }
            let state_map = &mut self.transitions[current_state_id as usize];

            if state_map.get(current_symbol as usize).copied().flatten().is_some() {
                return Err(Error::InvalidTransition(format!(
                    "Duplicate transition for state {} and symbol {}",
                    self.state_interner[current_state_id as usize], current_symbol
                )));
            }

            if (state_map.len() as SymbolId) <= current_symbol {
                state_map.resize((current_symbol + 1) as usize, None);
            }
            state_map[current_symbol as usize] = Some((new_state_id, new_symbol, move_direction));
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

    #[test]
    fn test_even_odd() {
        let transitions = vec![
            (
                "INIT".to_string(),
                "_".to_string(),
                "HALT".to_string(),
                "E".to_string(),
                "R".to_string(),
            ),
            (
                "INIT".to_string(),
                "|".to_string(),
                "ODD".to_string(),
                "_".to_string(),
                "R".to_string(),
            ),
            (
                "ODD".to_string(),
                "_".to_string(),
                "HALT".to_string(),
                "O".to_string(),
                "R".to_string(),
            ),
            (
                "ODD".to_string(),
                "|".to_string(),
                "INIT".to_string(),
                "_".to_string(),
                "R".to_string(),
            ),
        ];

        for n in 0..50 {
            let input_tape = "|".repeat(n);
            let expected_output = if n % 2 == 0 { "E" } else { "O" };
            let mut machine = LogicMill::new(transitions.clone(), "INIT", "HALT", '_').unwrap();
            let (output_tape, steps) = machine.run(input_tape, 1000, false).unwrap();
            assert_eq!(output_tape, expected_output);
            assert!(steps > 0);
        }
    }

    #[test]
    fn test_no_initial_state() {
        let transitions = vec![(
            "START".to_string(),
            "a".to_string(),
            "HALT".to_string(),
            "b".to_string(),
            "R".to_string(),
        )];
        let result = LogicMill::new(transitions, "INIT", "HALT", '_');
        assert!(matches!(result, Err(Error::InvalidTransition(_))));
    }

    #[test]
    fn test_no_halt_state() {
        let transitions = vec![(
            "INIT".to_string(),
            "a".to_string(),
            "END".to_string(),
            "b".to_string(),
            "R".to_string(),
        )];
        let result = LogicMill::new(transitions, "INIT", "HALT", '_');
        assert!(matches!(result, Err(Error::InvalidTransition(_))));
    }
}
