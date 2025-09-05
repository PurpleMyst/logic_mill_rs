use slab::Slab;
use rustc_hash::FxHashMap as HashMap;

pub const RIGHT: &str = "R";
pub const LEFT: &str = "L";

pub type Transition = (String, String, String, String, String);

#[derive(Clone, Copy, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
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

/// Custom error type for the Turing machine.
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// An invalid transition was encountered.
    InvalidTransition(String),

    /// A required transition is missing.
    MissingTransition(String),

    /// An invalid symbol was encountered.
    InvalidSymbol(String),

    /// The maximum number of steps has been reached.
    MaxStepsReached(u64),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidTransition(s) => f.pad(s),
            Error::MissingTransition(s) => f.pad(s),
            Error::InvalidSymbol(s) => f.pad(s),
            Error::MaxStepsReached(n) => write!(f, "Max steps reached: {n}"),
        }
    }
}

impl std::error::Error for Error {}

type StateId = u16;
type SymbolId = u16;

const BLANK_SYMBOL_ID: SymbolId = 0;

const INITIAL_STATE_ID: StateId = 1;
const HALT_STATE_ID: StateId = 0;

/// A high-performance Turing machine implementation in Rust.
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct LogicMill {
    /// Transition matrix:
    /// [current_state][current_symbol] -> (new_state, new_symbol, move_direction)
    transitions: Vec<Vec<Option<(StateId, SymbolId, MoveDirection)>>>,

    /// Tracks which rules have been used: [state_id][symbol_id] -> bool
    rules_used: Vec<Vec<bool>>,

    /// The tape is represented as two vectors for left and right of the head. The head position is
    /// an index into these vectors; negative for left, non-negative for right. The left tape is
    /// reversed to allow for efficient growth.
    right_tape: Vec<SymbolId>,
    left_tape: Vec<SymbolId>,

    /// The current head position on the tape.
    head_position: i64,

    /// The current state ID.
    current_state: StateId,

    /// Slab that allows mapping state IDs to names.
    state_interner: Slab<String>,

    /// Map for reverse lookup of state names to IDs.
    state_map: HashMap<String, StateId>,

    /// Slab that allows mapping symbols to IDs and vice versa.
    symbol_interner: Slab<char>,

    /// Map for reverse lookup of symbols to IDs.
    symbol_map: HashMap<char, SymbolId>,
}

impl LogicMill {
    /// Initialize the Turing Machine.
    pub fn new(
        transitions_list: &[Transition],
        initial_state: &str,
        halt_state: &str,
        blank_symbol: char,
    ) -> Result<Self, Error> {
        let mut machine = Self {
            transitions: Default::default(),
            rules_used: Default::default(),
            left_tape: Default::default(),
            right_tape: Default::default(),
            head_position: 0,
            current_state: 0,
            state_interner: {
                let mut slab = Slab::new();
                let halt_state_id = slab.insert(halt_state.to_string()) as StateId;
                assert_eq!(halt_state_id, HALT_STATE_ID);
                let initial_state_id = slab.insert(initial_state.to_string()) as StateId;
                assert_eq!(initial_state_id, INITIAL_STATE_ID);
                slab
            },
            state_map: {
                let mut map = HashMap::default();
                map.insert(halt_state.to_string(), HALT_STATE_ID);
                map.insert(initial_state.to_string(), INITIAL_STATE_ID);
                map
            },
            symbol_interner: {
                let mut slab = Slab::new();
                let blank_symbol_id = slab.insert(blank_symbol) as SymbolId;
                assert_eq!(blank_symbol_id, BLANK_SYMBOL_ID);
                slab
            },
            symbol_map: {
                let mut map = HashMap::default();
                map.insert(blank_symbol, BLANK_SYMBOL_ID);
                map
            },
        };

        machine.parse_transitions_list(transitions_list)?;

        machine.rules_used = vec![Vec::new(); machine.state_interner.len()];
        for (state_id, symbols) in machine.transitions.iter().enumerate() {
            machine.rules_used[state_id].resize(symbols.len(), false);
        }

        if machine
            .transitions
            .get(INITIAL_STATE_ID as usize)
            .is_none_or(|state_map| state_map.iter().flatten().next().is_none())
        {
            return Err(Error::InvalidTransition(format!(
                "Initial state {initial_state} not found in the transitions"
            )));
        }

        let mut found_halt = false;
        for transition_map in &machine.transitions {
            for new_state in transition_map.iter().flatten().map(|(s, _, _)| *s) {
                if new_state == HALT_STATE_ID {
                    found_halt = true;
                    break;
                }
            }
        }
        if !found_halt {
            return Err(Error::InvalidTransition(format!(
                "Halt state {halt_state} not found in the transitions"
            )));
        }

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

        let state_transitions = self.transitions.get(self.current_state as usize).ok_or_else(|| {
            let state_name = &self.state_interner[self.current_state as usize];
            Error::MissingTransition(format!("No transitions for state {state_name}"))
        })?;

        let transition = state_transitions
            .get(current_symbol as usize)
            .copied()
            .flatten()
            .ok_or_else(|| {
                let current_symbol = self.symbol_interner[current_symbol as usize];
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
        self.current_state = INITIAL_STATE_ID;

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
            for (symbol, rule) in symbols.iter().enumerate() {
                if rule.is_none() {
                    continue;
                }
                if let Some(&used) = self.rules_used[state_id].get(symbol)
                    && !used
                {
                    unused.push((self.state_interner[state_id].clone(), self.symbol_interner[symbol]));
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
            if self.current_state == HALT_STATE_ID {
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

    /// Intern a state name, returning its ID. If the state is new, it is added to the interner.
    fn get_or_intern_state(&mut self, state: &str) -> Result<StateId, Error> {
        if let Some(&id) = self.state_map.get(state) {
            Ok(id)
        } else {
            let id = self.state_interner.len() as u16;
            if id == 1024 {
                return Err(Error::InvalidTransition(format!(
                    "Too many states: {id}. Maximum is 1024."
                )));
            }
            self.state_interner.insert(state.to_string());
            self.state_map.insert(state.to_string(), id);
            Ok(id)
        }
    }

    /// Intern a symbol, returning its ID. If the symbol is new, it is added to the interner.
    fn get_or_intern_symbol(&mut self, symbol: char) -> Result<SymbolId, Error> {
        if let Some(&id) = self.symbol_map.get(&symbol) {
            Ok(id as SymbolId)
        } else {
            let id = u16::try_from(self.symbol_interner.len())
                .map_err(|_| Error::InvalidSymbol("Exceeded the maximum of 65536 unique symbols.".to_string()))?;
            self.symbol_interner.insert(symbol);
            self.symbol_map.insert(symbol, id);
            Ok(id)
        }
    }

    /// Validate and parse a single transition tuple.
    fn validate_and_parse_transition(
        &mut self,
        transition: &Transition,
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

    /// Parse and validate a list of transitions, populating the transition matrix.
    fn parse_transitions_list(&mut self, transitions_list: &[Transition]) -> Result<(), Error> {
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
                    self.state_interner[current_state_id as usize], self.symbol_interner[current_symbol as usize]
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

/// Parses a string into a list of transition rules.
pub fn parse_transition_rules(transition_rules_str: &str) -> Result<Vec<Transition>, Error> {
    const COMMENT_PREFIX: &str = "//";
    let mut transitions_list = Vec::new();
    for raw_line in transition_rules_str.lines() {
        let line = raw_line.trim();
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
        let mut machine = LogicMill::new(&transitions, "INIT", "HALT", '_').unwrap();
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
            let mut machine = LogicMill::new(&transitions, "INIT", "HALT", '_').unwrap();
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
        let result = LogicMill::new(&transitions, "INIT", "HALT", '_');
        assert!(matches!(result, Err(Error::InvalidTransition(_))));

        let result = LogicMill::new(&[], "INIT", "HALT", '_');
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
        let result = LogicMill::new(&transitions, "INIT", "HALT", '_');
        assert!(matches!(result, Err(Error::InvalidTransition(_))));

        let result = LogicMill::new(&[], "INIT", "HALT", '_');
        assert!(matches!(result, Err(Error::InvalidTransition(_))));
    }

    #[test]
    fn test_comments_in_transition_rules() {
        let rules_str = r#"
            // This is a comment
            INIT a HALT b R  // Another comment
            INIT b INIT a L
            // One more comment
        "#;
        let transitions = parse_transition_rules(rules_str).unwrap();
        assert_eq!(transitions.len(), 2);
        assert_eq!(
            transitions[0],
            (
                "INIT".to_string(),
                "a".to_string(),
                "HALT".to_string(),
                "b".to_string(),
                "R".to_string()
            )
        );
        assert_eq!(
            transitions[1],
            (
                "INIT".to_string(),
                "b".to_string(),
                "INIT".to_string(),
                "a".to_string(),
                "L".to_string()
            )
        );
    }

    #[test]
    fn test_unused_rules() {
        let transitions = vec![
            (
                "INIT".to_string(),
                "a".to_string(),
                "STATE1".to_string(),
                "b".to_string(),
                "R".to_string(),
            ),
            (
                "STATE1".to_string(),
                "a".to_string(),
                "HALT".to_string(),
                "c".to_string(),
                "R".to_string(),
            ),
            (
                "STATE1".to_string(),
                "c".to_string(),
                "HALT".to_string(),
                "d".to_string(),
                "R".to_string(),
            ),
        ];
        let mut machine = LogicMill::new(&transitions, "INIT", "HALT", '_').unwrap();
        eprintln!("{machine:?}");
        let (output_tape, steps) = machine.run("aa_".to_string(), 100, false).unwrap();
        assert_eq!(output_tape, "bc");
        assert_eq!(steps, 2);

        let unused = machine.unused_rules();
        assert_eq!(unused.len(), 1);
        assert_eq!(unused[0], ("STATE1".to_string(), 'c'));
    }

    fn to_roman(n: u16) -> String {
        const M: [&str; 4] = ["", "M", "MM", "MMM"];
        const C: [&str; 10] = ["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"];
        const X: [&str; 10] = ["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"];
        const I: [&str; 10] = ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"];
        format!(
            "{}{}{}{}",
            M[(n / 1000) as usize],
            C[((n % 1000) / 100) as usize],
            X[((n % 100) / 10) as usize],
            I[(n % 10) as usize]
        )
    }

    #[test]
    fn test_no_unused_rules() {
        let rules = r#"
ADD_1 I ADD_1 I R
ADD_1 V ADD_4 _ R
ADD_1 X ADD_9 _ R
ADD_1 _ NEXT | L
ADD_1 | ADD_1 | R
ADD_10 C ADD_90 _ R
ADD_10 I ADD_10 I R
ADD_10 L ADD_40 _ R
ADD_10 V ADD_10 V R
ADD_10 X ADD_10 X R
ADD_10 _ ADD_9 | R
ADD_10 | ADD_10 | R
ADD_100 C ADD_100 C R
ADD_100 D ADD_400 _ R
ADD_100 I ADD_100 I R
ADD_100 L ADD_100 L R
ADD_100 M ADD_900 _ R
ADD_100 V ADD_100 V R
ADD_100 X ADD_100 X R
ADD_100 _ ADD_99 | R
ADD_100 | ADD_100 | R
ADD_1000 C ADD_1000 C R
ADD_1000 D ADD_1000 D R
ADD_1000 I ADD_1000 I R
ADD_1000 L ADD_1000 L R
ADD_1000 M ADD_1000 M R
ADD_1000 V ADD_1000 V R
ADD_1000 X ADD_1000 X R
ADD_1000 _ ADD_999 | R
ADD_1000 | ADD_1000 | R
ADD_101 _ ADD_100 | R
ADD_102 _ ADD_101 | R
ADD_103 _ ADD_102 | R
ADD_104 _ ADD_103 | R
ADD_105 _ ADD_104 | R
ADD_106 _ ADD_105 | R
ADD_107 _ ADD_106 | R
ADD_108 _ ADD_107 | R
ADD_109 _ ADD_108 | R
ADD_11 _ ADD_10 | R
ADD_110 _ ADD_109 | R
ADD_111 _ ADD_110 | R
ADD_112 _ ADD_111 | R
ADD_113 _ ADD_112 | R
ADD_114 _ ADD_113 | R
ADD_115 _ ADD_114 | R
ADD_116 _ ADD_115 | R
ADD_117 _ ADD_116 | R
ADD_118 _ ADD_117 | R
ADD_119 _ ADD_118 | R
ADD_12 _ ADD_11 | R
ADD_120 _ ADD_119 | R
ADD_121 _ ADD_120 | R
ADD_122 _ ADD_121 | R
ADD_123 _ ADD_122 | R
ADD_124 _ ADD_123 | R
ADD_125 _ ADD_124 | R
ADD_126 _ ADD_125 | R
ADD_127 _ ADD_126 | R
ADD_128 _ ADD_127 | R
ADD_129 _ ADD_128 | R
ADD_13 _ ADD_12 | R
ADD_130 _ ADD_129 | R
ADD_131 _ ADD_130 | R
ADD_132 _ ADD_131 | R
ADD_133 _ ADD_132 | R
ADD_134 _ ADD_133 | R
ADD_135 _ ADD_134 | R
ADD_136 _ ADD_135 | R
ADD_137 _ ADD_136 | R
ADD_138 _ ADD_137 | R
ADD_139 _ ADD_138 | R
ADD_14 _ ADD_13 | R
ADD_140 _ ADD_139 | R
ADD_141 _ ADD_140 | R
ADD_142 _ ADD_141 | R
ADD_143 _ ADD_142 | R
ADD_144 _ ADD_143 | R
ADD_145 _ ADD_144 | R
ADD_146 _ ADD_145 | R
ADD_147 _ ADD_146 | R
ADD_148 _ ADD_147 | R
ADD_149 _ ADD_148 | R
ADD_15 _ ADD_14 | R
ADD_150 _ ADD_149 | R
ADD_151 _ ADD_150 | R
ADD_152 _ ADD_151 | R
ADD_153 _ ADD_152 | R
ADD_154 _ ADD_153 | R
ADD_155 _ ADD_154 | R
ADD_156 _ ADD_155 | R
ADD_157 _ ADD_156 | R
ADD_158 _ ADD_157 | R
ADD_159 _ ADD_158 | R
ADD_16 _ ADD_15 | R
ADD_160 _ ADD_159 | R
ADD_161 _ ADD_160 | R
ADD_162 _ ADD_161 | R
ADD_163 _ ADD_162 | R
ADD_164 _ ADD_163 | R
ADD_165 _ ADD_164 | R
ADD_166 _ ADD_165 | R
ADD_167 _ ADD_166 | R
ADD_168 _ ADD_167 | R
ADD_169 _ ADD_168 | R
ADD_17 _ ADD_16 | R
ADD_170 _ ADD_169 | R
ADD_171 _ ADD_170 | R
ADD_172 _ ADD_171 | R
ADD_173 _ ADD_172 | R
ADD_174 _ ADD_173 | R
ADD_175 _ ADD_174 | R
ADD_176 _ ADD_175 | R
ADD_177 _ ADD_176 | R
ADD_178 _ ADD_177 | R
ADD_179 _ ADD_178 | R
ADD_18 _ ADD_17 | R
ADD_180 _ ADD_179 | R
ADD_181 _ ADD_180 | R
ADD_182 _ ADD_181 | R
ADD_183 _ ADD_182 | R
ADD_184 _ ADD_183 | R
ADD_185 _ ADD_184 | R
ADD_186 _ ADD_185 | R
ADD_187 _ ADD_186 | R
ADD_188 _ ADD_187 | R
ADD_189 _ ADD_188 | R
ADD_19 _ ADD_18 | R
ADD_190 _ ADD_189 | R
ADD_191 _ ADD_190 | R
ADD_192 _ ADD_191 | R
ADD_193 _ ADD_192 | R
ADD_194 _ ADD_193 | R
ADD_195 _ ADD_194 | R
ADD_196 _ ADD_195 | R
ADD_197 _ ADD_196 | R
ADD_198 _ ADD_197 | R
ADD_199 _ ADD_198 | R
ADD_2 _ ADD_1 | R
ADD_20 _ ADD_19 | R
ADD_200 _ ADD_199 | R
ADD_201 _ ADD_200 | R
ADD_202 _ ADD_201 | R
ADD_203 _ ADD_202 | R
ADD_204 _ ADD_203 | R
ADD_205 _ ADD_204 | R
ADD_206 _ ADD_205 | R
ADD_207 _ ADD_206 | R
ADD_208 _ ADD_207 | R
ADD_209 _ ADD_208 | R
ADD_21 _ ADD_20 | R
ADD_210 _ ADD_209 | R
ADD_211 _ ADD_210 | R
ADD_212 _ ADD_211 | R
ADD_213 _ ADD_212 | R
ADD_214 _ ADD_213 | R
ADD_215 _ ADD_214 | R
ADD_216 _ ADD_215 | R
ADD_217 _ ADD_216 | R
ADD_218 _ ADD_217 | R
ADD_219 _ ADD_218 | R
ADD_22 _ ADD_21 | R
ADD_220 _ ADD_219 | R
ADD_221 _ ADD_220 | R
ADD_222 _ ADD_221 | R
ADD_223 _ ADD_222 | R
ADD_224 _ ADD_223 | R
ADD_225 _ ADD_224 | R
ADD_226 _ ADD_225 | R
ADD_227 _ ADD_226 | R
ADD_228 _ ADD_227 | R
ADD_229 _ ADD_228 | R
ADD_23 _ ADD_22 | R
ADD_230 _ ADD_229 | R
ADD_231 _ ADD_230 | R
ADD_232 _ ADD_231 | R
ADD_233 _ ADD_232 | R
ADD_234 _ ADD_233 | R
ADD_235 _ ADD_234 | R
ADD_236 _ ADD_235 | R
ADD_237 _ ADD_236 | R
ADD_238 _ ADD_237 | R
ADD_239 _ ADD_238 | R
ADD_24 _ ADD_23 | R
ADD_240 _ ADD_239 | R
ADD_241 _ ADD_240 | R
ADD_242 _ ADD_241 | R
ADD_243 _ ADD_242 | R
ADD_244 _ ADD_243 | R
ADD_245 _ ADD_244 | R
ADD_246 _ ADD_245 | R
ADD_247 _ ADD_246 | R
ADD_248 _ ADD_247 | R
ADD_249 _ ADD_248 | R
ADD_25 _ ADD_24 | R
ADD_250 _ ADD_249 | R
ADD_251 _ ADD_250 | R
ADD_252 _ ADD_251 | R
ADD_253 _ ADD_252 | R
ADD_254 _ ADD_253 | R
ADD_255 _ ADD_254 | R
ADD_256 _ ADD_255 | R
ADD_257 _ ADD_256 | R
ADD_258 _ ADD_257 | R
ADD_259 _ ADD_258 | R
ADD_26 _ ADD_25 | R
ADD_260 _ ADD_259 | R
ADD_261 _ ADD_260 | R
ADD_262 _ ADD_261 | R
ADD_263 _ ADD_262 | R
ADD_264 _ ADD_263 | R
ADD_265 _ ADD_264 | R
ADD_266 _ ADD_265 | R
ADD_267 _ ADD_266 | R
ADD_268 _ ADD_267 | R
ADD_269 _ ADD_268 | R
ADD_27 _ ADD_26 | R
ADD_270 _ ADD_269 | R
ADD_271 _ ADD_270 | R
ADD_272 _ ADD_271 | R
ADD_273 _ ADD_272 | R
ADD_274 _ ADD_273 | R
ADD_275 _ ADD_274 | R
ADD_276 _ ADD_275 | R
ADD_277 _ ADD_276 | R
ADD_278 _ ADD_277 | R
ADD_279 _ ADD_278 | R
ADD_28 _ ADD_27 | R
ADD_280 _ ADD_279 | R
ADD_281 _ ADD_280 | R
ADD_282 _ ADD_281 | R
ADD_283 _ ADD_282 | R
ADD_284 _ ADD_283 | R
ADD_285 _ ADD_284 | R
ADD_286 _ ADD_285 | R
ADD_287 _ ADD_286 | R
ADD_288 _ ADD_287 | R
ADD_289 _ ADD_288 | R
ADD_29 _ ADD_28 | R
ADD_290 _ ADD_289 | R
ADD_291 _ ADD_290 | R
ADD_292 _ ADD_291 | R
ADD_293 _ ADD_292 | R
ADD_294 _ ADD_293 | R
ADD_295 _ ADD_294 | R
ADD_296 _ ADD_295 | R
ADD_297 _ ADD_296 | R
ADD_298 _ ADD_297 | R
ADD_299 _ ADD_298 | R
ADD_3 _ ADD_2 | R
ADD_30 _ ADD_29 | R
ADD_300 _ ADD_299 | R
ADD_301 _ ADD_300 | R
ADD_302 _ ADD_301 | R
ADD_303 _ ADD_302 | R
ADD_304 _ ADD_303 | R
ADD_305 _ ADD_304 | R
ADD_306 _ ADD_305 | R
ADD_307 _ ADD_306 | R
ADD_308 _ ADD_307 | R
ADD_309 _ ADD_308 | R
ADD_31 _ ADD_30 | R
ADD_310 _ ADD_309 | R
ADD_311 _ ADD_310 | R
ADD_312 _ ADD_311 | R
ADD_313 _ ADD_312 | R
ADD_314 _ ADD_313 | R
ADD_315 _ ADD_314 | R
ADD_316 _ ADD_315 | R
ADD_317 _ ADD_316 | R
ADD_318 _ ADD_317 | R
ADD_319 _ ADD_318 | R
ADD_32 _ ADD_31 | R
ADD_320 _ ADD_319 | R
ADD_321 _ ADD_320 | R
ADD_322 _ ADD_321 | R
ADD_323 _ ADD_322 | R
ADD_324 _ ADD_323 | R
ADD_325 _ ADD_324 | R
ADD_326 _ ADD_325 | R
ADD_327 _ ADD_326 | R
ADD_328 _ ADD_327 | R
ADD_329 _ ADD_328 | R
ADD_33 _ ADD_32 | R
ADD_330 _ ADD_329 | R
ADD_331 _ ADD_330 | R
ADD_332 _ ADD_331 | R
ADD_333 _ ADD_332 | R
ADD_334 _ ADD_333 | R
ADD_335 _ ADD_334 | R
ADD_336 _ ADD_335 | R
ADD_337 _ ADD_336 | R
ADD_338 _ ADD_337 | R
ADD_339 _ ADD_338 | R
ADD_34 _ ADD_33 | R
ADD_340 _ ADD_339 | R
ADD_341 _ ADD_340 | R
ADD_342 _ ADD_341 | R
ADD_343 _ ADD_342 | R
ADD_344 _ ADD_343 | R
ADD_345 _ ADD_344 | R
ADD_346 _ ADD_345 | R
ADD_347 _ ADD_346 | R
ADD_348 _ ADD_347 | R
ADD_349 _ ADD_348 | R
ADD_35 _ ADD_34 | R
ADD_350 _ ADD_349 | R
ADD_351 _ ADD_350 | R
ADD_352 _ ADD_351 | R
ADD_353 _ ADD_352 | R
ADD_354 _ ADD_353 | R
ADD_355 _ ADD_354 | R
ADD_356 _ ADD_355 | R
ADD_357 _ ADD_356 | R
ADD_358 _ ADD_357 | R
ADD_359 _ ADD_358 | R
ADD_36 _ ADD_35 | R
ADD_360 _ ADD_359 | R
ADD_361 _ ADD_360 | R
ADD_362 _ ADD_361 | R
ADD_363 _ ADD_362 | R
ADD_364 _ ADD_363 | R
ADD_365 _ ADD_364 | R
ADD_366 _ ADD_365 | R
ADD_367 _ ADD_366 | R
ADD_368 _ ADD_367 | R
ADD_369 _ ADD_368 | R
ADD_37 _ ADD_36 | R
ADD_370 _ ADD_369 | R
ADD_371 _ ADD_370 | R
ADD_372 _ ADD_371 | R
ADD_373 _ ADD_372 | R
ADD_374 _ ADD_373 | R
ADD_375 _ ADD_374 | R
ADD_376 _ ADD_375 | R
ADD_377 _ ADD_376 | R
ADD_378 _ ADD_377 | R
ADD_379 _ ADD_378 | R
ADD_38 _ ADD_37 | R
ADD_380 _ ADD_379 | R
ADD_381 _ ADD_380 | R
ADD_382 _ ADD_381 | R
ADD_383 _ ADD_382 | R
ADD_384 _ ADD_383 | R
ADD_385 _ ADD_384 | R
ADD_386 _ ADD_385 | R
ADD_387 _ ADD_386 | R
ADD_388 _ ADD_387 | R
ADD_389 _ ADD_388 | R
ADD_39 _ ADD_38 | R
ADD_390 _ ADD_389 | R
ADD_391 _ ADD_390 | R
ADD_392 _ ADD_391 | R
ADD_393 _ ADD_392 | R
ADD_394 _ ADD_393 | R
ADD_395 _ ADD_394 | R
ADD_396 _ ADD_395 | R
ADD_397 _ ADD_396 | R
ADD_398 _ ADD_397 | R
ADD_399 _ ADD_398 | R
ADD_4 _ ADD_3 | R
ADD_4 | ADD_4 | R
ADD_40 I ADD_40 I R
ADD_40 V ADD_40 V R
ADD_40 X ADD_40 X R
ADD_40 _ ADD_39 | R
ADD_40 | ADD_40 | R
ADD_400 C ADD_400 C R
ADD_400 I ADD_400 I R
ADD_400 L ADD_400 L R
ADD_400 V ADD_400 V R
ADD_400 X ADD_400 X R
ADD_400 _ ADD_399 | R
ADD_400 | ADD_400 | R
ADD_401 _ ADD_400 | R
ADD_402 _ ADD_401 | R
ADD_403 _ ADD_402 | R
ADD_404 _ ADD_403 | R
ADD_405 _ ADD_404 | R
ADD_406 _ ADD_405 | R
ADD_407 _ ADD_406 | R
ADD_408 _ ADD_407 | R
ADD_409 _ ADD_408 | R
ADD_41 _ ADD_40 | R
ADD_410 _ ADD_409 | R
ADD_411 _ ADD_410 | R
ADD_412 _ ADD_411 | R
ADD_413 _ ADD_412 | R
ADD_414 _ ADD_413 | R
ADD_415 _ ADD_414 | R
ADD_416 _ ADD_415 | R
ADD_417 _ ADD_416 | R
ADD_418 _ ADD_417 | R
ADD_419 _ ADD_418 | R
ADD_42 _ ADD_41 | R
ADD_420 _ ADD_419 | R
ADD_421 _ ADD_420 | R
ADD_422 _ ADD_421 | R
ADD_423 _ ADD_422 | R
ADD_424 _ ADD_423 | R
ADD_425 _ ADD_424 | R
ADD_426 _ ADD_425 | R
ADD_427 _ ADD_426 | R
ADD_428 _ ADD_427 | R
ADD_429 _ ADD_428 | R
ADD_43 _ ADD_42 | R
ADD_430 _ ADD_429 | R
ADD_431 _ ADD_430 | R
ADD_432 _ ADD_431 | R
ADD_433 _ ADD_432 | R
ADD_434 _ ADD_433 | R
ADD_435 _ ADD_434 | R
ADD_436 _ ADD_435 | R
ADD_437 _ ADD_436 | R
ADD_438 _ ADD_437 | R
ADD_439 _ ADD_438 | R
ADD_44 _ ADD_43 | R
ADD_440 _ ADD_439 | R
ADD_441 _ ADD_440 | R
ADD_442 _ ADD_441 | R
ADD_443 _ ADD_442 | R
ADD_444 _ ADD_443 | R
ADD_445 _ ADD_444 | R
ADD_446 _ ADD_445 | R
ADD_447 _ ADD_446 | R
ADD_448 _ ADD_447 | R
ADD_449 _ ADD_448 | R
ADD_45 _ ADD_44 | R
ADD_450 _ ADD_449 | R
ADD_451 _ ADD_450 | R
ADD_452 _ ADD_451 | R
ADD_453 _ ADD_452 | R
ADD_454 _ ADD_453 | R
ADD_455 _ ADD_454 | R
ADD_456 _ ADD_455 | R
ADD_457 _ ADD_456 | R
ADD_458 _ ADD_457 | R
ADD_459 _ ADD_458 | R
ADD_46 _ ADD_45 | R
ADD_460 _ ADD_459 | R
ADD_461 _ ADD_460 | R
ADD_462 _ ADD_461 | R
ADD_463 _ ADD_462 | R
ADD_464 _ ADD_463 | R
ADD_465 _ ADD_464 | R
ADD_466 _ ADD_465 | R
ADD_467 _ ADD_466 | R
ADD_468 _ ADD_467 | R
ADD_469 _ ADD_468 | R
ADD_47 _ ADD_46 | R
ADD_470 _ ADD_469 | R
ADD_471 _ ADD_470 | R
ADD_472 _ ADD_471 | R
ADD_473 _ ADD_472 | R
ADD_474 _ ADD_473 | R
ADD_475 _ ADD_474 | R
ADD_476 _ ADD_475 | R
ADD_477 _ ADD_476 | R
ADD_478 _ ADD_477 | R
ADD_479 _ ADD_478 | R
ADD_48 _ ADD_47 | R
ADD_480 _ ADD_479 | R
ADD_481 _ ADD_480 | R
ADD_482 _ ADD_481 | R
ADD_483 _ ADD_482 | R
ADD_484 _ ADD_483 | R
ADD_485 _ ADD_484 | R
ADD_486 _ ADD_485 | R
ADD_487 _ ADD_486 | R
ADD_488 _ ADD_487 | R
ADD_489 _ ADD_488 | R
ADD_49 _ ADD_48 | R
ADD_490 _ ADD_489 | R
ADD_491 _ ADD_490 | R
ADD_492 _ ADD_491 | R
ADD_493 _ ADD_492 | R
ADD_494 _ ADD_493 | R
ADD_495 _ ADD_494 | R
ADD_496 _ ADD_495 | R
ADD_497 _ ADD_496 | R
ADD_498 _ ADD_497 | R
ADD_499 _ ADD_498 | R
ADD_5 I ADD_5 I R
ADD_5 _ ADD_4 | R
ADD_5 | ADD_5 | R
ADD_50 I ADD_50 I R
ADD_50 V ADD_50 V R
ADD_50 X ADD_50 X R
ADD_50 _ ADD_49 | R
ADD_50 | ADD_50 | R
ADD_500 C ADD_500 C R
ADD_500 I ADD_500 I R
ADD_500 L ADD_500 L R
ADD_500 V ADD_500 V R
ADD_500 X ADD_500 X R
ADD_500 _ ADD_499 | R
ADD_500 | ADD_500 | R
ADD_501 _ ADD_500 | R
ADD_502 _ ADD_501 | R
ADD_503 _ ADD_502 | R
ADD_504 _ ADD_503 | R
ADD_505 _ ADD_504 | R
ADD_506 _ ADD_505 | R
ADD_507 _ ADD_506 | R
ADD_508 _ ADD_507 | R
ADD_509 _ ADD_508 | R
ADD_51 _ ADD_50 | R
ADD_510 _ ADD_509 | R
ADD_511 _ ADD_510 | R
ADD_512 _ ADD_511 | R
ADD_513 _ ADD_512 | R
ADD_514 _ ADD_513 | R
ADD_515 _ ADD_514 | R
ADD_516 _ ADD_515 | R
ADD_517 _ ADD_516 | R
ADD_518 _ ADD_517 | R
ADD_519 _ ADD_518 | R
ADD_52 _ ADD_51 | R
ADD_520 _ ADD_519 | R
ADD_521 _ ADD_520 | R
ADD_522 _ ADD_521 | R
ADD_523 _ ADD_522 | R
ADD_524 _ ADD_523 | R
ADD_525 _ ADD_524 | R
ADD_526 _ ADD_525 | R
ADD_527 _ ADD_526 | R
ADD_528 _ ADD_527 | R
ADD_529 _ ADD_528 | R
ADD_53 _ ADD_52 | R
ADD_530 _ ADD_529 | R
ADD_531 _ ADD_530 | R
ADD_532 _ ADD_531 | R
ADD_533 _ ADD_532 | R
ADD_534 _ ADD_533 | R
ADD_535 _ ADD_534 | R
ADD_536 _ ADD_535 | R
ADD_537 _ ADD_536 | R
ADD_538 _ ADD_537 | R
ADD_539 _ ADD_538 | R
ADD_54 _ ADD_53 | R
ADD_540 _ ADD_539 | R
ADD_541 _ ADD_540 | R
ADD_542 _ ADD_541 | R
ADD_543 _ ADD_542 | R
ADD_544 _ ADD_543 | R
ADD_545 _ ADD_544 | R
ADD_546 _ ADD_545 | R
ADD_547 _ ADD_546 | R
ADD_548 _ ADD_547 | R
ADD_549 _ ADD_548 | R
ADD_55 _ ADD_54 | R
ADD_550 _ ADD_549 | R
ADD_551 _ ADD_550 | R
ADD_552 _ ADD_551 | R
ADD_553 _ ADD_552 | R
ADD_554 _ ADD_553 | R
ADD_555 _ ADD_554 | R
ADD_556 _ ADD_555 | R
ADD_557 _ ADD_556 | R
ADD_558 _ ADD_557 | R
ADD_559 _ ADD_558 | R
ADD_56 _ ADD_55 | R
ADD_560 _ ADD_559 | R
ADD_561 _ ADD_560 | R
ADD_562 _ ADD_561 | R
ADD_563 _ ADD_562 | R
ADD_564 _ ADD_563 | R
ADD_565 _ ADD_564 | R
ADD_566 _ ADD_565 | R
ADD_567 _ ADD_566 | R
ADD_568 _ ADD_567 | R
ADD_569 _ ADD_568 | R
ADD_57 _ ADD_56 | R
ADD_570 _ ADD_569 | R
ADD_571 _ ADD_570 | R
ADD_572 _ ADD_571 | R
ADD_573 _ ADD_572 | R
ADD_574 _ ADD_573 | R
ADD_575 _ ADD_574 | R
ADD_576 _ ADD_575 | R
ADD_577 _ ADD_576 | R
ADD_578 _ ADD_577 | R
ADD_579 _ ADD_578 | R
ADD_58 _ ADD_57 | R
ADD_580 _ ADD_579 | R
ADD_581 _ ADD_580 | R
ADD_582 _ ADD_581 | R
ADD_583 _ ADD_582 | R
ADD_584 _ ADD_583 | R
ADD_585 _ ADD_584 | R
ADD_586 _ ADD_585 | R
ADD_587 _ ADD_586 | R
ADD_588 _ ADD_587 | R
ADD_589 _ ADD_588 | R
ADD_59 _ ADD_58 | R
ADD_590 _ ADD_589 | R
ADD_591 _ ADD_590 | R
ADD_592 _ ADD_591 | R
ADD_593 _ ADD_592 | R
ADD_594 _ ADD_593 | R
ADD_595 _ ADD_594 | R
ADD_596 _ ADD_595 | R
ADD_597 _ ADD_596 | R
ADD_598 _ ADD_597 | R
ADD_599 _ ADD_598 | R
ADD_6 _ ADD_5 | R
ADD_60 _ ADD_59 | R
ADD_600 _ ADD_599 | R
ADD_601 _ ADD_600 | R
ADD_602 _ ADD_601 | R
ADD_603 _ ADD_602 | R
ADD_604 _ ADD_603 | R
ADD_605 _ ADD_604 | R
ADD_606 _ ADD_605 | R
ADD_607 _ ADD_606 | R
ADD_608 _ ADD_607 | R
ADD_609 _ ADD_608 | R
ADD_61 _ ADD_60 | R
ADD_610 _ ADD_609 | R
ADD_611 _ ADD_610 | R
ADD_612 _ ADD_611 | R
ADD_613 _ ADD_612 | R
ADD_614 _ ADD_613 | R
ADD_615 _ ADD_614 | R
ADD_616 _ ADD_615 | R
ADD_617 _ ADD_616 | R
ADD_618 _ ADD_617 | R
ADD_619 _ ADD_618 | R
ADD_62 _ ADD_61 | R
ADD_620 _ ADD_619 | R
ADD_621 _ ADD_620 | R
ADD_622 _ ADD_621 | R
ADD_623 _ ADD_622 | R
ADD_624 _ ADD_623 | R
ADD_625 _ ADD_624 | R
ADD_626 _ ADD_625 | R
ADD_627 _ ADD_626 | R
ADD_628 _ ADD_627 | R
ADD_629 _ ADD_628 | R
ADD_63 _ ADD_62 | R
ADD_630 _ ADD_629 | R
ADD_631 _ ADD_630 | R
ADD_632 _ ADD_631 | R
ADD_633 _ ADD_632 | R
ADD_634 _ ADD_633 | R
ADD_635 _ ADD_634 | R
ADD_636 _ ADD_635 | R
ADD_637 _ ADD_636 | R
ADD_638 _ ADD_637 | R
ADD_639 _ ADD_638 | R
ADD_64 _ ADD_63 | R
ADD_640 _ ADD_639 | R
ADD_641 _ ADD_640 | R
ADD_642 _ ADD_641 | R
ADD_643 _ ADD_642 | R
ADD_644 _ ADD_643 | R
ADD_645 _ ADD_644 | R
ADD_646 _ ADD_645 | R
ADD_647 _ ADD_646 | R
ADD_648 _ ADD_647 | R
ADD_649 _ ADD_648 | R
ADD_65 _ ADD_64 | R
ADD_650 _ ADD_649 | R
ADD_651 _ ADD_650 | R
ADD_652 _ ADD_651 | R
ADD_653 _ ADD_652 | R
ADD_654 _ ADD_653 | R
ADD_655 _ ADD_654 | R
ADD_656 _ ADD_655 | R
ADD_657 _ ADD_656 | R
ADD_658 _ ADD_657 | R
ADD_659 _ ADD_658 | R
ADD_66 _ ADD_65 | R
ADD_660 _ ADD_659 | R
ADD_661 _ ADD_660 | R
ADD_662 _ ADD_661 | R
ADD_663 _ ADD_662 | R
ADD_664 _ ADD_663 | R
ADD_665 _ ADD_664 | R
ADD_666 _ ADD_665 | R
ADD_667 _ ADD_666 | R
ADD_668 _ ADD_667 | R
ADD_669 _ ADD_668 | R
ADD_67 _ ADD_66 | R
ADD_670 _ ADD_669 | R
ADD_671 _ ADD_670 | R
ADD_672 _ ADD_671 | R
ADD_673 _ ADD_672 | R
ADD_674 _ ADD_673 | R
ADD_675 _ ADD_674 | R
ADD_676 _ ADD_675 | R
ADD_677 _ ADD_676 | R
ADD_678 _ ADD_677 | R
ADD_679 _ ADD_678 | R
ADD_68 _ ADD_67 | R
ADD_680 _ ADD_679 | R
ADD_681 _ ADD_680 | R
ADD_682 _ ADD_681 | R
ADD_683 _ ADD_682 | R
ADD_684 _ ADD_683 | R
ADD_685 _ ADD_684 | R
ADD_686 _ ADD_685 | R
ADD_687 _ ADD_686 | R
ADD_688 _ ADD_687 | R
ADD_689 _ ADD_688 | R
ADD_69 _ ADD_68 | R
ADD_690 _ ADD_689 | R
ADD_691 _ ADD_690 | R
ADD_692 _ ADD_691 | R
ADD_693 _ ADD_692 | R
ADD_694 _ ADD_693 | R
ADD_695 _ ADD_694 | R
ADD_696 _ ADD_695 | R
ADD_697 _ ADD_696 | R
ADD_698 _ ADD_697 | R
ADD_699 _ ADD_698 | R
ADD_7 _ ADD_6 | R
ADD_70 _ ADD_69 | R
ADD_700 _ ADD_699 | R
ADD_701 _ ADD_700 | R
ADD_702 _ ADD_701 | R
ADD_703 _ ADD_702 | R
ADD_704 _ ADD_703 | R
ADD_705 _ ADD_704 | R
ADD_706 _ ADD_705 | R
ADD_707 _ ADD_706 | R
ADD_708 _ ADD_707 | R
ADD_709 _ ADD_708 | R
ADD_71 _ ADD_70 | R
ADD_710 _ ADD_709 | R
ADD_711 _ ADD_710 | R
ADD_712 _ ADD_711 | R
ADD_713 _ ADD_712 | R
ADD_714 _ ADD_713 | R
ADD_715 _ ADD_714 | R
ADD_716 _ ADD_715 | R
ADD_717 _ ADD_716 | R
ADD_718 _ ADD_717 | R
ADD_719 _ ADD_718 | R
ADD_72 _ ADD_71 | R
ADD_720 _ ADD_719 | R
ADD_721 _ ADD_720 | R
ADD_722 _ ADD_721 | R
ADD_723 _ ADD_722 | R
ADD_724 _ ADD_723 | R
ADD_725 _ ADD_724 | R
ADD_726 _ ADD_725 | R
ADD_727 _ ADD_726 | R
ADD_728 _ ADD_727 | R
ADD_729 _ ADD_728 | R
ADD_73 _ ADD_72 | R
ADD_730 _ ADD_729 | R
ADD_731 _ ADD_730 | R
ADD_732 _ ADD_731 | R
ADD_733 _ ADD_732 | R
ADD_734 _ ADD_733 | R
ADD_735 _ ADD_734 | R
ADD_736 _ ADD_735 | R
ADD_737 _ ADD_736 | R
ADD_738 _ ADD_737 | R
ADD_739 _ ADD_738 | R
ADD_74 _ ADD_73 | R
ADD_740 _ ADD_739 | R
ADD_741 _ ADD_740 | R
ADD_742 _ ADD_741 | R
ADD_743 _ ADD_742 | R
ADD_744 _ ADD_743 | R
ADD_745 _ ADD_744 | R
ADD_746 _ ADD_745 | R
ADD_747 _ ADD_746 | R
ADD_748 _ ADD_747 | R
ADD_749 _ ADD_748 | R
ADD_75 _ ADD_74 | R
ADD_750 _ ADD_749 | R
ADD_751 _ ADD_750 | R
ADD_752 _ ADD_751 | R
ADD_753 _ ADD_752 | R
ADD_754 _ ADD_753 | R
ADD_755 _ ADD_754 | R
ADD_756 _ ADD_755 | R
ADD_757 _ ADD_756 | R
ADD_758 _ ADD_757 | R
ADD_759 _ ADD_758 | R
ADD_76 _ ADD_75 | R
ADD_760 _ ADD_759 | R
ADD_761 _ ADD_760 | R
ADD_762 _ ADD_761 | R
ADD_763 _ ADD_762 | R
ADD_764 _ ADD_763 | R
ADD_765 _ ADD_764 | R
ADD_766 _ ADD_765 | R
ADD_767 _ ADD_766 | R
ADD_768 _ ADD_767 | R
ADD_769 _ ADD_768 | R
ADD_77 _ ADD_76 | R
ADD_770 _ ADD_769 | R
ADD_771 _ ADD_770 | R
ADD_772 _ ADD_771 | R
ADD_773 _ ADD_772 | R
ADD_774 _ ADD_773 | R
ADD_775 _ ADD_774 | R
ADD_776 _ ADD_775 | R
ADD_777 _ ADD_776 | R
ADD_778 _ ADD_777 | R
ADD_779 _ ADD_778 | R
ADD_78 _ ADD_77 | R
ADD_780 _ ADD_779 | R
ADD_781 _ ADD_780 | R
ADD_782 _ ADD_781 | R
ADD_783 _ ADD_782 | R
ADD_784 _ ADD_783 | R
ADD_785 _ ADD_784 | R
ADD_786 _ ADD_785 | R
ADD_787 _ ADD_786 | R
ADD_788 _ ADD_787 | R
ADD_789 _ ADD_788 | R
ADD_79 _ ADD_78 | R
ADD_790 _ ADD_789 | R
ADD_791 _ ADD_790 | R
ADD_792 _ ADD_791 | R
ADD_793 _ ADD_792 | R
ADD_794 _ ADD_793 | R
ADD_795 _ ADD_794 | R
ADD_796 _ ADD_795 | R
ADD_797 _ ADD_796 | R
ADD_798 _ ADD_797 | R
ADD_799 _ ADD_798 | R
ADD_8 _ ADD_7 | R
ADD_80 _ ADD_79 | R
ADD_800 _ ADD_799 | R
ADD_801 _ ADD_800 | R
ADD_802 _ ADD_801 | R
ADD_803 _ ADD_802 | R
ADD_804 _ ADD_803 | R
ADD_805 _ ADD_804 | R
ADD_806 _ ADD_805 | R
ADD_807 _ ADD_806 | R
ADD_808 _ ADD_807 | R
ADD_809 _ ADD_808 | R
ADD_81 _ ADD_80 | R
ADD_810 _ ADD_809 | R
ADD_811 _ ADD_810 | R
ADD_812 _ ADD_811 | R
ADD_813 _ ADD_812 | R
ADD_814 _ ADD_813 | R
ADD_815 _ ADD_814 | R
ADD_816 _ ADD_815 | R
ADD_817 _ ADD_816 | R
ADD_818 _ ADD_817 | R
ADD_819 _ ADD_818 | R
ADD_82 _ ADD_81 | R
ADD_820 _ ADD_819 | R
ADD_821 _ ADD_820 | R
ADD_822 _ ADD_821 | R
ADD_823 _ ADD_822 | R
ADD_824 _ ADD_823 | R
ADD_825 _ ADD_824 | R
ADD_826 _ ADD_825 | R
ADD_827 _ ADD_826 | R
ADD_828 _ ADD_827 | R
ADD_829 _ ADD_828 | R
ADD_83 _ ADD_82 | R
ADD_830 _ ADD_829 | R
ADD_831 _ ADD_830 | R
ADD_832 _ ADD_831 | R
ADD_833 _ ADD_832 | R
ADD_834 _ ADD_833 | R
ADD_835 _ ADD_834 | R
ADD_836 _ ADD_835 | R
ADD_837 _ ADD_836 | R
ADD_838 _ ADD_837 | R
ADD_839 _ ADD_838 | R
ADD_84 _ ADD_83 | R
ADD_840 _ ADD_839 | R
ADD_841 _ ADD_840 | R
ADD_842 _ ADD_841 | R
ADD_843 _ ADD_842 | R
ADD_844 _ ADD_843 | R
ADD_845 _ ADD_844 | R
ADD_846 _ ADD_845 | R
ADD_847 _ ADD_846 | R
ADD_848 _ ADD_847 | R
ADD_849 _ ADD_848 | R
ADD_85 _ ADD_84 | R
ADD_850 _ ADD_849 | R
ADD_851 _ ADD_850 | R
ADD_852 _ ADD_851 | R
ADD_853 _ ADD_852 | R
ADD_854 _ ADD_853 | R
ADD_855 _ ADD_854 | R
ADD_856 _ ADD_855 | R
ADD_857 _ ADD_856 | R
ADD_858 _ ADD_857 | R
ADD_859 _ ADD_858 | R
ADD_86 _ ADD_85 | R
ADD_860 _ ADD_859 | R
ADD_861 _ ADD_860 | R
ADD_862 _ ADD_861 | R
ADD_863 _ ADD_862 | R
ADD_864 _ ADD_863 | R
ADD_865 _ ADD_864 | R
ADD_866 _ ADD_865 | R
ADD_867 _ ADD_866 | R
ADD_868 _ ADD_867 | R
ADD_869 _ ADD_868 | R
ADD_87 _ ADD_86 | R
ADD_870 _ ADD_869 | R
ADD_871 _ ADD_870 | R
ADD_872 _ ADD_871 | R
ADD_873 _ ADD_872 | R
ADD_874 _ ADD_873 | R
ADD_875 _ ADD_874 | R
ADD_876 _ ADD_875 | R
ADD_877 _ ADD_876 | R
ADD_878 _ ADD_877 | R
ADD_879 _ ADD_878 | R
ADD_88 _ ADD_87 | R
ADD_880 _ ADD_879 | R
ADD_881 _ ADD_880 | R
ADD_882 _ ADD_881 | R
ADD_883 _ ADD_882 | R
ADD_884 _ ADD_883 | R
ADD_885 _ ADD_884 | R
ADD_886 _ ADD_885 | R
ADD_887 _ ADD_886 | R
ADD_888 _ ADD_887 | R
ADD_889 _ ADD_888 | R
ADD_89 _ ADD_88 | R
ADD_890 _ ADD_889 | R
ADD_891 _ ADD_890 | R
ADD_892 _ ADD_891 | R
ADD_893 _ ADD_892 | R
ADD_894 _ ADD_893 | R
ADD_895 _ ADD_894 | R
ADD_896 _ ADD_895 | R
ADD_897 _ ADD_896 | R
ADD_898 _ ADD_897 | R
ADD_899 _ ADD_898 | R
ADD_9 _ ADD_8 | R
ADD_9 | ADD_9 | R
ADD_90 I ADD_90 I R
ADD_90 V ADD_90 V R
ADD_90 X ADD_90 X R
ADD_90 _ ADD_89 | R
ADD_90 | ADD_90 | R
ADD_900 C ADD_900 C R
ADD_900 I ADD_900 I R
ADD_900 L ADD_900 L R
ADD_900 V ADD_900 V R
ADD_900 X ADD_900 X R
ADD_900 _ ADD_899 | R
ADD_900 | ADD_900 | R
ADD_901 _ ADD_900 | R
ADD_902 _ ADD_901 | R
ADD_903 _ ADD_902 | R
ADD_904 _ ADD_903 | R
ADD_905 _ ADD_904 | R
ADD_906 _ ADD_905 | R
ADD_907 _ ADD_906 | R
ADD_908 _ ADD_907 | R
ADD_909 _ ADD_908 | R
ADD_91 _ ADD_90 | R
ADD_910 _ ADD_909 | R
ADD_911 _ ADD_910 | R
ADD_912 _ ADD_911 | R
ADD_913 _ ADD_912 | R
ADD_914 _ ADD_913 | R
ADD_915 _ ADD_914 | R
ADD_916 _ ADD_915 | R
ADD_917 _ ADD_916 | R
ADD_918 _ ADD_917 | R
ADD_919 _ ADD_918 | R
ADD_92 _ ADD_91 | R
ADD_920 _ ADD_919 | R
ADD_921 _ ADD_920 | R
ADD_922 _ ADD_921 | R
ADD_923 _ ADD_922 | R
ADD_924 _ ADD_923 | R
ADD_925 _ ADD_924 | R
ADD_926 _ ADD_925 | R
ADD_927 _ ADD_926 | R
ADD_928 _ ADD_927 | R
ADD_929 _ ADD_928 | R
ADD_93 _ ADD_92 | R
ADD_930 _ ADD_929 | R
ADD_931 _ ADD_930 | R
ADD_932 _ ADD_931 | R
ADD_933 _ ADD_932 | R
ADD_934 _ ADD_933 | R
ADD_935 _ ADD_934 | R
ADD_936 _ ADD_935 | R
ADD_937 _ ADD_936 | R
ADD_938 _ ADD_937 | R
ADD_939 _ ADD_938 | R
ADD_94 _ ADD_93 | R
ADD_940 _ ADD_939 | R
ADD_941 _ ADD_940 | R
ADD_942 _ ADD_941 | R
ADD_943 _ ADD_942 | R
ADD_944 _ ADD_943 | R
ADD_945 _ ADD_944 | R
ADD_946 _ ADD_945 | R
ADD_947 _ ADD_946 | R
ADD_948 _ ADD_947 | R
ADD_949 _ ADD_948 | R
ADD_95 _ ADD_94 | R
ADD_950 _ ADD_949 | R
ADD_951 _ ADD_950 | R
ADD_952 _ ADD_951 | R
ADD_953 _ ADD_952 | R
ADD_954 _ ADD_953 | R
ADD_955 _ ADD_954 | R
ADD_956 _ ADD_955 | R
ADD_957 _ ADD_956 | R
ADD_958 _ ADD_957 | R
ADD_959 _ ADD_958 | R
ADD_96 _ ADD_95 | R
ADD_960 _ ADD_959 | R
ADD_961 _ ADD_960 | R
ADD_962 _ ADD_961 | R
ADD_963 _ ADD_962 | R
ADD_964 _ ADD_963 | R
ADD_965 _ ADD_964 | R
ADD_966 _ ADD_965 | R
ADD_967 _ ADD_966 | R
ADD_968 _ ADD_967 | R
ADD_969 _ ADD_968 | R
ADD_97 _ ADD_96 | R
ADD_970 _ ADD_969 | R
ADD_971 _ ADD_970 | R
ADD_972 _ ADD_971 | R
ADD_973 _ ADD_972 | R
ADD_974 _ ADD_973 | R
ADD_975 _ ADD_974 | R
ADD_976 _ ADD_975 | R
ADD_977 _ ADD_976 | R
ADD_978 _ ADD_977 | R
ADD_979 _ ADD_978 | R
ADD_98 _ ADD_97 | R
ADD_980 _ ADD_979 | R
ADD_981 _ ADD_980 | R
ADD_982 _ ADD_981 | R
ADD_983 _ ADD_982 | R
ADD_984 _ ADD_983 | R
ADD_985 _ ADD_984 | R
ADD_986 _ ADD_985 | R
ADD_987 _ ADD_986 | R
ADD_988 _ ADD_987 | R
ADD_989 _ ADD_988 | R
ADD_99 _ ADD_98 | R
ADD_990 _ ADD_989 | R
ADD_991 _ ADD_990 | R
ADD_992 _ ADD_991 | R
ADD_993 _ ADD_992 | R
ADD_994 _ ADD_993 | R
ADD_995 _ ADD_994 | R
ADD_996 _ ADD_995 | R
ADD_997 _ ADD_996 | R
ADD_998 _ ADD_997 | R
ADD_999 _ ADD_998 | R
INIT C ADD_100 _ R
INIT D ADD_500 _ R
INIT I ADD_1 _ R
INIT L ADD_50 _ R
INIT M ADD_1000 _ R
INIT V ADD_5 _ R
INIT X ADD_10 _ R
INIT | HALT | L
NEXT C NEXT C L
NEXT D NEXT D L
NEXT I NEXT I L
NEXT L NEXT L L
NEXT M NEXT M L
NEXT V NEXT V L
NEXT X NEXT X L
NEXT _ INIT _ R
NEXT | NEXT | L
        "#;
        let mut machine = LogicMill::new(&parse_transition_rules(rules).unwrap(), "INIT", "HALT", '_').unwrap();
        eprintln!("{machine:#?}");
        for n in 1..=3999 {
            let roman = to_roman(n);
            if n == 5 {
                eprintln!("{n} -> {roman}");
            }
            machine.run(roman, 20_000_000, false).unwrap();
        }
        assert_eq!(machine.unused_rules(), Vec::new());
    }

    #[test]
    fn test_state_without_rules() {
        let rules = include_str!("testcase_rules/unhandled_symbol.txt");
        let mut machine = LogicMill::new(&parse_transition_rules(rules).unwrap(), "INIT", "HALT", '_').unwrap();
        assert_eq!(
            machine.run("I".to_string(), 10, false),
            Err(Error::MissingTransition("No transitions for state SAW_1".to_string()))
        );
    }
}
