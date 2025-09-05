pub mod core;

use pyo3::{
    create_exception,
    exceptions::{PyException, PyRuntimeError},
    prelude::*,
    types::PyBytes,
};

// Create custom Python exceptions that can be raised from Rust.
create_exception!(logic_mill_rs, InvalidTransitionError, PyException);
create_exception!(logic_mill_rs, MissingTransitionError, PyException);
create_exception!(logic_mill_rs, InvalidSymbolError, PyException);

/// A high-performance Turing machine implementation in Rust.
#[pyclass(name = "LogicMill")]
pub struct LogicMill {
    machine: core::LogicMill,
}

#[pymethods]
impl LogicMill {
    /// Initialize the Logic Mill.
    #[new]
    #[pyo3(signature = (
        transitions_list,
        initial_state = "INIT",
        halt_state = "HALT",
        blank_symbol = '_'
    ))]
    pub fn new(
        transitions_list: Vec<(String, String, String, String, String)>,
        initial_state: &str,
        halt_state: &str,
        blank_symbol: char,
    ) -> PyResult<Self> {
        let machine =
            core::LogicMill::new(transitions_list, initial_state, halt_state, blank_symbol).map_err(to_py_err)?;
        Ok(LogicMill { machine })
    }

    /// Run the Logic Mill with the given input string.
    ///
    /// Returns a tuple containing the final tape content and the number of steps taken.
    #[pyo3(signature = (input_tape, max_steps = 2_000_000, *, verbose = false))]
    pub fn run(&mut self, input_tape: String, max_steps: u64, verbose: bool) -> PyResult<(String, u64)> {
        self.machine.run(input_tape, max_steps, verbose).map_err(to_py_err)
    }

    /// Return a list of unused transition rules.
    fn unused_rules(&self) -> PyResult<Vec<(String, char)>> {
        Ok(self.machine.unused_rules())
    }

    /// Return the number of states in the Logic Mill.
    fn state_count(&self) -> PyResult<usize> {
        Ok(self.machine.state_count())
    }

    fn __getstate__(&self, py: Python<'_>) -> PyResult<Py<PyBytes>> {
        let serialized = serde_pickle::to_vec(&self.machine, serde_pickle::SerOptions::new())
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(e.to_string()))?;
        Ok(PyBytes::new(py, &serialized).into())
    }

    fn __setstate__(&mut self, bytes: &[u8]) -> PyResult<()> {
        self.machine = serde_pickle::from_slice(bytes, serde_pickle::DeOptions::new())
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(e.to_string()))?;
        Ok(())
    }
}

/// Parses a string into a list of transition rules.
#[pyfunction]
pub fn parse_transition_rules(transition_rules_str: String) -> PyResult<Vec<core::Transition>> {
    core::parse_transition_rules(&transition_rules_str).map_err(to_py_err)
}

/// A Python module implemented in Rust.
#[pymodule]
fn logic_mill_rs(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<LogicMill>()?;
    m.add_function(wrap_pyfunction!(parse_transition_rules, m)?)?;

    m.add("InvalidTransitionError", m.py().get_type::<InvalidTransitionError>())?;
    m.add("MissingTransitionError", m.py().get_type::<MissingTransitionError>())?;
    m.add("InvalidSymbolError", m.py().get_type::<InvalidSymbolError>())?;

    Ok(())
}

/// Helper function to convert core::Error to a PyErr.
fn to_py_err(err: core::Error) -> PyErr {
    match err {
        core::Error::InvalidTransition(s) => InvalidTransitionError::new_err(s),
        core::Error::MissingTransition(s) => MissingTransitionError::new_err(s),
        core::Error::InvalidSymbol(s) => InvalidSymbolError::new_err(s),
        core::Error::MaxStepsReached(n) => PyRuntimeError::new_err(format!("Max steps reached: {n}")),
    }
}
