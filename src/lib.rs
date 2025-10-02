pub mod core;

use pyo3::{
    create_exception,
    exceptions::{PyException, PyRuntimeError},
    prelude::*,
    types::{PyBytes, PyTuple},
};

// Create custom Python exceptions that can be raised from Rust.
create_exception!(logic_mill_rs, InvalidTransitionError, PyException);
create_exception!(logic_mill_rs, MissingTransitionError, PyException);
create_exception!(logic_mill_rs, InvalidSymbolError, PyException);

/// A high-performance Turing machine implementation in Rust.
#[pyclass(module = "logic_mill_rs")]
pub struct LogicMill {
    machine: core::LogicMill,
}

#[pymethods]
impl LogicMill {
    /// Initialize the Logic Mill.
    #[new]
    #[pyo3(signature = (
        rules,
        initial_state = "INIT",
        halt_state = "HALT",
        blank_symbol = '_'
    ))]
    pub fn new(rules: &str, initial_state: &str, halt_state: &str, blank_symbol: char) -> PyResult<Self> {
        let machine = core::LogicMill::new(rules, initial_state, halt_state, blank_symbol)?;
        Ok(LogicMill { machine })
    }

    /// Run the Logic Mill with the given input string.
    ///
    /// Returns a tuple containing the final tape content and the number of steps taken.
    #[pyo3(signature = (input_tape, max_steps = 2_000_000, *, verbose = false))]
    pub fn run(&mut self, py: Python<'_>, input_tape: &str, max_steps: u64, verbose: bool) -> PyResult<(String, u64)> {
        py.detach(|| {
            self.machine
                .run(input_tape, max_steps, verbose, || {
                    Python::attach(|py| py.check_signals())
                })
                .map_err(Into::into)
        })
    }

    /// Return a list of unused transition rules.
    fn unused_rules(&self) -> PyResult<Vec<(String, char)>> {
        Ok(self.machine.unused_rules())
    }

    /// Return the number of states in the Logic Mill.
    fn state_count(&self) -> PyResult<usize> {
        Ok(self.machine.state_count())
    }

    /// Serialize the Logic Mill state for pickling.
    fn __getstate__(&self, py: Python<'_>) -> PyResult<Py<PyBytes>> {
        let serialized = serde_pickle::to_vec(&self.machine, serde_pickle::SerOptions::new())
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(e.to_string()))?;
        Ok(PyBytes::new(py, &serialized).into())
    }

    /// Deserialize the Logic Mill state from pickled data.
    fn __setstate__(&mut self, bytes: &[u8]) -> PyResult<()> {
        self.machine = serde_pickle::from_slice(bytes, serde_pickle::DeOptions::new())
            .map_err(|e| pyo3::exceptions::PyValueError::new_err(e.to_string()))?;
        Ok(())
    }

    /// Support for the pickle protocol; this is necessary (i.e. the __getstate__ and __setstate__
    /// methods alone are not enough) because our __init__ method takes arguments.
    fn __reduce__(&self, py: Python<'_>) -> PyResult<(Py<PyAny>, Py<PyTuple>, Py<PyBytes>)> {
        let cls = py.get_type::<LogicMill>();
        // Dummy args just to get past __init__; the actual state is in the third element returned.
        let args = PyTuple::new(py, ["INIT _ HALT _ R"])?;
        Ok((cls.into(), args.into(), self.__getstate__(py)?))
    }
}

/// A Python module implemented in Rust.
#[pymodule]
fn logic_mill_rs(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<LogicMill>()?;

    m.add("InvalidTransitionError", m.py().get_type::<InvalidTransitionError>())?;
    m.add("MissingTransitionError", m.py().get_type::<MissingTransitionError>())?;
    m.add("InvalidSymbolError", m.py().get_type::<InvalidSymbolError>())?;

    Ok(())
}

/// Helper function to convert core::Error to a PyErr.
impl From<core::Error> for PyErr {
    fn from(err: core::Error) -> PyErr {
        match err {
            core::Error::InvalidTransition(s) => InvalidTransitionError::new_err(s),
            core::Error::MissingTransition(s) => MissingTransitionError::new_err(s),
            core::Error::InvalidSymbol(s) => InvalidSymbolError::new_err(s),
            core::Error::MaxStepsReached(n) => PyRuntimeError::new_err(format!("Max steps reached: {n}")),
            core::Error::PyErr(e) => e,
        }
    }
}
