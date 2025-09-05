use pyo3::types::PyDict;
use std::hint::black_box;

use criterion::*;
use pyo3::prelude::*;

use logic_mill_rs::core::{LogicMill as RustLogicMill, parse_transition_rules as rust_parse_rules};

fn sqrt_inputs() -> impl Iterator<Item = String> {
    (1..=120).step_by(10).map(|n| "|".repeat(n * n))
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

fn roman_inputs() -> impl Iterator<Item = String> {
    (1..=3999).step_by(256).map(to_roman)
}

fn run_rust_benchmark(c: &mut Criterion) {
    let transitions = rust_parse_rules(include_str!("sqrt_rules.txt")).unwrap();
    let mut tm = RustLogicMill::new(&transitions, "INIT", "HALT", '_').unwrap();

    {
        let mut new_g = c.benchmark_group("rust/new");

        for (name, input) in [
            ("sqrt", include_str!("sqrt_rules.txt")),
            ("roman", include_str!("roman_rules.txt")),
        ] {
            let transitions = rust_parse_rules(input).unwrap();
            new_g.throughput(Throughput::Elements(transitions.len() as u64));
            new_g.bench_with_input(BenchmarkId::from_parameter(name), &transitions, |b, transitions| {
                b.iter(|| {
                    let tm = RustLogicMill::new(transitions, "INIT", "HALT", '_').unwrap();
                    black_box(tm);
                })
            });
        }
    }
    {
        let mut parse_g = c.benchmark_group("rust/parse");
        for (name, input) in [
            ("sqrt", include_str!("sqrt_rules.txt")),
            ("roman", include_str!("roman_rules.txt")),
        ] {
            parse_g.throughput(Throughput::Elements(input.lines().count() as u64));
            parse_g.bench_with_input(BenchmarkId::from_parameter(name), &input, |b, input| {
                b.iter(|| {
                    let transitions = rust_parse_rules(black_box(input)).unwrap();
                    black_box(transitions);
                })
            });
        }
    }

    {
        let mut g = c.benchmark_group("rust/sqrt");
        for input in sqrt_inputs() {
            g.throughput(Throughput::Elements(input.len() as u64));
            g.bench_with_input(BenchmarkId::from_parameter(input.len()), &input, |b, input| {
                b.iter(|| {
                    tm.run(black_box(input.to_string()), black_box(20_000_000), false)
                        .unwrap();
                })
            });
        }
    }

    {
        let transitions = rust_parse_rules(include_str!("roman_rules.txt")).unwrap();
        let mut tm = RustLogicMill::new(&transitions, "INIT", "HALT", '_').unwrap();

        let mut g = c.benchmark_group("rust/roman");
        for input in roman_inputs() {
            g.throughput(Throughput::Elements(input.len() as u64));
            g.bench_with_input(BenchmarkId::from_parameter(&input), &input, |b, input| {
                b.iter(|| {
                    tm.run(black_box(input.to_string()), black_box(20_000_000), false)
                        .unwrap();
                })
            });
        }
    }
}

fn run_python_benchmark(c: &mut Criterion) {
    // MODIFIED: Removed the call to the non-existent `prepare_interpreter`.
    // Python::with_gil handles initialization automatically.
    Python::initialize();
    Python::attach(|py| {
        // Add the current directory to Python's sys.path to find logic_mill.py
        // MODIFIED: Changed `import_bound` to the modern `import`
        let sys = py.import("sys").unwrap();
        let path = sys.getattr("path").unwrap();
        path.call_method1("append", (".",)).unwrap();

        // Import the Python module and the LogicMill class
        // MODIFIED: Changed `import_bound` to the modern `import`
        let logic_mill_module = py.import("logic_mill").unwrap();
        let py_logic_mill_class = logic_mill_module.getattr("LogicMill").unwrap();
        let py_parse_rules = logic_mill_module.getattr("parse_transition_rules").unwrap();

        // Parse rules using the Python function
        let transitions = py_parse_rules.call1((include_str!("sqrt_rules.txt"),)).unwrap();

        // Instantiate the Python LogicMill
        let kwargs = PyDict::new(py);
        kwargs.set_item("initial_state", "INIT").unwrap();
        kwargs.set_item("halt_state", "HALT").unwrap();
        kwargs.set_item("blank_symbol", '_').unwrap();

        let tm = py_logic_mill_class.call((transitions,), Some(&kwargs)).unwrap();

        let mut g = c.benchmark_group("py/sqrt");
        for input in sqrt_inputs() {
            g.throughput(Throughput::Elements(input.len() as u64));
            g.bench_with_input(BenchmarkId::from_parameter(input.len()), &input, |b, input| {
                b.iter(|| {
                    tm.call_method1("run", (black_box(input.to_string()), black_box(20_000_000)))
                        .unwrap();
                })
            });
        }
    });
}

// Define the benchmark group
criterion_group!(benches, run_rust_benchmark, run_python_benchmark);
// Generate the main function to run the benchmarks
criterion_main!(benches);
