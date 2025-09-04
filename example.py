import time
from logic_mill_rs import (
    LogicMill,
    parse_transition_rules,
    InvalidTransitionError,
    MissingTransitionError,
)

# A simple Turing machine that adds two numbers represented in unary.
# For example, "11_111" (2 + 3) becomes "11111" (5).
unary_adder_rules = """
// State  Symbol  New State  New Symbol  Move
// -------------------------------------------
INIT     1       ADD1       _           R
ADD1     1       ADD1       1           R
ADD1     _       ADD2       1           L
ADD2     1       ADD2       1           L
ADD2     _       CLEANUP    _           R
CLEANUP  1       CLEANUP    _           R
CLEANUP  _       HALT       _           L
"""

def main():
    """Demonstrates the use of the LogicMill Rust module."""
    try:
        print("--- Parsing transition rules ---")
        transitions = parse_transition_rules(unary_adder_rules)
        print(f"Parsed {len(transitions)} rules successfully.")

        print("\n--- Initializing Logic Mill ---")
        tm = LogicMill(transitions)
        print("LogicMill initialized.")

        input_tape = "11_111"  # Represents 2 + 3
        print(f"\n--- Running simulation for input: '{input_tape}' ---")

        start_time = time.perf_counter()
        # Set verbose=True to see each step of the simulation
        final_tape, steps = tm.run(input_tape, verbose=False)
        end_time = time.perf_counter()

        print("\n--- Results ---")
        print(f"Final tape: '{final_tape}'")
        print(f"Unary result: {len(final_tape)} (Expected 5)")
        print(f"Steps taken: {steps}")
        print(f"Execution time: {(end_time - start_time) * 1000:.4f} ms")

        print("\n--- Checking for unused rules ---")
        unused = tm.unused_rules()
        if unused:
            print("Unused rules found:", unused)
        else:
            print("All rules were used.")

    except (InvalidTransitionError, MissingTransitionError) as e:
        print(f"\nAn error occurred: {e}")
    except Exception as e:
        print(f"\nA general error occurred: {e}")

if __name__ == "__main__":
    main()
