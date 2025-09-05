logic-mill-rs
=============

Implementation of Logic Mill in Rust, from the lovely [Marches & Gnats](https://mng.quest/) game.

Why?
----

Speed! I tinker with my solutions a lot, so being able to quickly iterate is important to me. :)

> [!NOTE]  
> The Rust core can be found in the creatively named `core` module, which can be used independently
> of the Python bindings.

How?
----

I started with a direct translation of the Python implementation via Gemini 2.5 Pro to Rust, then
refactored heavily. At time of writing, the following techniques are used for speed:
- Symbol & state interning (i.e. using integers instead of strings) via the `slab` crate;
- Left-n-right vectors for tape representation;

> [!TIP]
> The `LogicMill` class is fully pickleable, so it can be used in multiprocessing contexts.

Building
--------

Just `pip install git+https://github.com/PurpleMyst/logic_mill_rs.git`.

Thanks
------

Thank you to Kirill, original author of the Python implementation and of Marches & Gnats, for
creating such a lovely game and sharing the code!
