# maltese

`maltese` is a symbolic execution engine for circuits written in the [Chisel](https://github.com/chipsalliance/chisel3) language.
In order to use `maltese` you need to export your Chisel circuit into the [FIRRTL](https://github.com/chipsalliance/firrtl)
format. Then you can load your circuit using the `SymbolicSim.loadFirrtl` function.

`maltese` uses the `firrtl` compiler in order to translate the circuit to a formal transition system which can
be executed on concrete as well as symbolic values. This can be useful for reverse-engineering or verifying data paths.
You can find an example of this in the `test/chiseltest/symbolic` folder which shows how to extract a symbolic
expression for how the execution of a RISC-V `ADD` instruction changes the register file content.

`maltese` also supports many important memory features such as initialization from a file or with a constant value.
`maltese` is still work in progress, in particular we are trying to improve the performance of the symbolic engine
and add better mechanisms for debugging and improve usability.

Please file an issue if you encounter any problems or bugs.
