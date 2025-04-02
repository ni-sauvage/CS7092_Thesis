# CS7092 Thesis - Niall Sauvage

This repository comprises of two main components: Grammars and Haskell

## Grammars
Grammars contains everything related to [BNFC](https://github.com/BNFC/bnfc) grammars in four languages

- Promela: Grammar taken from [spinroot](https://spinroot.com/spin/Man/grammar.html)
- Observation language: Grammar devised by myself, information taken from [here](https://docs.rtems.org/docs/main/eng/fv/refinement.html#annotation-refinement-guide)
- C: adapted from BNFC [example](https://github.com/BNFC/bnfc/blob/master/examples/C/C.cf).
- Propositional language: Grammar devised by myself, inspired by [Dafny](https://dafny.org/latest/Dafny-cheat-sheet.pdf)

The Promela Grammar is to be used to parse the models of RTEMS managers, while the Observation language grammar can be used to parse the output of those managers.

## Haskell
Haskell contains everything related to creating a semantics for Promela and for the Observation language, along with a toy manager prototype
- `Semantics/Obslang` models the semantics of the observation language for the `proto-sem` manager.
- `Semantics/Promela` contains a Promela semantic interpreter, which can run the `proto-sem` manager model.

## Tutorial 
### Prerequisites
- GHCi 
- BNFC Grammars tool

### Grammars
In order to make the Promela grammar, `cd` into `Grammars/Pml` and run ```bnfc -d -m Pml.cf && make```.

Next, check that the parser works. Run

```cat preprocessed/proto-sem-preprocessed.pml | Pml/Test```

Scroll up in your terminal and copy-paste the AST to clipboard. 
`cd` back to the `CS7092_Thesis directory`.
Next, run `ghci`. In order to run the interpreter, enter:

```
:l Haskell/Semantics/Promela/Promela.hs
runProgState <AST>
```

This will run your programme. Note that there are many bugs to be worked out, especially regarding inlines.
