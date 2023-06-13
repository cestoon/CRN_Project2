# CRN_Project2

A molecular programming language

## Project Structure  

The solution is split in to multiple projects. Here is a short overview of the purpose of each.

### AST

The AST project contains types representing our Abstract Syntax Tree (AST) and functions for manipulation there of. It should be relatively small but is the basis many other projects build on.  

#### _**Important:**_  

Before making changes to this project after initial implementation, **always** discuss it with your group members. Any changes made should be made on a new branch specifically for those changes so everyone can rebase or merge them immediately without any other code.

### TypeChecker

This project does what it says, it provides the type checking function and implements all necessary checks not covered by the grammar.This may include things such as:

- Checking that conditionals only appear after a comparison (not in the same step)
- Checking for cyclical dependencies within steps
- Checking that all species/variables used have been defined either as (Doesn't seem to actually be a restriction. see Fig 9 for example of it being broken)
  - an initial concentrations (concS)
  - the result of a module or reaction in the same or a previous step
- Check the restrictions of the modules are satisfied

### Parser

The parser project is simply a collection of parser combinators that combine into a single parse function. This function turns a string into an AST.

### ParserTests

Implements the tests specified for the parser in the assignment as well as any parser specific tests we might find useful.

### State

The State project implements the `State` type mentioned in the assignment. If any functions are needed for easy use of the type they will also go here. The project is depended on by the Interpreter, Compiler and Visualizer projects. Remember to discuss any changes with group members working on those projects.

### Interpreter

The interpreter project defines a function that turns an `AST` into an infinite sequence of steps with type: `seq<State>`. The sequence is index such that the i'th element of the sequence corresponds to having computed all steps i times. This project should also be able to output graphs showing the computation over time as in Figure 4B and 6B. For more on this see the Visualization project.

### Compiler

The compiler takes an AST and turns each step into a Chemical Reaction Network (CRN). This network should be constructed in accordance with the paper. If we have time we may extend the compiler as laid out in the Discussion of the paper.

### ChemicalSimulator

This project is analogous to the Interpreter in that it transforms an input into an infinite sequence of states representing the run of the program. In this project however we use a CRN from the compiler as input and the output isn't indexed as whole cycles but instead as time steps of some unit. Like the interpreter this project also has to be able to visualize the computation, but for this module it should do so as in Figure 1 and 4. Again see the Visualization project for more.

### Visualization

This project implements functions that provides functions that transform states into charts. The way in which it does it is up to us, but it should have functions suitable for for both the interpreter and the ChemicalSimulator. It may additionally make use of the Drawing_Trees solution to make renderings of an AST.

### CompilerTests

This project holds tests for the compiler. In the project description it says to test that compiled and interpreted code produces the same output. Such tests also go here.

## Project development order  

The below chart shows which order different parts of the code needs to be developed in. A line means that the thing pointed to is strictly necessary and a dotted line means that the thing pointed to likely will influence the other project.

```mermaid
graph RL;
    Interp([Interpreter])
    ChemSim([ChemicalSimulator])
    AST([AST])
    Compiler([Compiler])
    CompilerTest([CompilerTests])
    TypeCheck([TypeChecker])
    Visual([Visualizer])
    Parser([Parser])
    ParserTests([ParserTests])
    State([State])

    Parser-->AST;

    ChemSim--> Parser;

    Visual.->Interp;

    ChemSim-->State;
    Visual--> State;
    Interp..->Parser;
    ParserTests-->Parser;
    Interp-->State;
    Interp-->AST;

    Compiler--> AST;
    TypeCheck-->AST;
    CompilerTest-->Interp;
    CompilerTest-->Compiler;

    CompilerTest-->ChemSim;
    Visual.->ChemSim;

    CompilerTest~~~Visual;
    
```
