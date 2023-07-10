# Cluna

Cluna is a tool that converts Lua code into Clue code. If you want to read more about Clue visit the repo [https://github.com/cluelang/clue](https://github.com/cluelang/clue)

## Usage
Use `cluna --help` for help on flags

## Feature support
Supported versions:
- Lua 5.1
- Lua 5.2
- Lua 5.3
- Lua 5.4


Unsupported Lua features:
- Goto and labels: Clue does not support goto statements and labels
- Lua 5.4 language features
    - Clue has no equivelant of constant variables
    - To-be-closed variables will be compiled to manual __close metamethod calls

## Installation
Binaries can be downloaded from the [releases](https://github.com/ClueLang/Clue/releases/latest)

RPM and DEB packages are available.

