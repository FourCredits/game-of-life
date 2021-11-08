# Game of Life

The Game of Life, written in Haskell. Uses gloss for the GUI components.

This project is built using stack.

## Usage

To run:

```sh
stack build
stack exec gol 50 50
```

This will give you a 50x50 grid. Click on squares to turn them on, then press
'p' to start the simulation.

To install to your system:

```sh
stack install gol
```
