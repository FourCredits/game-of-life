# Game of Life

The Game of Life, written in Haskell. Uses gloss for the GUI components.

This project is built using stack.

## Usage

To run:

```sh
stack build
stack exec gol
```

This will give you a grid. Click on squares to turn them on or off, press 'p' to
pause or unpause the game, and press 'r' to reset.

If you want to change some aspect of the game's behaviour, you can look in
`src/Configuration.hs` for various parameters you can tweak.

To install to your system:

```sh
stack install gol
```
