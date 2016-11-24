A chess engine and simple GUI, written in Haskell and using the TK framework.

This was written for my senior thesis. While performance is of interest to me, it's not my top goal. I'm more interested in the evaluation function than in doing stuff with bitboards.

To run, first make sure you have `Tk` installed on your system and then install the Nyx package with `cabal`.

This implementation isn't fully finished or polished-- the search function isn't efficient enough, non-Queen promotions aren't supported, and draw-by-repetition isn't implemented.

![Screenshot](https://i.imgur.com/exoJm1n.png)
