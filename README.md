# About

This is a solver for [Clues by Sam](https://cluesbysam.com). The solver part is not that interesting but the clue parsing is quite complicated (and almost tautologically incomplete).

## How to run

* Use `cargo run` in the root directory (with `cargo` installed).
* To be able to download the daily puzzle, put an API token from [browserless](https://www.browserless.io/) in a file named `browserless_api_key` in the current directory.

## Is it cheating?

Using this program should probably count as cheating, since at the very least you can use it to replace hints.

On whether the program itself is "cheating" by looking at the source, well, almost no. It's only reading the rendered html, which doesn't contain whether each suspect is innocent or criminal until you determine them as such. But there is one piece of information about the puzzle in the html but is not visually apparent: whether each suspect's statement is a logical clue or just flavor text. But even then, the program only uses this information to avoid trying to parse non-clues if you supply it with a partially solved puzzle.
