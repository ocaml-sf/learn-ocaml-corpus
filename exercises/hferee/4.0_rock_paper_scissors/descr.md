# Rock-Paper-Scissors

The following sum types describe the three possible moves in the game "Rock-Paper-Scissors" and the three possible outcomes.

```ocaml
type move = Rock | Paper | Scissors

type outcome = Victory | Defeat | Draw
```

---

**Question 1**:

Define a function `round: move -> move -> outcome` that, given the move of a player and the move of their opponent, determines the outcome of a round from the player's perspective.

---

The following type describes the scoreboard for two players, with the number of victories for each.

```ocaml
type scoreboard = { player: int; opponent: int }
```

In the top-level, verify that you can construct an object of type `scoreboard` using the following syntax:

```ocaml
{ player = 0; opponent = 0 }
```

---

**Question 2**:

Define a function `score: scoreboard -> outcome -> scoreboard` that takes a scoreboard as input and returns a new scoreboard with the scores modified based on a round's outcome (e.g., a victory increments the player's score).

---

**Bonus (untested)**:

Define a function that plays two players randomly for three rounds and returns the final result. You can use the `Random.int` function.

---

**Bonus (untested)**:

Adapt this game to the "Rock-Paper-Scissors-Lizard-Spock" version. Think about an approach that does not explode the number of cases in the `round` function.
