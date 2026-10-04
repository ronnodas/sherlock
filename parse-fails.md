# Known/expected failures to parse

If a clue starts with `!` that means it's misparsed, rather than failing to parse at all.

| date       | speaker | clue                                                                         | reason             |
| ---------- | ------- | ---------------------------------------------------------------------------- | ------------------ |
| 2025-05-19 | Gary    | One of the cooks poisoned my food!                                           | unique             |
| 2025-05-20 | Wally   | All sleuths are actually a criminal gang in disguise!                        | unique             |
| 2025-05-22 | Katie   | The only criminal to the right of Logan is definitely not Noah               | flavor             |
| 2025-05-25 | Larry   | Either Oscar or Paula is criminal.                                           | ambiguous          |
| 2025-05-27 | Carol   | Every second person in my row is innocent. I just got unlucky.               | unique             |
| 2025-05-31 | Clyde   | I heard Zach and Isaac plotting with the criminals between them              | unique             |
|            | Nancy   | Alice is not the only criminal sleuth, I'll tell you that!                   | ambiguous + flavor |
| 2025-06-01 | Anna    | Hi, my name is Anna. I have two criminal neighbors, and I hate karaoke.      | flavor             |
|            | Nick    | The guy below me is a criminal. Please take him and his karaoke set away!    | indirect           |
| 2025-06-04 | Xena    | Either Nicole or Susan is a criminal                                         | ambiguous          |
| 2025-06-05 | Olivia  | Floyd has only one criminal on his row                                       | indirect           |
|            | Tyler   | !There is one criminal hiding in a corner                                    | flavor             |
| 2025-06-06 | Ellie   | Paul is connected to 7 criminals. Not me. I don't like his ego.              | connected          |
| 2025-06-13 | Julie   | Flora is my friend. She's definitely innocent.                               | flavor             |
| 2025-06-15 | Karen   | Sam's mom is not one of the two criminals above Zach                         | unique             |
|            | Wanda   | The only criminal below Amy is definitely not Sam                            | flavor             |
| 2025-06-16 | Sarah   | Everyone next to two guards is a criminal                                    | unique             |
| 2025-06-17 | Gus     | Column A has more criminals than any other column or row                     | unique             |
| 2025-06-18 | Brian   | Two cooks have two criminals to the right of them                            | unique             |
| 2025-06-19 | Betty   | One row has as many criminals as all other rows combined                     | unique             |
|            | Phil    | There's at least one innocent below Donna, but it's not Xia                  | negative           |
| 2025-06-21 | Isaac   | The only criminal above Vicky is not a farmer                                | negative           |
|            | Olof    | I'd be lying if I told you Mary is a criminal                                | unique             |
| 2025-06-22 | Isaac   | Tyler has more innocent neighbors than the person above me                   | indirect           |
|            | Peter   | The remaining criminal is closer to Nancy than Maria                         | ambiguous          |
| 2025-06-23 | Isaac   | Tom is neither of the 2 criminals on row 4                                   | negative           |
|            | Sam     | There's no one between the 2 criminals above me. They're "connected."        | unique             |
| 2025-06-24 | Denis   | Oscar is neither of the two innocents neighboring Nancy                      | negative           |
| 2025-06-25 | Larry   | Either Cheryl and Gary are both innocent, or Cheryl is a criminal            | unique             |
| 2025-06-27 | Donna   | Isaac is just as innocent as Vicky. So not very.                             | unique             |
|            | Eve     | Wanda is my buddy. She's just as innocent as I am.                           | unique             |
|            | Vicky   | The singer closest to me is just as innocent as Wanda                        | unique             |
|            | Xena    | There are no criminals next to each other on my row                          | unique             |
| 2025-06-28 | Julie   | I have one criminal to the right of me, but it's not Mary                    | negative           |
|            | Olof    | There are no connected innocents in column A                                 | unique             |
| 2025-06-29 | Olivia  | I saw some criminal activity to the left of me. There was two of them!       | unique             |
| 2025-07-01 | David   | Carol has the painting! She's one of 9 criminals on the edges                | flavor             |
| 2025-07-03 | Zena    | Only one of Ruth's criminal neighbors is closer to Lisa than to me           | ambiguous          |
| 2025-07-04 | Jimmy   | There are no innocents neighboring each other in my column                   | flavor             |
| 2025-07-05 | Diane   | !There are no criminal neighbors in column D                                 | unique             |
|            | Sofia   | A crime boss? All I know is that the only innocent mech is Frank's neighbor. | flavor             |
|            | Xavi    | Will you let me go if I tell you Olivia is a criminal? Well, she is!         | flavor             |
| 2025-07-08 | Isaac   | Innocents in column C are 4 steps away from each other                       | unique             |
|            | Ruth    | No innocent is between two criminals in column A                             | unique             |
| 2025-07-09 | Mark    | Exactly half of us are criminals. The other half are losers.                 | unique             |
| 2025-07-10 | Wanda   | Mary is not the only criminal in row 3                                       | ambiguous          |
| 2025-07-11 | Olivia  | Either Eric or Janet is innocent                                             | ambiguous          |
| 2025-07-13 | Denis   | Mary is not the only innocent in row 3                                       | ambiguous          |
| 2025-07-14 | Isaac   | Freya is not the only innocent to the left of Helen                          | ambiguous          |
|            | Scott   | Helen is not the only criminal between Denis and Zed                         | ambiguous          |
|            | Xena    | Jane is not the only innocent to the left of Laura                           | ambiguous          |
| 2025-07-15 | Xavi    | Criminals below Gary are not connected                                       | ambiguous          |
| 2025-07-27 | Nicole  | I've known Adam for a long time. He's as innocent as a sheep.                | unique             |
| 2025-07-28 | Nick    | There is an innocent between Freya and Isaac                                 | ambiguous          |
| 2025-08-05 | David   | Rose is innocent for sure                                                    | flavor             |
| 2025-08-09 | Zena    | The only innocent above me is closer to Floyd than to Bruce                  | ambiguous          |
| 2025-08-11 | Amy     | Ben is not the only criminal to the right of me                              | ambiguous          |
| 2025-08-25 | Floyd   | Criminals below Chris are not connected                                      | ambiguous          |
| 2025-09-16 | Carol   | The person directly below Nancy is innocent                                  | indirect           |
|            | Nancy   | I don't have more innocent neighbors than Diane                              | negative           |
| 2025-09-22 | Alice   | Igor's diagonal neighbours are all criminals                                 | unique             |
|            | Igor    | The person directly above Freya is a criminal                                | indirect           |
| 2025-09-23 | Ruth    | The one singer neighboring both me and Ollie is criminal                     | unique             |
|            | Noah    | Isaac is innocent only if all other singers are innocent                     | unique             |
| 2025-09-25 | Zed     | The builder between two other builders is a criminal                         | unique             |
| 2025-10-02 | Peter   | If and only if I'm innocent then so is Vicky                                 | unique             |
| 2025-10-08 | Isaac   | Innocents in row 1 are not connected                                         | ambiguous          |
| 2025-10-21 | Hilda   | There are as many criminal builders as criminal... pumpkins?                 | flavor             |
| 2025-10-29 | Evie    | Julie is one of the 2 innocent non-pumpkins                                  | negative           |
| 2026-04-06 | Betty   | I'm one of 2 innocents neighboring Floyd in row 1                            | unique             |

## Reasons

- **ambiguous**: the clue is ambiguous, which can be for a few reasons:
  - "X is not the only Y" is ambiguous because it could mean X is one of multiple Y, or that there is exactly one Y, and it is different from X.
  - "Either ... or ..." is ambiguous because the "or" could be inclusive or exclusive. Other clues seem to force exclusivity in each encountered instance.
  - "... are not connected" is ambiguous because it could mean the negation of connected, or totally disconnected, ie no two are connected.
  - "There is an innocent ..." is ambiguous because it could mean at least one or exactly one.
  - "closer" is ambiguous because a metric hasn't been specified.
- **connected**: the clue uses the connected components in some non-trivial way
- **flavor**: the clue is combined with flavor text in a way that is difficult or inconvenient to programmatically separate
- **indirect**: the clue refers to some component in an unnecessarily indirect way:
  - "the person below X", instead of by name
  - "on his row", instead of "row 1"
- **negative**: the clue negates some component, these will theoretically be supported in the future
- **unique**:
  - the clue uses something special about the grid or flavor text
  - the clue is phrased in a way that is unlikely to be generalized
  - the clue has a phrasing that hasn't been reused, and would require non-trivial new logic
