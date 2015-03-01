# evol_pris_dilem
Evolutionary game theory simulation in R. First model (agents are randomly paired off and play a 2-person prisoner's dilemma every period).

In order to run simulations, source the relevant files (**game_xx.R**, **play_xx.R** and **mutate_xx.R**), then for some positive integers *N* and *T*, type **game_xx(*N*,*T*)** in the command line and hit enter. *N* determines population (population = 2 x *N*) and *T* is the number of periods in the simulation.

Note that **play_own_history.R** corresponds to both **game_own_history.R** and **game_OH_no_evol.R**, so if you want to run **game_OH_no_evol.R**, source **play_own_history.R** since there is no play_OH_no_evol.R.

Some terminology I use (abuse) is defined here:
* *strategy gene* - a player's choice of play (i.e. cooperate or defect) given a specific history (in most cases, players look at four historic plays and can choose to cooperate (0) or defect (1) in each of these scenarios; so, they have 4<sup>2</sup> = 16 strategy genes). The 16 strategy genes taken together constitute a player's "type" or "species".
* *type* or *species* - a specific set of 16 strategy genes. In the code, this is represented as a 16-digit binary number (where each 0 denotes cooperation and each 1 denotes defection) or, when the code prints information every 1000 periods, as the corresponding base 10 number.
* *mutate* - in this model, mutation is when one of a player's strategy genes is changed from 0 (cooperate) to 1 (defect) or vice-versa. In some versions of the code, mutated players might also "evolve" to be able to look back one period further or "devolve" (simply the opposite of "evolve"; yes, this is very poor nomenclature).

The different file name suffixes denote differences in the mechanics of the model:
* **own_history** or **OH**:
  * **Not in file name**: a player's strategy depends only on her opponent's history of plays (in particular, her opponent's four most recent plays).
  * **In file name**: a player's strategy depends both on her own and her opponent's history of plays (in particular, her own and her opponent's two most recent plays; four plays in total).
* **no_evol** or **NE**:
  * **Not in file name**: players have "levels of evolution" which determine how far back they look when choosing their plays (all players start at level 0, i.e. they either always cooperate or always defect; then "evolve" to level 1, where their strategy depends only on their opponent's most recent play; and can "evolve" or "devolve" between levels 0 and 4). Players only "evolve" or "devolve" if they are randomly mutated.
  * **In file name**: there are no "levels of evolution". All players can see all four historical plays, and so mutation only changes one strategy gene at a time (whereas with levels of evolution and, e.g. a level 1 player, mutation will change either the 8 strategy genes corresponding to an opponent who cooperated in the most recent period or the 8 strategy genes corresponding to an opponent who defected in the most recent period).
* **misinfo** or **MI**:
  * **Not in file name**: players are always right about their own and their opponents' histories.
  * **In file name**: each player has a chance of being wrong about one or more of her opponent's plays, and possibly one or more of her own plays (depending on the version of the code).

If you're reading this, I hope you enjoy running a simulation or reading through the code!

For a first run, I suggest you source **game_OH_no_evol.R**, **play_own_history.R** and **mutate_OH_no_evol.R** and run **game_OH_no_evol(1000,10000)** in the command line.

Currently, this code produces no graphs. This is a huge shortcoming, as the output is unintelligible unless you have a good understanding of the model. I plan on adding some automated graphs soon (as of 2/10/2015).

Thanks for your interest!

**Robert Sharp**
**bob.wynbert.sharp@gmail.com**
