# Evolutionary Game Theory: Agent-Based Simulations in R

This directory contains R scripts that run agent-based evolutionary game theory simulations. The basic structure of the simulations is as follows:

* Simulations are run over discrete "time periods".
* In each time period, agents are randomly partitioned into pairs.
* Each pair of agents plays a cooperate-defect style game (similar to, but not necessarily the same as the prisoner's dilemma), whose outcome determines each agent's chance of surviving to the next time period.
  - Each agent's "play" (decision to cooperate or defect) depends on their and their opponent's 2-period history of cooperate/defect decisions, together with that agent's "genetic strategy".
    * A genetic strategy is a binary vector that determines, in each of the `2^(2+2) = 16` possible 2-period histories for a pair of agents, what an agent's decision will be.
    * I use the term "genetic strategy" because this length-16 binary vector can be mutated, i.e. when an agent self-replicates there might be an error copying any of his 16 binary genes (so that `0000000000000000` might be replicated as `0010000000000000`).
  - In this project, the following protocol is used:
    * 0 = cooperate
    * 1 = defect
* Some agents die, according to their survival probability.
* Some of the surviving agents reproduce, i.e. self-replicate.
* When agents self-replicate, each of their 16 binary genes has a chance of being 'mutated', i.e. changed from 0 to 1 or vice-versa.

The inspiration for this project, and for many parts of the model used, comes from [this paper by Kristian Lindgren](http://publications.lib.chalmers.se/records/fulltext/140676/local_140676.pdf), which I initially discovered through [this working paper by W. Brian Arthur](http://tuvalu.santafe.edu/~wbarthur/Papers/Comp.Econ.SFI.pdf).

## Setup

To get the scripts running in R, you must:

* [Install R](https://cran.r-project.org/bin/).
* Download the `game_flexible_popn.R` and `play_flexible_popn.R` scripts from this repository.
* In R, source these two scripts:
  - On Windows, something like
    ```
    source("C:\\Users\\yourusername\\Desktop\\game_flexible_popn.R")
    source("C:\\Users\\yourusername\\Desktop\\play_flexible_popn.R")
    ```
  - On Mac, something like
    ```
    source("/Users/yourusername/Desktop/game_flexible_popn.R")
    source("/Users/yourusername/Desktop/play_flexible_popn.R")
    ```
* Then call the `game_flexible_popn()` function:
  ```
  game_stats <- game_flexible_popn(T = 1000)
  ```
* (Optional, I recommend) [Install RStudio](https://www.rstudio.com/products/rstudio/download3/)
  - This is a nice IDE for R.

