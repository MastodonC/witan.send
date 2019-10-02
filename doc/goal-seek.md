# Implementing goal-seek

Goal-seek allows one to search for the best model config parameters with which to achieve a certain subpopulation within the SEND projection.

Goal-seek will run the SEND model multiple times with the same underlying data but varying the scenario parameters by input values, stopping when it hits your target population size.

## Usage

In the REPL, call `target-results` providing it with six arguments:

1. A map partially matching a transition to be modified, for more information see [here](https://github.com/MastodonC/witan.send/blob/feature/goal-seek/doc/config.md#transitions-to-change)

2. A value to start transitions modification at. This value multiplies the number of transitions, so 0.5 would halve the number of matching transitions and 2 would double them. Typically the starting value would be 1 (e.g. no multiplier).

3. A value to stop the transition modificaiton at. When the model has been run this many times (- 1, due to zero indexing), goal-seek will cease looping. This value is somewhat arbitary if you are somewhat confident in the range of values to provide goal-seek.

4. A value by which to increment by when modifying transitions. A value of 1 here, and 1 and 10 will run the model ten times.

5. A base config must be provided, which will then be modified with the preceeding information. The base config will provide information such as where the baseline input data is located and for how many iterations the model should run.

6. A map consisting of a target population number and a calendar year (e.g. `{:year 2020 :population [40 50]}`). Goal-seek will have found an intial optimum projection when the range of values provided here for `:population` for the corresponding calendar year, match the transition to modify as described in `1.`. Here there is a trade off between making this range quite wide, and reduce upon subsequent goal-seek run, else provide `4.` above with a finer scale incrementor and do more runs.
