# Implementing goal-seek

Goal-seek allows one to search for the best modifier config parameters with which to achieve a certain subpopulation within the SEND projection. See ["transtions to change"](config.md#transitions-to-change) for more information on applying transition modifiers.

Goal-seek can run the SEND model multiple times with the same underlying data but varying the scenario parameters by input values, stopping when it hits the target subpopulation size.

## Usage

There are two ways of using goal-seek; either for a single run to test a modifier input (`target-result`), or multiple runs whereby a string of modifiers are tested (`target-results`). For multiple runs goal-seek will stop when it achieves or exceeds the target population. 

### `target-result`

In the REPL, call `target-results` providing it with at least two arguments:

1. A base config, with which to be modified. The base config will provide information such as where the baseline input data is located and for how many iterations the model should run. N.B. See [here](config.md) for more information on building a config (or config file). Note that using `main/read-config` will allow one to load a config in the REPL while in the goal-seek namespace.

2. A target year is the calendar year in which the the population we seek is present. This is mainly used in conjunction with `target-results`, but allows this function to return the population count for the target year.

3. A map partially matching a transition to be modified, for more information see [here](config.md#transitions-to-change) (Optional).

### `target-results`

In the REPL, call `target-results` providing it with at least three arguments:

1. A base config, with which to be modified. The base config will provide information such as where the baseline input data is located and for how many iterations the model should run. N.B. See [here](config.md) for more information on building a config (or config file). Note that using `main/read-config` will allow one to load a config in the REPL while in the goal-seek namespace.

2. A map consisting of a target population range and a calendar year (e.g. `{:year 2020 :population [40 50]}`). Goal-seek will have found an intial optimum projection when the range of values provided here for `:population` for the corresponding calendar year, match the transition to modify described in `3.`. Here there is a trade off between making this range quite wide, and reduce upon subsequent goal-seek run, else provide `4.` above with a finer scale incrementor and do more runs.

3. A map partially matching a transition to be modified, for more information see [here](config.md#transitions-to-change)

4. A value by which to increment by when modifying transitions (Optional).
