# [run-model-iteration](https://github.com/MastodonC/witan.send/blob/bd28393b275a6ced6fdf32fbae74e13e3fcfc252/src/clj/witan/send/model/run.clj#L77)

Function is applied to every projected year, recursively

1. For the current cohort (need/setting/AY), if less than maximum possible AY, plus one to each cohorts AY
2. For each state:
    * If state is non-send do nothing (leaver will be calculated elsewhere)
    * If state AY is outside allowed age limits make a leaver
    * Else: 
        1. Get the mover state alphas for previous state AY (as we’ve already aged-on the cohort) - currently if this doesn’t exist the state is “skipped”
        2. Calculate leavers by sampling from beta-binomial with leaver beta params for previous state AY and current population
        3. Check whether current state can _"move"_ to other states
            1. If state can change apply mover params to get where individuals will move to, and minus leavers from cohort
            2. If state can't change, minus the leavers
        4. Apply new population counts to the model (and transition matrix)
3. For each AY:
    1. Calculate joiners by sampling from beta-binomial with joiner beta params and current population
        * If joiners are zero do nothing
        * Else:
            1. Calculate joiner states from Direchlet mulitnomial with joiner alphas and joiner count
            2. Apply joiners to model (and transitions matrix)



### Glossary
AY - academic year, also know as NCY (National Curriculum Year)

State - need, setting, academic year combination `[:CI-OOE 4]`

Population - count of individuals in a state

Cohort - state with associated population count `[[:CI-OOE 4] 10]`

Model - current (i.e. at any stage in a run) collection of cohorts

Mover state alphas - _these look like they are the rates of mover state transitions_ **TBC**
