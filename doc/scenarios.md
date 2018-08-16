# Alternative Scenarios

This is a short primer for how to use some of the parameters available in the witan.send model in order to run _"alternative scenarios"_.

Each of the paramater keys (e.g. `:splice-ncy`) can be found, and their values defined, in the [config.edn](https://github.com/MastodonC/witan.send/blob/master/data/demo/config.edn).

More information on how to define a scenario can be found here (TBD). 

Here each scenario is simply defined in terms of the collection of parameters to use together, what arguments are expected and what the expected behaviuor is.

### _“Ignore historic data before a specific calendar year for an age group”_

* Parameters = `:filter-transitions-from` & `:splice-ncy`
* Arguments = a calendar year (`2016`) and national curriculum year (`11`)
* Defines a separate set of transition rates based on a defined start year and age
* For example, historic transitions may data back to 2014, but a user may only want to use data for a specific age group from 2016
Typically we see a significant change in the behaviour in those over NCY 11, as these are only recently included in SEND, so data prior to this change may not want to be included as it could skew the models results

### _“Modify setting(s) transitions rates”_

* Parameters = `:which-transitions?`, `:modify-transitions-by` and `:settings-to-change`
* Arguments = a file defining the settings rates to be modified (`"path/filename"`), a value modify the transitions by (`0.5`), and a vector containing one (`["joiners"]`) or multiple strings describing the possible type of transitions to be modified, e.g.
  * “Joiners”
  * “Leavers”
  * “Movers-to”
  * “Movers-from”
* Defines single or multiple settings and type of transitions to modify the rates of, and by how much
* For example, a user may want to modify how many individuals are joining to the setting “Mainstream” by halving the current rate.

### _“Modify setting(s) transitions rates and transfer individuals to alternative setting(s)”_

* Parameter and Arguments as described above, however additionally takes a list of settings to transfer individuals in the modified setting(s) to (see [modify-settings.csv](https://github.com/MastodonC/witan.send/blob/master/data/demo/data/modify-settings.csv))
* An example may be if a user halves the number of individuals joining “Mainstream”, they may wish to redistribute those joiners to another setting, for example “Special Independent”

### _“Modify transitions from a specific future calendar year”_

* Parameters = `:modify-transitions-from`, `:which-transitions?`, `:modify-transitions-by` and `:settings-to-change`
* Arguments = as with above two scenarios, and additionally takes a calendar year (`2020`) from when a user may wish to apply the new transition rates
* An example may be that a user only wants to start modelling a transition rate policy change in three years time and maintain the current trends until that time
