# Alternative Scenarios

This is a short primer for how to use some of the parameters available in the witan.send model in order to run _"alternative scenarios"_.

Each of the paramater keys (e.g. `:splice-ncy`) can be found, and their values defined, in the [config.edn](https://github.com/MastodonC/witan.send/blob/master/data/demo/config.edn).

More information on how to define a scenario can be found here (TBD). 

Here each scenario is simply defined in terms of the collection of parameters to use together, what arguments are expected and what the expected behaviuor is.

### _“Ignore historic data before a specific calendar year for an age group”_

* Parameters = `:filter-transitions-from` & `:splice-ncy`
* Arguments = a calendar year (`2016`) and national curriculum year (`11`)
* Defines a separate set of transition rates based on a defined start year and minimum age
* For [example](https://gist.github.com/seb231/c752e3a8562017c29ea0df01f76b0169), historic transitions may date back to 2014, but a user may only want to use data for a specific age group from 2016
* Providing `:splice-ncy` with 11 filters anyone over that age
* Typically we see a significant change in the trends in those over NCY 11, as these are only recently included in SEND, so data prior to this change may not want to be included as it could skew the models results

### _“Modify setting(s) transitions rates”_

* Parameters = `:which-transitions?`, `:modify-transitions-by` and `:settings-to-change`
* Arguments = a file defining the settings rates to be modified (see [modify-settings1.csv](https://github.com/MastodonC/witan.send/blob/master/data/demo/data/modify-settings1.csv)), a value modify the transitions by (`0.5`), and a vector containing one (`["joiners"]`) or multiple strings describing the possible type of transitions to be modified, e.g.
  * “Joiners”
  * “Leavers”
  * “Movers-to”
  * “Movers-from”
* Defines single or multiple settings and type of transitions to modify the rates of, and by how much
* For [example](https://gist.github.com/seb231/b994bc040ed967e136424b623f165403), a user may want to modify how many individuals are joining to the setting “Mainstream” by halving the current rate.

### _“Modify setting(s) transitions rates and transfer individuals to alternative setting(s)”_

* Parameter and Arguments as described above, however additionally takes a list of settings to transfer individuals in the modified setting(s) to (see [modify-settings2.csv](https://github.com/MastodonC/witan.send/blob/master/data/demo/data/modify-settings2.csv))
* An [example](https://gist.github.com/seb231/b994bc040ed967e136424b623f165403) may be if a user halves the number of individuals joining “Mainstream”, they may wish to redistribute those joiners to another setting, for example “Special Independent”

### _“Modify transitions from a specific future calendar year”_

* Parameters = `:modify-transitions-from`, `:which-transitions?`, `:modify-transitions-by` and `:settings-to-change`
* Arguments = as with above two scenarios, and additionally takes a calendar year (`2020`) from when a user may wish to apply the new transition rates
* An [example](https://gist.github.com/seb231/0218cb773df526e4e99b992db028703d) may be that a user only wants to start modelling a transition rate policy change in three years time and maintain the current trends until that time
