# Scenarios Projections

This is a short primer for how to use some of the parameters available in the witan.send model in order to run scenarios projections.

Each of the paramater keys (e.g. `::settings-to-change`) can be found, and their values defined, in the [config.edn](https://github.com/MastodonC/witan.send/blob/master/data/demo/config.edn) file.

More information on how to define a scenario projection can be found [here](https://docs.google.com/document/d/1lQ2RrESpiyU5x2YUY8YvT287K4iq6NJw4jqHUIqC2VM/edit?ts=5b7d6444#heading=h.ebeiyry5kb3k). 

Here each scenario projection is simply defined in terms of the collection of parameters to use together, what arguments are expected and what the expected behaviuor is.

### _“Ignore historic data for a set entity pair group”_

* Parameters = `:filter-transitions-from`
* Arguments = for example, a calendar year (`2016`) and national curriculum year (`11`) and corresponding operators (e.g. `>`)
* Defines a separate set of transition rates based on a defined start year and minimum age
* For [example](https://gist.github.com/seb231/c752e3a8562017c29ea0df01f76b0169), historic transitions may date back to 2014, but a user may only want to use data for a specific age group from 2016
* Typically we see a significant change in the trends in those over NCY 11, as these are only recently included in SEND, so data prior to this change may not want to be included as it could skew the models results

### _“Modify transition state(s) rates”_

* Parameters = `:transitions-to-change`
* Arguments = a vector of map(s) each containing one or more entity keys (see below), each with corresponding keys or integers, and `:modify-transition-by` with a value corresponding to a number (integer or float) by which to multiply transitions matching the previous entity keys. Possible entity keys include:
  * `:setting-1`
  * `:setting-2
  * `:need-1`
  * `:need-2`
  * `:academic-year-1`
  * `:academic-year-2`
  * `:calendar-year`
* Defines single or multiple transition states to modify the rates of, and by how much
* For [example](https://github.com/MastodonC/witan.send/blob/83cacebd36053a6e74f94ea36cdadac98cd8335a/data/demo/config-transitions-to-change.edn), a user may want to modify how many individuals are joining and moving to to the setting “A” in academic year 1 by increasing the current rate 40 fold.

### _“Modify transitions from a specific future calendar year”_

* Parameters = `:modify-transitions-from` and `:transitions-to-change
* Arguments = as with above scenario, and additionally a calendar year (`2020`) from when a user may wish to apply the new transition rates
* An [example](https://gist.github.com/seb231/0218cb773df526e4e99b992db028703d) may be that a user only wants to start modelling a transition rate policy change in three years time and maintain the current trends until that time

### _“Make a setting invalid”_

* Parameters = `:make-setting-invalid` and `:transitions-to-change`
* Arguments = `:make-setting-invalid` expects a setting name as a key and `:transitions-to-change` must take a map whereby `:setting-2` equals the setting to make invalid and `:modify-transition-by` equals `0`.
