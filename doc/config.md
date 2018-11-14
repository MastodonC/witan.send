# Setting up a config file

The config is an .edn file, essentially a map (`{}`) containing keys (`:foo`) and associated values. There are five sections to the config, defined as maps within the larger config map. 

As example config.edn can be found [here](https://github.com/MastodonC/witan.send/blob/master/data/demo/config.edn).

### `:file-inputs`

Each key refers to a specific input file and requires a string value (`"foo.csv"`) defining a filepath where the input data will be found. This will typically be a subdirectory to where the config file is stored.

There are four required keys and one optional key. 

The required keys are:
- `:transition-matrix`
- `:population`
- `:setting-cost`
- `:valid-setting-academic-years`

The single optional key is `:settings-to-change` and is required when wanting to project a [“Modify setting(s) transitions rates”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-settings-transitions-rates), [“Modify setting(s) transitions rates and transfer individuals to alternative setting(s)”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-settings-transitions-rates-and-transfer-individuals-to-alternative-settings) or [“Modify transitions from a specific future calendar year”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-transitions-from-a-specific-future-calendar-year) scenario (more information in links).

Description of what each file should contain can be found [here](https://docs.google.com/document/d/138mSLMwTnH5ev1z0po07qGPxcfvuVkjR0ax8Yo88724/edit#) and example files can be found [here](https://github.com/MastodonC/witan.send/tree/master/data/demo/data).

### `:transitions-parameters`

The transitions parameters are all optional parameters, used for running scenario projections.

##### `:filter-transitions-from`

Expects a calendar year stored in a vector (e.g. `[2016]`). Sets a year to filter historic transitions from (e.g. for `[2016]` all transitions data prior to 2016 will be ignored. To be used with with `:splice-ncy` for a [“Ignore historic data before a specific calendar year for an age group”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#ignore-historic-data-before-a-specific-calendar-year-for-an-age-group) scenario.

##### `:splice-ncy`

NCY = National Curricullum Year. 

Expects an integer corresponding to the national curricullum year/academic year to filter from (typically between -5 and 20). To be used in conjunction with `:filter-transitions-from` for a [“Ignore historic data before a specific calendar year for an age group”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#ignore-historic-data-before-a-specific-calendar-year-for-an-age-group) scenario. Currently will filter the provided NCY and all years above (e.g. for 16, NCYs 16-20 will be filtered).

##### `:which-transitions?`

Expects a vector (`["foo"]`) of either one or more transition types stored as strings i.e. "joiners", "movers-to", "movers-from" or "leavers", to be used in conjunction with `:modify-transition-by` and `:setting-to-change` parameter with which to define a [“Modify setting(s) transitions rates”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-settings-transitions-rates), [“Modify setting(s) transitions rates and transfer individuals to alternative setting(s)”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-settings-transitions-rates-and-transfer-individuals-to-alternative-settings) or [“Modify transitions from a specific future calendar year”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-transitions-from-a-specific-future-calendar-year) scenario.

##### `:modify-transition-by`

Expects an integer to multiply a list of settings by (provided with `:settings-to-change`). For example here a `2` will multiply the transition rate for a list of settings by two, thus doubling the historic number of transitions by.

### `:run-parameters`

Contains three required and one optional key.

The three required keys, `:random-seed`, `:simulations` and `:seed-year` define how the model will be run. `:modify-transitions-from` is optional and used in conjunction with `:transitions-parameters`.

##### `:random-seed`

Expects and integer with which to randomly seed the model with and enable repeatability.

##### `:simulations`

Expects an integer corresponding to a number with which to repeat the model over _n_ number of times. Due to parallelisation, the minimum number of simulations is eight. In tests 1000 simulations typically provides sufficient depth to normalise the results.

##### `:seed-year`

Expects a calendar year, provided as an integer with which to project the model from.

##### `:modify-transitions-from`

Expects a calendar year, provided as an integer, to start modifying transitions from, when `:modify-transition-by` & `:which-transitions?` are defined for either a [“Modify setting(s) transitions rates”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-settings-transitions-rates), [“Modify setting(s) transitions rates and transfer individuals to alternative setting(s)”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-settings-transitions-rates-and-transfer-individuals-to-alternative-settings) or [“Modify transitions from a specific future calendar year”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-transitions-from-a-specific-future-calendar-year) scenario.

### `:output-parameters`

Contains four required and two additional optional keys pertainting to what the model should output.

##### `:run-outputs`

Required key, expects a boolean where `true` will output the results of the model as csv files.

##### `:run-charts`

Required key, expects a boolean where `true` will output plots of the output. Requires `:run-outputs` to also equal `true`.

##### `:output-dir`

Required key, expects a path as a string where all outputs will be stored if `:run-outputs` and `:run-charts` are `true`. A new directory will be created within the the directoy where the config file is stored.

##### `:keep-temp-files?`

Required key, expects a boolean where `true` will keep temporary data files used for plotting results in R. Can be used when debugging or writing bespoke R plots.

##### `:settings-to-exclude-in-charts`

Optional key, expects a string of comma-delimited settings to exclude from all plotting associated with SEND settings.

##### `:use-confidence-bound-or-interval`

Optional key, expects the string "interval", to use confidence interval in total population plotting, otherwise will use simple bounds.

### `:validation-parameters`

Contains two required keys, both booleans, for whether to run the validation of the SEND model on the data specified in the `:file-inputs` with the `:run-as-default` key and whether to keep the temporary files generated by the validation with the `:keep-temp-files?` key.
