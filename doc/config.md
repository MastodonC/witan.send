# Setting up a config file

The config is an .edn file, essentially a map (`{}`) containing keys (`:foo`) and associated values. There are five sections to the config, defined as maps within the larger config map.

As example config.edn can be found [here](https://github.com/MastodonC/witan.send/blob/master/data/demo/config.edn).

### `:file-inputs`

Each key refers to a specific input file and requires a string value (`"foo.csv"`) defining a filepath where the input data will be found. This will typically be a subdirectory to where the config file is stored.

There are four required keys and one optional key.

The required keys are:
- `:transitions`
- `:population`
- `:costs`
- `:valid-states`

The single optional key is `:settings-to-change` and is required when wanting to project a [“Modify setting(s) transitions rates”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-settings-transitions-rates), [“Modify setting(s) transitions rates and transfer individuals to alternative setting(s)”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-settings-transitions-rates-and-transfer-individuals-to-alternative-settings) or [“Modify transitions from a specific future calendar year”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-transitions-from-a-specific-future-calendar-year) scenario (more information in links).

Description of what each file should contain can be found [here](https://docs.google.com/document/d/138mSLMwTnH5ev1z0po07qGPxcfvuVkjR0ax8Yo88724/edit#) and example files can be found [here](https://github.com/MastodonC/witan.send/tree/master/data/demo/data).

### `:transition-parameters`

The transition parameters are all optional, used for running scenario projections.

##### `:filter-transitions-from`

Expects a map consisting of keys corresponding to a set list of possible entity combinations (see below) and inner maps indicating which transitions to remove by an operator key and a value. For example to filter by everything earlier than 2016 and over and including academic year 11:

`{:calendar-academic {:< 2016 :>= 11}}`

The possible entity pair keys include:
* `:calendar-academic`
* `:calendar-setting`
* `:calendar-need`
* `:academic-setting`
* `:academic-need`
* `:setting-need`
`
The inner maps contents should correspond to the order of the outer key, so for `{:< 2016 :>= 11}` the first, key-value pair refers to `calendar-year` and the second refers to `academic-year`.

Operator keys include:
* `>` - more than
* `>=` - more than or equal to
* `<` - less than
* `<=` - less than or equal to
* `=` - equal to

Multiple entity pair filters can be applied in parallel.

To be used for a [“Ignore historic data before a specific calendar year for an age group”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#ignore-historic-data-before-a-specific-calendar-year-for-an-age-group) scenario.

##### `:which-transitions?`

Expects a vector (`["foo"]`) of either one or more transition types stored as strings i.e. "joiners", "movers-to", "movers-from" or "leavers", to be used in conjunction with `:modify-transition-by` and `:setting-to-change` parameter with which to define a [“Modify setting(s) transitions rates”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-settings-transitions-rates), [“Modify setting(s) transitions rates and transfer individuals to alternative setting(s)”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-settings-transitions-rates-and-transfer-individuals-to-alternative-settings) or [“Modify transitions from a specific future calendar year”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-transitions-from-a-specific-future-calendar-year) scenario.

##### `:modify-transition-by`

Expects an integer to multiply a list of settings by (provided with `:settings-to-change`). For example here a `2` will multiply the transition rate for a list of settings by two, thus doubling the historic number of transitions by.

##### `:modify-transitions-from`

Expects a calendar year, provided as an integer, to start modifying transitions from, when `:modify-transition-by` & `:which-transitions?` are defined for either a [“Modify setting(s) transitions rates”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-settings-transitions-rates), [“Modify setting(s) transitions rates and transfer individuals to alternative setting(s)”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-settings-transitions-rates-and-transfer-individuals-to-alternative-settings) or [“Modify transitions from a specific future calendar year”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-transitions-from-a-specific-future-calendar-year) scenario.

### `:projection-parameters`

Contains two required keys for every projection.

The three required keys, `:random-seed` and `:simulations` define how the model will be run.

##### `:random-seed`

Expects and integer with which to randomly seed the model with and enable repeatability.

##### `:simulations`

Expects an integer corresponding to a number with which to repeat the model over _n_ number of times. Due to parallelisation, the minimum number of simulations is eight. In tests 1000 simulations typically provides sufficient depth to normalise the results.

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

##### `:population-file`

Takes either a path to a population file or `ref [:file-inputs :population]` which duplicates the path to the population file in `:file-inputs`. For use with the general population chart.

### `:validation-parameters`

Contains two required keys, both booleans, for whether to run the validation of the SEND model on the data specified in the `:file-inputs` with the `:run-validation` key, and whether to keep the temporary files generated by the validation with the `:keep-temp-files?` key.
