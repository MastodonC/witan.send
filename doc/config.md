# Setting up a config file

The config is an .edn file, essentially a map (`{}`) containing keys (`:foo`) and associated values. There are five sections to the config, defined as maps within the larger config map. 

As example config.end can be found [here](https://github.com/MastodonC/witan.send/blob/master/data/demo/config.edn).

### `:file-inputs`

Each key refers to a specific input file and requires a string value (`"foo.csv"`) defining a filepath where the input data will be found. This will typically be 

There are four required keys and one optional key. 

The required keys are:
- `:transition-matrix`
- `:population`
- `:setting-cost`
- `:valid-setting-academic-years`

The single optional key is `:settings-to-change` and is required when wanting to project a [“Modify setting(s) transitions rates”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-settings-transitions-rates), [“Modify setting(s) transitions rates and transfer individuals to alternative setting(s)”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-settings-transitions-rates-and-transfer-individuals-to-alternative-settings) or [“Modify transitions from a specific future calendar year”](https://github.com/MastodonC/witan.send/blob/master/doc/scenarios.md#modify-transitions-from-a-specific-future-calendar-year) scenario (more information in links).

Description of what each file should contain can be found [here](https://docs.google.com/document/d/138mSLMwTnH5ev1z0po07qGPxcfvuVkjR0ax8Yo88724/edit#) and example files can be found [here](https://github.com/MastodonC/witan.send/tree/master/data/demo/data).

### `:transitions-parameters`

### `:run-parameters`

### `:output-parameters`

### `:validation-parameters`
