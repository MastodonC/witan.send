# witan.send

[![CircleCI](https://circleci.com/gh/MastodonC/witan.send.svg?style=svg)](https://circleci.com/gh/MastodonC/witan.send)

## Description

Special educational needs and disability (demand and costs) model in development to be used on MastodonC's [Witan](http://www.mastodonc.com/products/witan/) city decision-making platform or as a standalone modelling library.

## Running the Model

At the top level of the send repo runs

``` bash
lein run ~/demo
```

where `demo` is a project dir.

``` bash
├── config.edn
├── data
│   ├── modify-settings.csv
│   ├── need-setting-costs.csv
│   ├── population.csv
│   ├── transitions.csv
│   └── valid-setting-academic-years.csv
└── results-180718
    ├── historic-data.csv
    ├── Output_AY.csv
    ├── Output_AY_Group.csv
    ├── Output_AY_State.csv
    ├── Output_Cost.csv
    ├── Output_Count.csv
    ├── Output_Need.csv
    ├── Output_Setting.csv
    ├── SEND_report.md
    ├── transitions.edn
    └── valid-settings.csv
```

The `config.edn` file looks like this

``` edn
{:file-inputs {:settings-to-change "data/modify-settings.csv" 
               :transition-matrix "data/transitions.csv" 
               :population "data/population.csv"
               :setting-cost "data/need-setting-costs.csv" 
               :valid-setting-academic-years "data/valid-setting-academic-years.csv"}

 :transition-parameters {:filter-transitions-from nil
                         :which-transitions? nil
                         :splice-ncy nil
                         :modify-transition-by 1}

 :run-parameters {:modify-transitions-from nil
                  :random-seed 50
                  :simulations 10
                  :seed-year 2017}

 :output-parameters {:run-report-header true
                     :run-reports true
                     :run-charts false
                     :output-dir "results-180718"}}
```

Alter this file to change the configuration. Particularly the
`:output-dir` key to save new runs in a different location.

### REPL

To use the REPL to run the model, launch and simply run

``` clojure
(-main "/home/matt/demo")
```

edit the projects `config.edn` to alter the parameters.

## License

Copyright © 2017 MastodonC Ltd

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
