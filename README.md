# witan.send

[![CircleCI](https://circleci.com/gh/MastodonC/witan.send.svg?style=svg)](https://circleci.com/gh/MastodonC/witan.send)

## Description

Special educational needs and disability (demand and costs) model in development to be used on MastodonC's [Witan](http://www.mastodonc.com/products/witan/) city decision-making platform or as a standalone modelling library.

## Running the Model

At the top level of the send repo run:

``` bash
lein run ~/demo/config.edn
```

where `demo` is a project dir and `config.edn` is the input you'd like to use.

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

To make life easier for yourself you can add this to your `.bashrc`
customise the location of repo appropriately.

``` bash
#send
export SEND_REPO=$HOME/src/github/mastodonc/witan.send/
sendl () {
    _PWD=${PWD}
    pushd $SEND_REPO
    lein run ${_PWD}/$1
    popd
}
```

Then to execute, change to a project dir and run.

``` bash
$ cd ~/src/github/mastodonc/witan.sendruns/mastodonc/demo
$ sendl config.edn
```

### Binary

You can build a binary of the send model via

``` bash
lein bin
```

it will install `send` into `~/bin` so make sure that's in your
path. For batch runs this is important as it prevents re-compilation.

The binary is also all that anyone needs to run the model.  If you
turn off chart generation in the config then this binary should work
on all platforms.


### REPL

To use the REPL to run the model, launch and simply run

``` clojure
(-main "/home/matt/demo/config.edn")
```

edit the projects `config.edn` to alter the parameters.

### R Dependencies

This is very fragile and *will* break in the future.

For Ubuntu, get enough of an env that will allow compilation of R
packages.

``` bash
sudo apt install r-base r-cra-ggplot2 libudunits2-dev libcairo2-dev \
                 libcurl4-openssl-dev libv8-3.14-dev libgdal-dev
```

Force an install of `ggforce` package from Github master branch.

``` rscript
install.packages("devtools")
install.packages("concaveman")
devtools::install_github('thomasp85/ggforce')
```
## Features

##### Experiments with different configurations

The `multi-configs` namespace contains functionality for easily running many different model
inputs with minimal effort. You can use any number of parameters available in the 
config.edn (e.g. file inputs, numerical parameters) and all possible combinations will be created 
and  run automatically. This would be useful, for example, if you wanted to compare model results 
for many different alternative scenario parameters. Example input for the main `run-multi-configs` 
function are given in the NS.

## Tools
 
 In `utils` you will find tools for preparing a a valid states file purely from a transitions file and for making comparative plots for total count and cost for alternative runs of the model.

## License

Copyright © 2018 MastodonC Ltd

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
