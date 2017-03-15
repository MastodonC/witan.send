# witan.send

[![CircleCI](https://circleci.com/gh/MastodonC/witan.send.svg?style=svg)](https://circleci.com/gh/MastodonC/witan.send)

## Description

Special educational needs and disability (demand and costs) model in development to be used on MastodonC's [Witan](http://www.mastodonc.com/products/witan/) city decision-making platform or as a standalone modelling library.


## Development tools

When combining functions into a model there are useful tools to take advantage of, thanks to dependencies for `witan.workspace-executor` and `witan.workspace-api`.

#### To visualise a model workflow, you need to:

1) Install `Graphviz`:

- Ubuntu: `$ sudo apt-get install graphviz`

- MacOS: `$ brew install graphviz`

For any OS you should also be able to install it with "pip": `$ pip install graphviz`.

2) Use the `view-workflow` function using the household model workflow (hh-model-workflow)
as follows:

```Clojure
(witan.workspace-executor.core/view-workflow hh-model-workflow)
```
#### To print logs, use the `set-api-logging!` function:

```Clojure
(witan.workspace-api/set-api-logging! println)
```
Whenever a `defworkflowfn` is called logs will be written to your repl or terminal. It's very  useful for debugging purpose.

Turn it off with:
```Clojure
(witan.workspace-api/set-api-logging! identity)
```

## License

Copyright © 2017 MastodonC Ltd

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
