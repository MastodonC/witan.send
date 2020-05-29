# Transition

## Context

We need an agreed upon definition of what a "transition" is. The model takes a ["transitions file"](data/demo/data/transitions.csv), which is a csv of all transitions each calendar year over two or more years.

## Decision

A transition is recorded in the transitions file composed of a calendar year, a starting academic year, a ending academic year, a need and a setting corresponding to the starting academic year and a need and setting corresponding to the ending academic year.

A transition is a movement between calendar and academic years (these should always increase). We have found in practice that the number of children staying back or skipping academic years hasn't been large enough to model, so we currently exclude these from the transitions file and modelling.

Need and setting can change, but this is not a prerequisite. A transition is typically built from two snapshots of where someone in SEND was in January of a particular year and then in the following January. The transition is labelled with the starting calendar year of the transitions, for example a transition between 2016 and 2017 would be labelled 2016.

## Status

In review.

## Consequences

The calendar year being moved to is assumed, not specified (in the example above, this would be 2017).
