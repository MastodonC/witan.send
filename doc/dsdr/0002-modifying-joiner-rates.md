# Modify joiner rates (not number of joiners per year)

## Context

When calculating joiners rates the model counts the number of joiners
for each academic year. It then works out the rate by which people 
join each possible state for that academic year.

This then means that, when increasing joiner transition rates to a
specific state, not only does the likeliness of joining that state
increase but also the rate by which that academic year's population
increases. This then leads to a much larger population when the 
scenario to be modelled is simply "increase joiner rate"

## Decision

When calculating the joiner rate, the original transitions data will 
be used for either a standard or scenario projection. As previously for 
scenario projections, when increasing the rate of a specific state
modified transtion data will be used.

## Status

In review

## Consequences

### There is no way of increasing the overall SEND joiner rate

This change makes it impossible to increase the overall number of 
people joining SEND. This means it would not be possible to run 
a scenario characterised as "we expect a larger population growth 
in SEND in the future, than is currently projected" 

### Some previous projections will not be repeatable with this change

The behaviour above has been a desired effect of some previous 
projections, and without returning to an older version of the model
it would be impossible (without further development) to re-run these
projections.
