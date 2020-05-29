# Modelling need transitions

## Context

The witan.send model projects subpopulations within a local authorities SEND population, broken down by academic year, primary need and setting. Academic years are expected (by the model) to increase year on year, and settings are allowed and expected to change as people flow through the SEND system. It reality it is also possible for the primary need of a person to be changed, however when building the model this was considered uncommon.

A (now out of date) [branch](https://github.com/MastodonC/witan.send/pull/175) was created to model what including need transitions in the witan.send model would mean in terms of modification to the model and generalised results.

## Decision

Need only changes are not included in the model. They are deemed uncommon and would increase the number of rates calculated, in short making the subpopulations smaller and harder to project.

The witan.send model is designed to project a local authorities SEND population and allow them to model scenarios for the future to help them adjust planning. It is considered unlikely that modelling the rate at which people change needs would be a policy that a local authority could realistically implement.

## Status

Implemented.

## Consequences

Some local authorities record need changes more commonly than others, so it may be important in some places. 

No matter how common need changes are, the information is not being included and so could be considered lost.
