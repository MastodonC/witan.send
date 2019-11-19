# Create new data slices at the simulation levels

## Context

The model projects future activity by creating multiple simulations of
the future. We then run these simulation results through a HDR
Histogram to get summary statistics. When we use the data directly at
the ay/need/setting + population level, then we don't need to roll up
the simulation results as they are already summarised at this level.

If we want to figure out costs, then we need to multiply each
need-setting by the appropriate amount, or if we wanted to sum needs
or settings, then we'd have to roll up using that criteria.

We can't do this at the end as we'd be summing means/medians/etc
rather than creating them at the simulation level.

## Decision

All data slicing and summarising should be done at the simulation
level, before putting those results through a HDR Histogram to produce
summary statistics across all simulations.

The function `data-products` in `witan.send.model.data-products` shows
how to do this.

## Status

Accepted

## Consequences

### New data products might add significant processing time

Because of this, we'll end up running summarising functions and a HDR
Histogram for each slice through the data. This is mathematically more
sound, but means that we have to do a larger chunk of work for each
new data product.

### New data products need to be built off the simulation data rather than the summaries

Because of how the HDR Histogram and the simulations work together, it
isn't mathematically sound to create new data products by using any
summary results. If you had the median/mean/iqr of the academic
years/need/settings of the simulation, you couldn't roll that up to
totals for need/setting without the academic years.
