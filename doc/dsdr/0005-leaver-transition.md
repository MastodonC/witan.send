# Leaver

## Context

We need an agreed upon definition of what a "leaver" is. 

## Decision

A leaver is someone whose EHCP has ended due to aging out or other reasons, or has moved out of the Local Authority and therefore the Local Authority is no longer responsible for their education and support. A leaver for 2016 would be someone who between Jan-16 and Jan-17 transitioned out of SEND.

In terms of data a leaver would have "NONSEND" for their ending need and setting (`need-2` and `setting-2`) and a specific SEND need and setting for their starting need and setting (`need-2` and `setting-2`). They would still require an applicable calendar year and academic years.

## Status

In review.

## Consequences

The calendar year being moved to is assumed, not specified (in the example above, this would be 2017).
