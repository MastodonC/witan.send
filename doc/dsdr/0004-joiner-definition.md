# Joiner

## Context

We need an agreed upon definition of what a "joiner" is. 

## Decision

A joiner is someone who has been provided with a new EHCP, or the EHCP has now become the responsibility of the local authority (they moved into the area). They are able to have previously had an EHCP (or statement) which has since ended. A joiner for 2016 would be someone who between Jan-16 and Jan-17 transitioned into SEND.

In terms of data a joiner would have "NONSEND" for their starting need and setting (`need-1` and `setting-1`) and specific SEND need and setting for their ending need and setting (`need-2` and `setting-2`). They would still require an applicable calendar year and academic years.

## Status

In review.

## Consequences

The calendar year being moved to is assumed, not specified (in the example above, this would be 2017).

Even though the "in SEND" information is on the "ending year" side (`need-2`, `setting-2`), a joiner transition is referred to be the calendar year stated in the transition data.

Even though there is a possibility the person was actually issued an EHCP in Jan 2017, when the snapshot was recorded, this person would still be consider a 2016 joiner.
