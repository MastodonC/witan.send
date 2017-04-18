# Introduction to witan.send

## Background

This model allows English local authorities to compare the costs and demand for SEND (special educational needs and disabilities) services for different future scenarios. The demand is for statements/EHC plans only and does not include SEN support.

A stochastic Markov chain approach is used to assign a state to each individual eligible for SEND at each time step of the model.

## Model structure

The main steps of the model (represented on the flowchart below) are:

* two first steps to prepare the data going into the loop,
* the loop itself (one round of the loop per year of projection), and
* the last step of associating the cost to the projections once the loop is finished.

![Overview of the SEND model](images/send-model-mvp.png)

## Model steps

### Two steps of data preparation

Those two steps are:

![Two data prep steps](images/two-data-prep-steps.png)

* “**Get historic population**”:
The historic population for 0 to 25 years old and the historic SEND population inputs are transformed and associated to generate one historic dataset of 0 to 25 years old of both SEND and non-SEND with one year per individual and per simulation.

* “**Account for population change**”:
The historic population for 0 to 25 years old and population projections for this LA are used to calculate the differences between the expected aged-on population and the projections for the following year and age group. These differences are an estimation for new non-SEND individuals added to the population due to new arrivants in the LA and also accounts for new births.

The outputs for both those steps are then joined to create the “Total Population”.
This dataset has one row per individual and per simulation and contains different SEND people and non-SEND people. It contains SEND historical data plus non-SEND data for the years of the projection.

### Inside the loop

![Steps inside the loop](images/steps-inside-the-loop.png)

* The **starting population is selected** from the “Total Population” by year (starting with the last year of historical data and incremented by one at each round of the loop)

* The **next year population is determined** from the **probabilities of the transition matrix** selected using a random number and the population numbers of the starting population.

* This new population is then **appended to the “Total population”**.

* If the loop isn’t finished **we go back at the start** where the new population plus the new joiners become the starting population (after being selected by year from the “Total population”). Otherwise **we exit the loop**.


### After the loop

![Steps after the loop](images/steps-after-the-loop.png)

* **Group individuals (by age, need and placement type)** and calculate averages and confidence intervals over simulations.

* In parallel upload the cost profile (cost per SEND need/placement combination, per individual).

* **Multiply the projected averages** for SEND individuals by the cost for a specific SEND need/placement.
