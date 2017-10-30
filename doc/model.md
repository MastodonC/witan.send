
The model tracks 3 things, joiners, leavers and transitions.

Model relies on sampling beta-binomial and dirichlet-multinomial sampling. One describes a distribution of a binary outcome, the other describes the distribution of a multinomial outcome.

Leavers are the easiest to model. The probability of leaving is given by beta params for each academic year and setting combination.

We may not have observed leavers at each academic year/ setting and the data may be noisy, so we calculate a prior belief that's based on leavers in each academic year and balance this with the posterior calculated from academic year / setting data.

We also may not have observed leavers at each academic year, so we calculate a prior based on leavers overall, and merge this with the prior for each academic year. This is to ensure that the probability of leaving is never zero at any academic year / setting combination, though it may be small.

Movers are the next most simple thing to calculate. We calculate the probability of transitioning very similar to the method above. We ensure that the probability of transitioning in each academic year / setting combination is never zero, though it may be small.

In addition to the probability of moving, we also want to know where someone will move to. This again is produced by looking first at the actual transitions, secondly at the transitions by academic year and thirdly by transitions within the model overall. We produce a balanced set of alphas which are fed to a dirichlet multinomial. Only impossible transitions are forbidden, though the probablility of some transitions may be very small.

Finally, having calculated both of the above, we calculate the joiners. These are left until last because we sample the number of joiners in each iteration as a function of the number of leavers. This ensures that the mean growth overall is predictable.

Joiner beta params are calculated looking just at joiners in the sample year overall. The alpha is the number who joined SEND, the beta is the number in the general population who did not join SEND. We adjust this alpha / beta combination so that the mean reflects the target growth rate.

Most of the variance in the model comes from the number of joiners, so we want this to match historic variance. We adjust the beta params so the expected variance equals this variance.

Sampling from the beta binomial with these beta params gives us joiners with the appropriate mean and variance.

Next we sample from a dirichlet multinomial to figure out what age our joiners are. This is based on observed joiners by academic year within the sample year. We assume a uniform prior, so no academic years have zero probability, though some are unlikely.

Having determined the joiners by academic year, we next want to know which need / setting they will join in. This is based on observed joiners by acadmic year, need, setting with no prior. Only observed joiner states are allowed.
