# Report
We first order the equations in a order such that the equations only depends on the previous ones.

We must pay attentation to the fact that REG instruction don't have dependencies (because we read the value
of the previous step) and that RAM don't have dependencies in writing, because write operation to ram can be defer
at the end of the step and be done in any possible order, as we can't read a value written at the same step,
so we read the value which was stored.


