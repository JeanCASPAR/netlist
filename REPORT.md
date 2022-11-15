# Report
We first order the equations in a order such that the equations only depends on the previous ones.

We must pay attentation to the fact that REG instruction don't have dependencies (because we read the value
of the previous step) and that RAM don't have dependencies in writing, because write operation to ram can be defer
at the end of the step and be done in any possible order, as we can't read a value written at the same step,
so we read the value which was stored. For implementing the REG instruction, we keep at each point the values of
the variables at the previous step, in the `old` variable.

We initialise variables defined by REG at zero, and we fill the RAM with zeros too, for the sake of
determinism. In order to fill the ROM, the user must provide a list of values as described in the `README.md`. Then the computer will, for each ROM, ask the user to either enter the list manually, or to provide a file path which contains such a list, or to say if the ROM should be zeroed (which is not very useful).

I encountered difficulty to understand how the RAM and ROM instructions should work. They create
an indexable array (which can be modified in the case of the RAM) but the read operations return the
values of the last step for the RAM, and I didn't understand right away that we shouldn't have
dependencies for the write operation for the RAM.

I changed the function `check_topo_on` in `graph_test.ml` because the previous version accepted
incorrect lists when repeted numbers which aren't topological ordering of a graph.
