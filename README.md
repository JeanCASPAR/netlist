# Launch the program
Compile the program with `ocamlbuild ./netlist_simulator.byte`
and then execute it `./netlist_simulator.byte` with the following options :
*  -n `int` : Number of steps to simulate (default `0`)
*  -print `bool` : Print the sorted netlist and exit (default `false`)
*  -debug `bool` : Print each variable at the end of each step (default `false`)
*  -help : Display this list of options
* PATH : Netlist to simulate
You can remove all build files with `ocamlbuild -clean`.

# ROM format
The values in the ROM should be given as sequences of `word_size` 0 or 1 without delimiters, one per line.

The ROM is filled from address 00...0 to address 11..1 in increasing lexicographic order.

If the list is too short, the rest is zeroed.
