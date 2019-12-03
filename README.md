# congruencer
This program build lattice of congruences of partitions of a finite set.
#### Usage
Let\`s start with example. In directory `src` you can see files with example tests.
Consider one: `src/test2`

        1 2 3 4 5 6 7
    s1 1 2 3 4 5 6 7
    s2 1 1 1 4 4 6 6
    s3 2 2 2 5 5 7 7
    s4 3 3 3 5 5 6 6
    s5 2 2 2 4 4 6 6

This file describes **transition function** of automaton or **action function** of an act. Meaning of rows:
 - First line contain all elements of automaton (or act). Here it\`s 7. Element name can consist of more than one letter, but without spaces.
 - Next lines contains action on specific semigroup element. For example, second line means: `1*s1 = 1`,  `1*s2=2`, etc.

Now, if you execute following command:
```console
foo@bar:~$ whoami
foo
```
