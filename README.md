# congruencer
##### This program checks holding lattice identity in lattice of congruences of partitions of a finite set.
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
 - Next lines contains action on specific semigroup element. For example, second line means: `1*s1 = 1`,  `1*s2 = 2`, etc.

#### Examples

1. If you execute following command: `congruencer test2 -m` - program generates lattice of congruences and check holding modular identity. Output:
```
Congruences: 30
Modularity: True
```

2.  `congruencer test2 -m -l` gives:
```
Congruences: 30
graph lattice {
        rankdir = TB;
        ratio = 0.75;
        node[shape = none];
        "(1234567)" -- "(67)(12345)";
        "(1234567)" -- "(12367)(45)";
        "(1234567)" -- "(4567)(123)";
        "(4567)(123)" -- "(67)(45)(123)";
        "(4567)(123)" -- "(4567)(12)";
...
        "(45)(13)" -- "(45)";
        "(45)(12)" -- "(12)";
        "(45)(12)" -- "(45)";
        "(45)" -- "Delta";
        "(123)" -- "(12)";
        "(123)" -- "(13)";
        "(123)" -- "(23)";
        "(23)" -- "Delta";
        "(13)" -- "Delta";
        "(12)" -- "Delta";
}
```
(Some output skipped).

You can specify `-o file.dot` option and redirect this output to a file `file.dot`.

3. If you want just only **print congruences**, then execute: `congruencer test2 -t`. Here is out:
```
[(1234567),(4567)(123),(4567)(23),(4567)(13),(4567)(12),(4567),(12367)(45),(67)(12345),
(67)(45)(123),(67)(45)(23),(67)(45)(13),(67)(45)(12),(67)(45),(12367),(67)(123),(67)(23),
(67)(13),(67)(12),(67),(12345),(45)(123),(45)(23),(45)(13),(45)(12),(45),(123),(23),(13),(12),Delta]
Congruences: 30
```

For more info, specify flag `-h`.

My e-mail: genary@ya.ru
