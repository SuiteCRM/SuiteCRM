# Combining Modifiers

You can apply any number of modifiers to a variable. They will be
applied in the order they are combined, from left to right. They must be
separated with a `|` (pipe) character.

```php
<?php

$smarty->assign('articleTitle', 'Smokers are Productive, but Death Cuts Efficiency.');
```

where template is:

```smarty
{$articleTitle}
{$articleTitle|upper|spacify}
{$articleTitle|lower|spacify|truncate}
{$articleTitle|lower|truncate:30|spacify}
{$articleTitle|lower|spacify|truncate:30:". . ."}
```
      

The above example will output:

```
Smokers are Productive, but Death Cuts Efficiency.
S M O K E R S   A R ....snip....  H   C U T S   E F F I C I E N C Y .
s m o k e r s   a r ....snip....  b u t   d e a t h   c u t s...
s m o k e r s   a r e   p r o d u c t i v e ,   b u t . . .
s m o k e r s   a r e   p. . .
```
