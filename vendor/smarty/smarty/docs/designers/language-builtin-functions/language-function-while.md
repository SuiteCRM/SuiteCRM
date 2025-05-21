# {while}

`{while}` loops in Smarty have much the same flexibility as PHP
[while](https://www.php.net/while) statements, with a few added features for
the template engine. Every `{while}` must be paired with a matching
`{/while}`. All PHP conditionals and functions are recognized, such as
*\|\|*, *or*, *&&*, *and*, *is_array()*, etc.

The following is a list of recognized qualifiers, which must be
separated from surrounding elements by spaces. Note that items listed in
\[brackets\] are optional. PHP equivalents are shown where applicable.

## Qualifiers

| Qualifier          | Alternates | Syntax Example       | Meaning                        | PHP Equivalent     |
|--------------------|------------|----------------------|--------------------------------|--------------------|
| ==                 | eq         | $a eq $b             | equals                         | ==                 |
| !=                 | ne, neq    | $a neq $b            | not equals                     | !=                 |
| >                  | gt         | $a gt $b             | greater than                   | >                  |
| <                  | lt         | $a lt $b             | less than                      | <                  |
| >=                 | gte, ge    | $a ge $b             | greater than or equal          | >=                 |
| <=                 | lte, le    | $a le $b             | less than or equal             | <=                 |
| ===                |            | $a === 0             | check for identity             | ===                |
| !                  | not        | not $a               | negation (unary)               | !                  |
| %                  | mod        | $a mod $b            | modulo                         | %                  |
| is \[not\] div by  |            | $a is not div by 4   | divisible by                   | $a % $b == 0       |
| is \[not\] even    |            | $a is not even       | \[not\] an even number (unary) | $a % 2 == 0        |
| is \[not\] even by |            | $a is not even by $b | grouping level \[not\] even    | ($a / $b) % 2 == 0 |
| is \[not\] odd     |            | $a is not odd        | \[not\] an odd number (unary)  | $a % 2 != 0        |
| is \[not\] odd by  |            | $a is not odd by $b  | \[not\] an odd grouping        | ($a / $b) % 2 != 0 |

## Examples
```smarty
{while $foo > 0}
  {$foo--}
{/while}
```

The above example will count down the value of $foo until 1 is reached.

See also [`{foreach}`](./language-function-foreach.md),
[`{for}`](./language-function-for.md) and
[`{section}`](./language-function-section.md).
