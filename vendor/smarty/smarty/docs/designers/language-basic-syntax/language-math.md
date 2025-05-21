# Math

Math can be applied directly to variable values.

## Examples
```smarty
{$foo+1}

{$foo*$bar}

{* some more complicated examples *}

{$foo->bar-$bar[1]*$baz->foo->bar()-3*7}

{if ($foo+$bar.test%$baz*134232+10+$b+10)}

{$foo|truncate:"`$fooTruncCount/$barTruncFactor-1`"}

{assign var="foo" value="`$foo+$bar`"}
```
      
> **Note**
>
> Although Smarty can handle some very complex expressions and syntax,
> it is a good rule of thumb to keep the template syntax minimal and
> focused on presentation. If you find your template syntax getting too
> complex, it may be a good idea to move the bits that do not deal
> explicitly with presentation to PHP by way of plugins or modifiers.
