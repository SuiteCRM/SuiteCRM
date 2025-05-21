# {setfilter}

The `{setfilter}...{/setfilter}` block tag allows the definition of
template instance's variable filters.

SYNTAX: `{setfilter filter1\|filter2\|filter3\....}\...{/setfilter}`

The filter can be:

-   A variable filter plugin specified by it's name.

-   A modifier specified by it's name and optional additional
    parameter.

`{setfilter}...{/setfilter}` blocks can be nested. The filter definition
of inner blocks does replace the definition of the outer block.

Template instance filters run in addition to other modifiers and
filters. They run in the following order: modifier, default_modifier,
$escape_html, registered variable filters, autoloaded variable
filters, template instance's variable filters. Everything after
default_modifier can be disabled with the `nofilter` flag.

> **Note**
>
> The setting of template instance filters does not affect the output of
> included subtemplates.

## Examples

```smarty
<script>
    {setfilter filter1}
      {$foo} {* filter1 runs on output of $foo *}
      {setfilter filter2|mod:true}
        {$bar} {* filter2 and modifier mod runs on output of $bar *}
      {/setfilter}
      {$buh} {* filter1 runs on output of $buh *}
    {/setfilter}
    {$blar} {* no template instance filter runs on output of $blar}
</script>
```

