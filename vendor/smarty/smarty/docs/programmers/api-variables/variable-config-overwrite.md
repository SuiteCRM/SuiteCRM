\$config\_overwrite {#variable.config.overwrite}
===================

If set to TRUE, the default then variables read in from [config
files](#config.files) will overwrite each other. Otherwise, the
variables will be pushed onto an array. This is helpful if you want to
store arrays of data in config files, just list each element multiple
times.

This examples uses [`{cycle}`](#language.function.cycle) to output a
table with alternating red/green/blue row colors with
`$config_overwrite` = FALSE.

The config file.


    # row colors
    rowColors = #FF0000
    rowColors = #00FF00
    rowColors = #0000FF

        

The template with a [`{section}`](#language.function.section) loop.


    <table>
      {section name=r loop=$rows}
      <tr bgcolor="{cycle values=#rowColors#}">
        <td> ....etc.... </td>
      </tr>
      {/section}
    </table>

        

See also [`{config_load}`](#language.function.config.load),
[`getConfigVars()`](#api.get.config.vars),
[`clearConfig()`](#api.clear.config), [`configLoad()`](#api.config.load)
and the [config files section](#config.files).
