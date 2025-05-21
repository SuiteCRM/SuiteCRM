# Variables loaded from config files

Variables that are loaded from the [config files](../config-files.md) are
referenced by enclosing them within `#hash_marks#`, or with the smarty
variable [`$smarty.config`](language-variables-smarty.md#smartyconfig-languagevariablessmartyconfig). The
later syntax is useful for embedding into quoted attribute values, or
accessing variable values such as `$smarty.config.$foo`.

## Examples

Example config file - `foo.conf`:
```ini
pageTitle = "This is mine"
bodyBgColor = '#eeeeee'
tableBorderSize = 3
tableBgColor = "#bbbbbb"
rowBgColor = "#cccccc"
```

A template demonstrating the `#hash#` method:

```smarty
{config_load file='foo.conf'}
<html>
    <title>{#pageTitle#}</title>
    <body bgcolor="{#bodyBgColor#}">
        <table border="{#tableBorderSize#}" bgcolor="{#tableBgColor#}">
            <tr bgcolor="{#rowBgColor#}">
                <td>First</td>
                <td>Last</td>
                <td>Address</td>
            </tr>
        </table>
    </body>
</html>
```
        
A template demonstrating the
[`$smarty.config`](language-variables-smarty.md#smartyconfig-languagevariablessmartyconfig) method:

```smarty
{config_load file='foo.conf'}
<html>
<title>{$smarty.config.pageTitle}</title>
    <body bgcolor="{$smarty.config.bodyBgColor}">
        <table border="{$smarty.config.tableBorderSize}" bgcolor="{$smarty.config.tableBgColor}">
            <tr bgcolor="{$smarty.config.rowBgColor}">
                <td>First</td>
                <td>Last</td>
                <td>Address</td>
            </tr>
        </table>
    </body>
</html>
```

Both examples would output:

```html
<html>
    <title>This is mine</title>
    <body bgcolor="#eeeeee">
        <table border="3" bgcolor="#bbbbbb">
            <tr bgcolor="#cccccc">
                <td>First</td>
                <td>Last</td>
                <td>Address</td>
            </tr>
        </table>
    </body>
</html>
```

Config file variables cannot be used until after they are loaded in from
a config file. This procedure is explained later in this document under
[`{config_load}`](../language-builtin-functions/language-function-config-load.md).

See also [variables](../language-basic-syntax/language-syntax-variables.md) and [$smarty reserved
variables](language-variables-smarty.md).
