# Config Files

Config files are handy for designers to manage global template variables
from one file. One example is template colors. Normally if you wanted to
change the color scheme of an application, you would have to go through
each and every template file and change the colors. With a config file,
the colors can be kept in one place, and only one file needs to be
updated.

```ini
# global variables
pageTitle = "Main Menu"
bodyBgColor = #000000
tableBgColor = #000000
rowBgColor = #00ff00

[Customer]
pageTitle = "Customer Info"

[Login]
pageTitle = "Login"
focus = "username"
Intro = """This is a value that spans more
           than one line. you must enclose
           it in triple quotes."""

# hidden section
[.Database]
host=my.example.com
db=ADDRESSBOOK
user=php-user
pass=foobar
```
      

Values of [config file variables](./language-variables/language-config-variables.md) can be in
quotes, but not necessary. You can use either single or double quotes.
If you have a value that spans more than one line, enclose the entire
value with triple quotes \("""\). You can put comments into config
files by any syntax that is not a valid config file syntax. We recommend
using a `#` (hash) at the beginning of the line.

The example config file above has two sections. Section names are
enclosed in \[brackets\]. Section names can be arbitrary strings not
containing `[` or `]` symbols. The four variables at the top are global
variables, or variables not within a section. These variables are always
loaded from the config file. If a particular section is loaded, then the
global variables and the variables from that section are also loaded. If
a variable exists both as a global and in a section, the section
variable is used. If you name two variables the same within a section,
the last one will be used unless
[`$config_overwrite`](../programmers/api-variables/variable-config-overwrite.md) is disabled.

Config files are loaded into templates with the built-in template
function [`{config_load}`](./language-builtin-functions/language-function-config-load.md) or the API
[`configLoad()`](../programmers/api-functions/api-config-load.md) function.

You can hide variables or entire sections by prepending the variable
name or section name with a period(.) eg `[.hidden]`. This is useful if
your application reads the config files and gets sensitive data from
them that the template engine does not need. If you have third parties
doing template editing, you can be certain that they cannot read
sensitive data from the config file by loading it into the template.

Config files (or resources) are loaded by the same resource facilities
as templates. That means that a config file can also be loaded from a db
`$smarty->configLoad("db:my.conf")`.

See also [`{config_load}`](./language-builtin-functions/language-function-config-load.md),
[`$config_overwrite`](../programmers/api-variables/variable-config-overwrite.md),
[`$default_config_handler_func`](../programmers/api-variables/variable-default-config-handler-func.md),
[`getConfigVars()`](../programmers/api-functions/api-get-config-vars.md),
[`clearConfig()`](../programmers/api-functions/api-clear-config.md) and
[`configLoad()`](../programmers/api-functions/api-config-load.md)
