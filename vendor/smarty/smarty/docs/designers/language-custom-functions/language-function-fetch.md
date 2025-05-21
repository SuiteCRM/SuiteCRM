# {fetch}

`{fetch}` is used to retrieve files from the local file system, http, or
ftp and display the contents.

## Attributes
| Attribute | Required | Description                                          |
|-----------|----------|------------------------------------------------------|
| file      | Yes      | The file, http or ftp site to fetch                  |
| assign    | No       | The template variable the output will be assigned to |

-   If the file name begins with `http://`, the website page will be
    fetched and displayed.

    > **Note**
    >
    > This will not support http redirects, be sure to include a
    > trailing slash on your web page fetches where necessary.

-   If the file name begins with `ftp://`, the file will be downloaded
    from the ftp server and displayed.

-   For local files, either a full system file path must be given, or a
    path relative to the executed php script.

    > **Note**
    >
    > If security is enabled, and you are fetching a file from the local
    > file system, `{fetch}` will only allow files from within the
    > `$secure_dir` path of the security policy. See the
    > [Security](../../programmers/advanced-features/advanced-features-security.md) section for details.

-   If the `assign` attribute is set, the output of the `{fetch}`
    function will be assigned to this template variable instead of being
    output to the template.

## Examples

```smarty
{* include some javascript in your template *}
{fetch file='/export/httpd/www.example.com/docs/navbar.js'}

{* embed some weather text in your template from another web site *}
{fetch file='http://www.myweather.com/68502/'}

{* fetch a news headline file via ftp *}
{fetch file='ftp://user:password@ftp.example.com/path/to/currentheadlines.txt'}
{* as above but with variables *}
{fetch file="ftp://`$user`:`$password`@`$server`/`$path`"}

{* assign the fetched contents to a template variable *}
{fetch file='http://www.myweather.com/68502/' assign='weather'}
{if $weather ne ''}
  <div id="weather">{$weather}</div>
{/if}
```
      

See also [`{capture}`](../language-builtin-functions/language-function-capture.md),
[`{eval}`](language-function-eval.md),
[`{assign}`](../language-builtin-functions/language-function-assign.md) and [`fetch()`](../../programmers/api-functions/api-fetch.md).
