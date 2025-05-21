# {html_image}

`{html_image}` is a [custom function](index.md) that
generates an HTML `<img>` tag. The `height` and `width` are
automatically calculated from the image file if they are not supplied.

## Attributes

| Attribute Name | Required | Description                                                             |
|----------------|----------|-------------------------------------------------------------------------|
| file           | Yes      | name/path to image                                                      |
| height         | No       | Height to display image (defaults to actual image height)               |
| width          | No       | Width to display image (defaults to actual image width)                 |
| basedir        | no       | Directory to base relative paths from (defaults to web server doc root) |
| alt            | no       | Alternative description of the image                                    |
| href           | no       | href value to link the image to                                         |
| path\_prefix   | no       | Prefix for output path                                                  |

-   `basedir` is the base directory that relative image paths are based
    from. If not given, the web server's document root
    `$_ENV['DOCUMENT_ROOT']` is used as the base. If security is
    enabled, then the image must be located in the `$secure_dir` path of
    the security policy. See the [Security](../../programmers/advanced-features/advanced-features-security.md)
    section for details.

-   `href` is the href value to link the image to. If link is supplied,
    an `<a href="LINKVALUE"><a>` tag is placed around the image tag.

-   `path_prefix` is an optional prefix string you can give the output
    path. This is useful if you want to supply a different server name
    for the image.

-   All parameters that are not in the list above are printed as
    name/value-pairs inside the created `<img>` tag.

> **Note**
>
> `{html_image}` requires a hit to the disk to read the image and
> calculate the height and width. If you don't use template
> [caching](../../programmers/caching.md), it is generally better to avoid `{html_image}`
> and leave image tags static for optimal performance.

## Examples

```smarty
{html_image file='pumpkin.jpg'}
{html_image file='/path/from/docroot/pumpkin.jpg'}
{html_image file='../path/relative/to/currdir/pumpkin.jpg'}
```

Example output of the above template would be:

```html
<img src="pumpkin.jpg" alt="" width="44" height="68" />
<img src="/path/from/docroot/pumpkin.jpg" alt="" width="44" height="68" />
<img src="../path/relative/to/currdir/pumpkin.jpg" alt="" width="44" height="68" />
```
      
