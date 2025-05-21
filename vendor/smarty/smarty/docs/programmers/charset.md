Charset Encoding {#charset}
================

Charset Encoding {#charset.encoding}
================

There are a variety of encodings for textual data, ISO-8859-1 (Latin1)
and UTF-8 being the most popular. Unless you change `Smarty::$_CHARSET`, 
Smarty recognizes `UTF-8` as the internal charset if 
[Multibyte String](https://www.php.net/mbstring) is available, 
`ISO-8859-1` if not.

> **Note**
>
> `ISO-8859-1` has been PHP\'s default internal charset since the
> beginning. Unicode has been evolving since 1991. Since then it has
> become the one charset to conquer them all, as it is capable of
> encoding most of the known characters even across different character
> systems (latin, cyrillic, japanese, ...). `UTF-8` is unicode\'s most
> used encoding, as it allows referencing the thousands of character
> with the smallest size overhead possible.
>
> Since unicode and UTF-8 are very wide spread nowadays, their use is
> strongly encouraged.

> **Note**
>
> Smarty\'s internals and core plugins are truly UTF-8 compatible since
> Smarty 3.1. To achieve unicode compatibility, the [Multibyte
> String](https://www.php.net/mbstring) PECL is required. Unless your PHP
> environment offers this package, Smarty will not be able to offer
> full-scale UTF-8 compatibility.


    // use japanese character encoding
    if (function_exists('mb_internal_charset')) {
      mb_internal_charset('EUC-JP');
    }

    require_once 'libs/Smarty.class.php';
    Smarty::$_CHARSET = 'EUC-JP';
    $smarty = new Smarty();
      
     
