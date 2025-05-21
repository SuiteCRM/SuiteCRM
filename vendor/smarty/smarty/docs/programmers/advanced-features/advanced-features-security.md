Security {#advanced.features.security}
========

Security is good for situations when you have untrusted parties editing
the templates e.g. via ftp, and you want to reduce the risk of system
security compromises through the template language.

The settings of the security policy are defined by properties of an
instance of the Smarty\_Security class. These are the possible settings:

-   `$secure_dir` is an array of template directories that are
    considered secure. [`$template_dir`](#variable.template.dir)
    considered secure implicitly. The default is an empty array.

-   `$trusted_dir` is an array of all directories that are considered
    trusted. Trusted directories are where you keep php scripts that are
    executed directly from the templates with
    [`{insert}`](#language.function.insert.php). The default is an
    empty array.

-   `$trusted_uri` is an array of regular expressions matching URIs that
    are considered trusted. This security directive used by
    [`{fetch}`](#language.function.fetch) and
    [`{html_image}`](#language.function.html.image). URIs passed to
    these functions are reduced to `{$PROTOCOL}://{$HOSTNAME}` to allow
    simple regular expressions (without having to deal with edge cases
    like authentication-tokens).

    The expression `'#https?://.*smarty.net$#i'` would allow accessing
    the following URIs:

    -   `http://smarty.net/foo`

    -   `http://smarty.net/foo`

    -   `http://www.smarty.net/foo`

    -   `http://smarty.net/foo`

    -   `https://foo.bar.www.smarty.net/foo/bla?blubb=1`

    but deny access to these URIs:

    -   `http://smarty.com/foo` (not matching top-level domain \"com\")

    -   `ftp://www.smarty.net/foo` (not matching protocol \"ftp\")

    -   `http://www.smarty.net.otherdomain.com/foo` (not matching end of
        domain \"smarty.net\")

-   `$static_classes` is an array of classes that are considered
    trusted. The default is an empty array which allows access to all
    static classes. To disable access to all static classes set
    \$static\_classes = null.

-   `$php_functions` is an array of PHP functions that are considered
    trusted and can be used from within template. To disable access to
    all PHP functions set \$php\_functions = null. An empty array (
    \$php\_functions = array() ) will allow all PHP functions. The
    default is array(\'isset\', \'empty\', \'count\', \'sizeof\',
    \'in\_array\', \'is\_array\',\'time\',\'nl2br\').

-   `$php_modifiers` is an array of PHP functions that are considered
    trusted and can be used from within template as modifier. To disable
    access to all PHP modifier set \$php\_modifier = null. An empty
    array ( \$php\_modifier = array() ) will allow all PHP functions.
    The default is array(\'escape\',\'count\').

-   `$streams` is an array of streams that are considered trusted and
    can be used from within template. To disable access to all streams
    set \$streams = null. An empty array ( \$streams = array() ) will
    allow all streams. The default is array(\'file\').

-   `$allowed_modifiers` is an array of (registered / autoloaded)
    modifiers that should be accessible to the template. If this array
    is non-empty, only the herein listed modifiers may be used. This is
    a whitelist.

-   `$disabled_modifiers` is an array of (registered / autoloaded)
    modifiers that may not be accessible to the template.

-   `$allowed_tags` is a boolean flag which controls if constants can
    function-, block and filter plugins that should be accessible to the
    template. If this array is non-empty, only the herein listed
    modifiers may be used. This is a whitelist.

-   `$disabled_tags` is an array of (registered / autoloaded) function-,
    block and filter plugins that may not be accessible to the template.

-   `$allow_constants` is a boolean flag which controls if constants can
    be accessed by the template. The default is \"true\".

-   `$allow_super_globals` is a boolean flag which controls if the PHP
    super globals can be accessed by the template. The default is
    \"true\".

If security is enabled, no private methods, functions or properties of
static classes or assigned objects can be accessed (beginning with
\'\_\') by the template.

To customize the security policy settings you can extend the
Smarty\_Security class or create an instance of it.


    <?php
    require 'Smarty.class.php';

    class My_Security_Policy extends Smarty_Security {
      // disable all PHP functions
      public $php_functions = null;
      // allow everthing as modifier
      public $php_modifiers = array();
    }
    $smarty = new Smarty();
    // enable security
    $smarty->enableSecurity('My_Security_Policy');
    ?>


    <?php
    require 'Smarty.class.php';
    $smarty = new Smarty();
    $my_security_policy = new Smarty_Security($smarty);
    // disable all PHP functions
    $my_security_policy->php_functions = null;
    // allow everthing as modifier
    $my_security_policy->php_modifiers = array();
    // enable security
    $smarty->enableSecurity($my_security_policy);
    ?>


    <?php
    require 'Smarty.class.php';
    $smarty = new Smarty();
    // enable default security
    $smarty->enableSecurity();
    ?>

> **Note**
>
> Most security policy settings are only checked when the template gets
> compiled. For that reason you should delete all cached and compiled
> template files when you change your security settings.
