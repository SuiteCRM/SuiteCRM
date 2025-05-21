# Getting started

## Requirements
Smarty can be run with PHP 7.1 to PHP 8.4.

## Installation
Smarty can be installed with [Composer](https://getcomposer.org/).

To get the latest stable version of Smarty use:
```shell
composer require smarty/smarty
```

To get the latest, unreleased version, use:
```shell
composer require smarty/smarty:dev-master
```

To get the previous stable version of Smarty, Smarty 3, use:
```shell
composer require smarty/smarty:^3
```

Here's how you create an instance of Smarty in your PHP scripts:
```php
<?php

require 'vendor/autoload.php';
$smarty = new Smarty();
```

Now that the library files are in place, it's time to set up the Smarty
directories for your application.

Smarty requires four directories which are by default named
    [`templates`](./programmers/api-variables/variable-template-dir.md),
    [`configs`](./programmers/api-variables/variable-config-dir.md),
    [`templates_c`](./programmers/api-variables/variable-compile-dir.md)
     and 
    [`cache`](./programmers/api-variables/variable-cache-dir.md) 
    relative to the current working directory.

The defaults can be changed as follows:
```php
$smarty = new Smarty();
$smarty->setTemplateDir('/some/template/dir');
$smarty->setConfigDir('/some/config/dir');
$smarty->setCompileDir('/some/compile/dir');
$smarty->setCacheDir('/some/cache/dir');
```

The compile dir and cache dir need to be writable for the user running the PHP script.

> **Note**
>
> This is usually user "nobody" and group "nobody". For OS X users, the
> default is user "www" and group "www". If you are using Apache, you
> can look in your `httpd.conf` file to see what user and group are
> being used.

```bash
chown nobody:nobody /web/www.example.com/guestbook/templates_c/
chmod 770 /web/www.example.com/guestbook/templates_c/

chown nobody:nobody /web/www.example.com/guestbook/cache/
chmod 770 /web/www.example.com/guestbook/cache/
```

You can verify if your system has the correct access rights for
    these directories with [`testInstall()`](./programmers/api-functions/api-test-install.md):

```php
$smarty = new Smarty();
$smarty->setTemplateDir('/some/template/dir');
$smarty->setConfigDir('/some/config/dir');
$smarty->setCompileDir('/some/compile/dir');
$smarty->setCacheDir('/some/cache/dir');
$smarty->testInstall();
```

Now, let's create the `index.tpl` file that Smarty will display. This
needs to be located in the [`$template_dir`](./programmers/api-variables/variable-template-dir.md).

```smarty
{* Smarty *}
Hello {$name}, welcome to Smarty!
```

> **Note**
>
> `{* Smarty *}` is a template [comment](./designers/language-basic-syntax/language-syntax-comments.md). It
> is not required, but it is good practice to start all your template
> files with this comment. It makes the file easy to recognize
> regardless of the file extension. For example, text editors could
> recognize the file and turn on special syntax highlighting.

Now lets edit our php file. We'll create an instance of Smarty,
[`assign()`](./programmers/api-functions/api-assign.md) a template variable and
[`display()`](./programmers/api-functions/api-display.md) the `index.tpl` file.

```php
<?php

require 'vendor/autoload.php';

$smarty = new Smarty();

$smarty->setTemplateDir('/web/www.example.com/guestbook/templates/');
$smarty->setCompileDir('/web/www.example.com/guestbook/templates_c/');
$smarty->setConfigDir('/web/www.example.com/guestbook/configs/');
$smarty->setCacheDir('/web/www.example.com/guestbook/cache/');

$smarty->assign('name', 'Ned');
$smarty->display('index.tpl');

```

> **Note**
>
> In our example, we are setting absolute paths to all the Smarty
> directories. If `/web/www.example.com/guestbook/` is within your PHP
> include\_path, then these settings are not necessary. However, it is
> more efficient and (from experience) less error-prone to set them to
> absolute paths. This ensures that Smarty is getting files from the
> directories you intended.

Now, run your PHP file. You should see *"Hello Ned, welcome to Smarty!"*

You have completed the basic setup for Smarty!

## Extended Setup

This is a continuation of the [basic installation](#installation), please read that first!

A slightly more flexible way to set up Smarty is to extend the Smarty
class and initialize your Smarty
environment. So instead of repeatedly setting directory paths, assigning
the same vars, etc., we can do that in one place.

```php
<?php

class Smarty_GuestBook extends Smarty {

   public function __construct()
   {
        parent::__construct();

        $this->setTemplateDir('/web/www.example.com/guestbook/templates/');
        $this->setCompileDir('/web/www.example.com/guestbook/templates_c/');
        $this->setConfigDir('/web/www.example.com/guestbook/configs/');
        $this->setCacheDir('/web/www.example.com/guestbook/cache/');

        $this->caching = Smarty::CACHING_LIFETIME_CURRENT;
        $this->assign('app_name', 'Guest Book');
   }

}
```

Now, we can use `Smarty_GuestBook` instead of `Smarty` in our scripts:
```php
<?php
$smarty = new Smarty_GuestBook();
$smarty->assign('name', 'Ned');
$smarty->display('index.tpl');
```
