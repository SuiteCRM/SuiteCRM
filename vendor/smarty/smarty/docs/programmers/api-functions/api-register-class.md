registerClass()

register a class for use in the templates

Description
===========

void

registerClass

string

class\_name

string

class\_impl

Smarty allows you to access static classes from templates as long as the
[Security Policy](#advanced.features.security) does not tell it
otherwise. If security is enabled, classes registered with
`registerClass()` are accessible to templates.


    <?php

    class Bar {
      $property = "hello world";
    }

    $smarty = new Smarty();
    $smarty->registerClass("Foo", "Bar");

       


    {* Smarty will access this class as long as it's not prohibited by security *}
    {Bar::$property}
    {* Foo translates to the real class Bar *}
    {Foo::$property}

       


    <?php
    namespace my\php\application {
      class Bar {
        $property = "hello world";
      }
    }

    $smarty = new Smarty();
    $smarty->registerClass("Foo", "\my\php\application\Bar");

       


    {* Foo translates to the real class \my\php\application\Bar *}
    {Foo::$property}

       

See also [`registerObject()`](#api.register.object), and
[Security](#advanced.features.security).
