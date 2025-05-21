fetch()

returns the template output

Description
===========

string

fetch

string

template

string

cache\_id

string

compile\_id

This returns the template output instead of [displaying](#api.display)
it. Supply a valid [template resource](#resources) type and path. As an
optional second parameter, you can pass a `$cache id`, see the [caching
section](#caching) for more information.

PARAMETER.COMPILEID


    <?php
    include('Smarty.class.php');
    $smarty = new Smarty;

    $smarty->setCaching(true);

    // set a separate cache_id for each unique URL
    $cache_id = md5($_SERVER['REQUEST_URI']);

    // capture the output
    $output = $smarty->fetch('index.tpl', $cache_id);

    // do something with $output here
    echo $output;
    ?>

        

The `email_body.tpl` template


    Dear {$contact_info.name},

    Welcome and thank you for signing up as a member of our user group.

    Click on the link below to login with your user name
    of '{$contact_info.username}' so you can post in our forums.

    {$login_url}

    List master

    {textformat wrap=40}
    This is some long-winded disclaimer text that would automatically get wrapped
    at 40 characters. This helps make the text easier to read in mail programs that
    do not wrap sentences for you.
    {/textformat}

        

The php script using the PHP [`mail()`](https://www.php.net/function.mail)
function


    <?php

    // get $contact_info from db or other resource here

    $smarty->assign('contact_info',$contact_info);
    $smarty->assign('login_url',"http://{$_SERVER['SERVER_NAME']}/login");

    mail($contact_info['email'], 'Thank You', $smarty->fetch('email_body.tpl'));

    ?>

        

See also [`{fetch}`](#language.function.fetch)
[`display()`](#api.display), [`{eval}`](#language.function.eval), and
[`templateExists()`](#api.template.exists).
