createTemplate()

returns a template object

Description
===========

Smarty\_Internal\_Template

createTemplate

string

template

object

parent

Smarty\_Internal\_Template

createTemplate

string

template

array

data

Smarty\_Internal\_Template

createTemplate

string

template

string

cache\_id

string

compile\_id

object

parent

Smarty\_Internal\_Template

createTemplate

string

template

string

cache\_id

string

compile\_id

array

data

This creates a template object which later can be rendered by the
[display](#api.display) or [fetch](#api.fetch) method. It uses the
following parameters:

-   `template` must be a valid [template resource](#resources) type and
    path.

<!-- -->


    <?php
    include('Smarty.class.php');
    $smarty = new Smarty;

    // create template object with its private variable scope
    $tpl = $smarty->createTemplate('index.tpl');

    // assign variable to template scope
    $tpl->assign('foo','bar');

    // display the template
    $tpl->display();
    ?>

        

See also [`display()`](#api.display), and
[`templateExists()`](#api.template.exists).
