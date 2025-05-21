createData()

creates a data object

Description
===========

string

createData

object

parent

string

createData

This creates a data object which will hold assigned variables. It uses
the following parameters:

-   `parent` is an optional parameter. It is an uplink to the main
    Smarty object, a another user-created data object or to user-created
    template object. These objects can be chained. Templates can access
    variables assigned to any of the objects in it\'s parent chain.

Data objects are used to create scopes for assigned variables. They can
be used to control which variables are seen by which templates.


    <?php
    include('Smarty.class.php');
    $smarty = new Smarty;

    // create data object with its private variable scope
    $data = $smarty->createData();

    // assign variable to data scope
    $data->assign('foo','bar');

    // create template object which will use variables from data object
    $tpl = $smarty->createTemplate('index.tpl',$data);

    // display the template
    $tpl->display();
    ?>

        

See also [`display()`](#api.display), and
[`createTemplate()`](#api.create.template),
