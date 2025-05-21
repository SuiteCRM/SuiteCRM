templateExists()

checks whether the specified template exists

Description
===========

bool

templateExists

string

template

It can accept either a path to the template on the filesystem or a
resource string specifying the template.

This example uses `$_GET['page']` to
[`{include}`](#language.function.include) a content template. If the
template does not exist then an error page is displayed instead. First
the `page_container.tpl`


    <html>
    <head><title>{$title}</title></head>
    <body>
    {include file='page_top.tpl'}

    {* include middle content page *}
    {include file=$content_template}

    {include file='page_footer.tpl'}
    </body>

      

And the php script


    <?php

    // set the filename eg index.inc.tpl
    $mid_template = $_GET['page'].'.inc.tpl';

    if( !$smarty->templateExists($mid_template) ){
        $mid_template = 'page_not_found.tpl';
    }
    $smarty->assign('content_template', $mid_template);

    $smarty->display('page_container.tpl');

    ?>

      

See also [`display()`](#api.display), [`fetch()`](#api.fetch),
[`{include}`](#language.function.include) and
[`{insert}`](#language.function.insert)
