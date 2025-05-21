Static Classes {#advanced.features.static.classes}
==============

You can directly access static classes. The syntax is the same as in
PHP.

> **Note**
>
> Direct access to PHP classes is not recommended. This ties the
> underlying application code structure directly to the presentation,
> and also complicates template syntax. It is recommended to register
> plugins which insulate templates from PHP classes/objects. Use at your
> own discretion. See the Best Practices section of the Smarty website.


    {assign var=foo value=myclass::BAR}  <--- class constant BAR

    {assign var=foo value=myclass::method()}  <--- method result

    {assign var=foo value=myclass::method1()->method2}  <--- method chaining

    {assign var=foo value=myclass::$bar}  <--- property bar of class myclass

    {assign var=foo value=$bar::method}  <--- using Smarty variable bar as class name


      
