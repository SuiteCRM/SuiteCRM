registerObject()

register an object for use in the templates

Description
===========

void

registerObject

string

object\_name

object

object

array

allowed\_methods\_properties

boolean

format

array

block\_methods

> **Note**
>
> When you register/assign objects to templates, be sure that all
> properties and methods accessed from the template are for presentation
> purposes only. It is very easy to inject application logic through
> objects, and this leads to poor designs that are difficult to manage.
> See the Best Practices section of the Smarty website.

See the [objects section](#advanced.features.objects) for more
information.

See also [`getRegisteredObject()`](#api.get.registered.object), and
[`unregisterObject()`](#api.unregister.object).
