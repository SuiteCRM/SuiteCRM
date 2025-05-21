Features
=======

Some of Smarty's features:
-   It is extremely fast.
-   It is efficient since the PHP parser does the dirty work.
-   No template parsing overhead, only compiles once.
-   It is smart about [recompiling](#variable.compile.check) only the
    template files that have changed.
-   You can easily create your own custom
    [functions](#language.custom.functions) and [variable
    modifiers](#language.modifiers), so the template language is
    extremely extensible.
-   Configurable template [{delimiter}](#variable.left.delimiter) tag
    syntax, so you can use `{$foo}`, `{{$foo}}`, `<!--{$foo}-->`, etc.
-   The [`{if}..{elseif}..{else}..{/if}`](#language.function.if)
    constructs are passed to the PHP parser, so the `{if...}` expression
    syntax can be as simple or as complex an evaluation as you like.
-   Allows unlimited nesting of
    [`sections`](#language.function.section), `if's` etc.
-   Built-in [caching](#caching) support
-   Arbitrary [template](#resources) sources
-   [Template Inheritance](#advanced.features.template.inheritance) for
    easy management of template content.
-   [Plugin](#plugins) architecture

## Separation of presentation from application code
-   This means templates can certainly contain logic under the condition
    that it is for presentation only. Things such as
    [including](./designers/language-builtin-functions/language-function-include.md) other templates,
    [alternating](./designers/language-custom-functions/language-function-cycle.md) table row colors,
    [upper-casing](./designers/language-modifiers/language-modifier-upper.md) a variable,
    [looping](./designers/language-builtin-functions/language-function-foreach.md) over an array of data and
    rendering it are examples of presentation logic.
-   This does not mean however that Smarty forces a separation of
    business and presentation logic. Smarty has no knowledge of which is
    which, so placing business logic in the template is your own doing.
-   Also, if you desire *no* logic in your templates you certainly can
    do so by boiling the content down to text and variables only.

## How does it work?

Under the hood, Smarty "compiles" (basically copies and converts) the
templates into PHP scripts. This happens once when each template is
first invoked, and then the compiled versions are used from that point
forward. Smarty takes care of this for you, so the template designer
just edits the Smarty templates and never has to manage the compiled
versions. This approach keeps the templates easy to maintain, and yet
keeps execution times extremely fast since the compiled code is just
PHP. And of course, all PHP scripts take advantage of PHP op-code caches
such as APC.

## Template Inheritance

Template inheritance was introduced in Smarty 3. Before template
inheritance, we managed our templates in
pieces such as header and footer templates. This organization lends
itself to many problems that require some hoop-jumping, such as managing
content within the header/footer on a per-page basis. With template
inheritance, instead of including other templates we maintain our
templates as single pages. We can then manipulate blocks of content
within by inheriting them. This makes templates intuitive, efficient and
easy to manage. See
[Template Inheritance](./programmers/advanced-features/advanced-features-template-inheritance.md)
for more info.

## Why not use XML/XSLT syntax?
There are a couple of good reasons. First, Smarty can be used for more
than just XML/HTML based templates, such as generating emails,
javascript, CSV, and PDF documents. Second, XML/XSLT syntax is even more
verbose and fragile than PHP code! It is perfect for computers, but
horrible for humans. Smarty is about being easy to read, understand and
maintain.

## Template Security
Although Smarty insulates you from PHP, you still have the option to use
it in certain ways if you wish. Template security forces the restriction
of PHP (and select Smarty functions.) This is useful if you have third
parties editing templates, and you don't want to unleash the full power
of PHP or Smarty to them.

## Integration
Sometimes Smarty gets compared to Model-View-Controller (MVC)
frameworks. Smarty is not an MVC, it is just the presentation layer,
much like the View (V) part of an MVC. As a matter of fact, Smarty can
easily be integrated as the view layer of an MVC. Many of the more
popular ones have integration instructions for Smarty, or you may find
some help here in the forums and documentation.

## Other Template Engines
Smarty is not the only engine following the *"Separate Programming Code
from Presentation"* philosophy. For instance, Python has template
engines built around the same principles such as Django Templates and
CheetahTemplate. *Note: Languages such as Python do not mix with HTML
natively, which give them the advantage of proper programming code
separation from the outset. There are libraries available to mix Python
with HTML, but they are typically avoided.*

## What Smarty is Not

Smarty is not an application development framework. Smarty is not an
MVC. Smarty is not an alternative to Laravel, Symfony, CodeIgniter,
or any of the other application development frameworks for PHP.

Smarty is a template engine, and works as the (V)iew component of your
application. Smarty can easily be coupled to any of the engines listed
above as the view component. No different than any other software,
Smarty has a learning curve. Smarty does not guarantee good application
design or proper separation of presentation, this still needs to be
addressed by a competent developer and web designer.

## Is Smarty Right for Me?

Smarty is not meant to be a tool for every job. The important thing is
to identify if Smarty fits your needs. There are some important
questions to ask yourself:

### Template Syntax
Are you content with PHP tags mixed with HTML? Are your
web designers comfortable with PHP? Would your web designers prefer a
tag-based syntax designed for presentation? Some experience working with
both Smarty and PHP helps answer these questions.

### The Business Case
Is there a requirement to insulate the templates from
PHP? Do you have untrusted parties editing templates that you do not
wish to unleash the power of PHP to? Do you need to programmatically
control what is and is not available within the templates? Smarty
supplies these capabilities by design.

## Feature set
Does Smarty's features such as caching, template
inheritance and plugin architecture save development cycles writing code
that would be needed otherwise? Does the codebase or framework you plan
on using have the features you need for the presentation component?

## Sites using Smarty
Many well-known PHP projects make use of Smarty such as XOOPS CMS, CMS Made Simple, Tiki
CMS/Groupware and X-Cart to name a few.

## Summary
Whether you are using Smarty for a small website or massive enterprise
solution, it can accommodate your needs. There are numerous features
that make Smarty a great choice:

-   separation of PHP from HTML/CSS just makes sense
-   readability for organization and management
-   security for 3rd party template access
-   feature completeness, and easily extendable to your own needs
-   massive user base, Smarty is here to stay
-   LGPL license for commercial use
-   100% free to use, open source project
