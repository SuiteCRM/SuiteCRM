# Philosophy

## What is Smarty?

Smarty is a template engine for PHP. More specifically, it facilitates a
manageable way to separate application logic and content from its
presentation. This is best described in a situation where the
application programmer and the template designer play different roles,
or in most cases are not the same person.

For example, let\'s say you are creating a web page that is displaying a
newspaper article.

-   The article `$headline`, `$tagline`, `$author` and `$body` are
    content elements, they contain no information about how they will be
    presented. They are [passed](#api.assign) into Smarty by the
    application.

-   Then the template designer edits the templates and uses a
    combination of HTML tags and [template tags](#language.basic.syntax)
    to format the presentation of these
    [variables](#language.syntax.variables) with elements such as
    tables, div\'s, background colors, font sizes, style sheets, svg
    etc.

-   One day the programmer needs to change the way the article content
    is retrieved, ie a change in application logic. This change does not
    affect the template designer, the content will still arrive in the
    template exactly the same.

-   Likewise, if the template designer wants to completely redesign the
    templates, this would require no change to the application logic.

-   Therefore, the programmer can make changes to the application logic
    without the need to restructure templates, and the template designer
    can make changes to templates without breaking application logic.

## Goals

The Smarty design was largely driven by these goals:
-   clean separation of presentation from application code
-   PHP backend, Smarty template frontend
-   complement PHP, not replace it
-   fast development/deployment for programmers and designers
-   quick and easy to maintain
-   syntax easy to understand, no PHP knowledge necessary
-   flexibility for custom development
-   security: insulation from PHP
-   free, open source



## Two camps of thought

When it comes to templating in PHP, there are basically two camps of
thought. The first camp exclaims that \"PHP is a template engine\". This
approach simply mixes PHP code with HTML. Although this approach is
fastest from a pure script-execution point of view, many would argue
that the PHP syntax is messy and complicated when mixed with tagged
markup such as HTML.

The second camp exclaims that presentation should be void of all
programming code, and instead use simple tags to indicate where
application content is revealed. This approach is common with other
template engines (even in other programming languages), and is also the
approach that Smarty takes. The idea is to keep the templates focused
squarely on presentation, void of application code, and with as little
overhead as possible.

## Why is separating PHP from templates important?

Two major benefits:

-   SYNTAX: Templates typically consist of semantic markup such as HTML.
    PHP syntax works well for application code, but quickly degenerates
    when mixed with HTML. Smarty\'s simple {tag} syntax is designed
    specifically to express presentation. Smarty focuses your templates
    on presentation and less on \"code\". This lends to quicker template
    deployment and easier maintenance. Smarty syntax requires no working
    knowledge of PHP, and is intuitive for programmers and
    non-programmers alike.

-   INSULATION: When PHP is mixed with templates, there are no
    restrictions on what type of logic can be injected into a template.
    Smarty insulates the templates from PHP, creating a controlled
    separation of presentation from business logic. Smarty also has
    security features that can further enforce restrictions on
    templates.

## Web designers and PHP

A common question: "Web designers have to learn a syntax anyway, why
not PHP?" Of course web designers can learn PHP, and they may already
be familiar with it. The issue isn't their ability to learn PHP, it is
about the consequences of mixing PHP with HTML. If designers use PHP, it
is too easy to add code into templates that doesn't belong there (you
just handed them a swiss-army knife when they just needed a knife.) You
can teach them the rules of application design, but this is probably
something they don't really need to learn (now they are developers!)
The PHP manual is also an overwhelming pile of information to sift
through. It is like handing the owner of a car the factory assembly
manual when all they need is the owners manual. Smarty gives web
designers exactly the tools they need, and gives developers fine-grained
control over those tools. The simplicity of the tag-based syntax is also
a huge welcome for designers, it helps them streamline the organization
and management of templates.

