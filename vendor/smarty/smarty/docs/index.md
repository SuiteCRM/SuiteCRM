# Smarty 4 Documentation
Smarty is a template engine for PHP, facilitating the separation of presentation (HTML/CSS) from application logic. 

It allows you to write **templates**, using **variables**, **modifiers**, **functions** and **comments**, like this:
```html
<h1>{$title|escape}</h1>

<p>
    The number of pixels is: {math equation="x * y" x=$height y=$width}.
</p>
```

When this template is rendered, with the value "Hello world" for the variable $title, 640 for $width, 
and 480 for $height, the result is:
```html
<h1>Hello world</h1>

<p>
    The number of pixels is: 307200.
</p>
```

## Introduction
- [Philosophy](./philosophy.md) - or "Why do I need a template engine?"
- [Features](./features.md) - or "Why do I want Smarty?"
- [Getting Started](./getting-started.md)

## Smarty for template designers
- [Basic Syntax](designers/language-basic-syntax/index.md)
- [Variables](designers/language-variables/index.md)
- [Variable Modifiers](designers/language-modifiers/index.md)
- [Combining Modifiers](./designers/language-combining-modifiers.md)
- [Built-in Functions](designers/language-builtin-functions/index.md)
- [Custom Functions](designers/language-custom-functions/index.md)
- [Config Files](./designers/config-files.md)
- [Debugging Console](./designers/chapter-debugging-console.md)

## Smarty for php developers
- [Charset Encoding](./programmers/charset.md)
- [Constants](./programmers/smarty-constants.md)
- [Smarty Class Variables](./programmers/api-variables.md)
- [Smarty Class Methods](./programmers/api-functions.md)
- [Caching](./programmers/caching.md)
- [Resources](./programmers/resources.md)
- [Advanced Features](./programmers/advanced-features.md)
- [Extending Smarty With Plugins](./programmers/plugins.md)

## Other
- [Some random tips & tricks](./appendixes/tips.md)
- [Troubleshooting](./appendixes/troubleshooting.md)
