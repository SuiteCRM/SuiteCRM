<?php
define('X', 'X');

function smarty_block_minify($params, $content, &$smarty, &$repeat)
{
    if (!$repeat && isset($content)) {
        // HTML Minifier
        $input = $content;
        if (trim($input) === "") {
            return $input;
        }
        // Remove extra white-space(s) between HTML attribute(s)
        $input = preg_replace_callback('#<([^\/\s<>!]+)(?:\s+([^<>]*?)\s*|\s*)(\/?)>#s', function ($matches) {
            return '<' . $matches[1] . preg_replace(
                '#([^\s=]+)(\=([\'"]?)(.*?)\3)?(\s+|$)#s',
                ' $1$2',
                $matches[2]
            ) . $matches[3] . '>';
        }, str_replace("\r", "", $input));
        // Minify inline CSS declaration(s)
        if (strpos($input, ' style=') !== false) {
            $input = preg_replace_callback('#<([^<]+?)\s+style=([\'"])(.*?)\2(?=[\/\s>])#s', function ($matches) {
                return '<' . $matches[1] . ' style=' . $matches[2] . minify_css($matches[3]) . $matches[2];
            }, $input);
        }

        return preg_replace(
            array(
                    // t = text
                    // o = tag open
                    // c = tag close
                    // Keep important white-space(s) after self-closing HTML tag(s)
                    '#<(img|input)(>| .*?>)#s',
                    // Remove a line break and two or more white-space(s) between tag(s)
                    '#(<!--.*?-->)|(>)(?:\n*|\s{2,})(<)|^\s*|\s*$#s',
                    '#(<!--.*?-->)|(?<!\>)\s+(<\/.*?>)|(<[^\/]*?>)\s+(?!\<)#s',
                    // t+c || o+t
                    '#(<!--.*?-->)|(<[^\/]*?>)\s+(<[^\/]*?>)|(<\/.*?>)\s+(<\/.*?>)#s',
                    // o+o || c+c
                    '#(<!--.*?-->)|(<\/.*?>)\s+(\s)(?!\<)|(?<!\>)\s+(\s)(<[^\/]*?\/?>)|(<[^\/]*?\/?>)\s+(\s)(?!\<)#s',
                    // c+t || t+o || o+t -- separated by long white-space(s)
                    '#(<!--.*?-->)|(<[^\/]*?>)\s+(<\/.*?>)#s',
                    // empty tag
                    '#<(img|input)(>| .*?>)<\/\1\x1A>#s',
                    // reset previous fix
                    '#(&nbsp;)&nbsp;(?![<\s])#',
                    // clean up ...
                    // Force line-break with `&#10;` or `&#xa;`
                    '#&\#(?:10|xa);#',
                    // Force white-space with `&#32;` or `&#x20;`
                    '#&\#(?:32|x20);#',
                    // Remove HTML comment(s) except IE comment(s)
                    '#\s*<!--(?!\[if\s).*?-->\s*|(?<!\>)\n+(?=\<[^!])#s'
                ),
            array(
                    "<$1$2</$1\x1A>",
                    '$1$2$3',
                    '$1$2$3',
                    '$1$2$3$4$5',
                    '$1$2$3$4$5$6$7',
                    '$1$2$3',
                    '<$1$2',
                    '$1 ',
                    "\n",
                    ' ',
                    ""
                ),
            $input
        );

        return $input;
    }
}

function fn_minify_css($input)
{
    // Keep important white-space(s) in `calc()`
    if (stripos($input, 'calc(') !== false) {
        $input = preg_replace_callback('#\b(calc\()\s*(.*?)\s*\)#i', function ($m) {
            return $m[1] . preg_replace('#\s+#', X . '\s', $m[2]) . ')';
        }, $input);
    }

    // Minify ...
    return preg_replace(
        array(
            // Fix case for `#foo [bar="baz"]` and `#foo :first-child` [^1]
            '#(?<![,\{\}])\s+(\[|:\w)#',
            // Fix case for `[bar="baz"] .foo` and `url(foo.jpg) no-repeat` [^2]
            '#\]\s+#',
            '#\)\s+\b#',
            // Minify HEX color code ... [^3]
            '#\#([\da-f])\1([\da-f])\2([\da-f])\3\b#i',
            // Remove white-space(s) around punctuation(s) [^4]
            '#\s*([~!@*\(\)+=\{\}\[\]:;,>\/])\s*#',
            // Replace zero unit(s) with `0` [^5]
            '#\b(?:0\.)?0([a-z]+\b|%)#i',
            // Replace `0.6` with `.6` [^6]
            '#\b0+\.(\d+)#',
            // Replace `:0 0`, `:0 0 0` and `:0 0 0 0` with `:0` [^7]
            '#:(0\s+){0,3}0(?=[!,;\)\}]|$)#',
            // Replace `background(?:-position)?:(0|none)` with `background$1:0 0` [^8]
            '#\b(background(?:-position)?):(0|none)\b#i',
            // Replace `(border(?:-radius)?|outline):none` with `$1:0` [^9]
            '#\b(border(?:-radius)?|outline):none\b#i',
            // Remove empty selector(s) [^10]
            '#(^|[\{\}])(?:[^\s\{\}]+)\{\}#',
            // Remove the last semi-colon and replace multiple semi-colon(s) with a semi-colon [^11]
            '#;+([;\}])#',
            // Replace multiple white-space(s) with a space [^12]
            '#\s+#'
        ),
        array(
            // [^1]
            X . '\s$1',
            // [^2]
            ']' . X . '\s',
            ')' . X . '\s',
            // [^3]
            '#$1$2$3',
            // [^4]
            '$1',
            // [^5]
            '0',
            // [^6]
            '.$1',
            // [^7]
            ':0',
            // [^8]
            '$1:0 0',
            // [^9]
            '$1:0',
            // [^10]
            '$1',
            // [^11]
            '$1',
            // [^12]
            ' '
        ),
        $input
    );
}

function minify_css($input)
{
    if (!$input = trim($input)) {
        return $input;
    }
    global $SS, $CC;
    // Keep important white-space(s) between comment(s)
    $input = preg_replace('#(' . $CC . ')\s+(' . $CC . ')#', '$1' . X . '\s$2', $input);
    // Create chunk(s) of string(s), comment(s) and text
    $input = preg_split('#(' . $SS . '|' . $CC . ')#', $input, -1, PREG_SPLIT_NO_EMPTY | PREG_SPLIT_DELIM_CAPTURE);
    $output = "";
    foreach ($input as $v) {
        if (trim($v) === "") {
            continue;
        }
        if (
            ($v[0] === '"' && substr($v, -1) === '"') ||
            ($v[0] === "'" && substr($v, -1) === "'") ||
            (substr($v, 0, 2) === '/*' && substr($v, -2) === '*/')
        ) {
            // Remove if not detected as important comment ...
            if ($v[0] === '/' && substr($v, 0, 3) !== '/*!') {
                continue;
            }
            $output .= $v; // String or comment ...
        } else {
            $output .= fn_minify_css($v);
        }
    }
    // Remove quote(s) where possible ...
    $output = preg_replace(
        array(
            '#(' . $CC . ')|(?<!\bcontent\:)([\'"])([a-z_][-\w]*?)\2#i',
            '#(' . $CC . ')|\b(url\()([\'"])([^\s]+?)\3(\))#i'
        ),
        array(
            '$1$3',
            '$1$2$4$5'
        ),
        $output
    );

    return __minify_v($output);
}

function __minify_v($input)
{
    return str_replace(array(X . '\n', X . '\t', X . '\s'), array("\n", "\t", ' '), $input);
}
