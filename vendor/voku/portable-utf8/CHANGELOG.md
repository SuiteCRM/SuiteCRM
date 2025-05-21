# Changelog

### 6.0.13 (2023-03-08)

- added "UTF8::str_limit_in_byte()"

### 6.0.12 (2023-01-11)

- added "UTF8::remove_iright()" 
- added "UTF8::remove_ileft()"

### 6.0.11 (2022-12-16)

- test with PHP 8.2
- added "UTF8::getUrlParamFromArray()": Get data from an array via array like string.

### 6.0.10 (2022-10-06)

- optimize phpdocs (use positive-int)

### 6.0.9 (2022-09-01)

- optimize phpdocs (use int-range)

### 6.0.8 (2022-09-01)

- optimize phpdocs (use Conditional Return Types) v2

### 6.0.7 (2022-09-01)

- optimize phpdocs (use Conditional Return Types)

### 6.0.6 (2022-08-31)

- optimize phpdocs
- optimize "from"-encoding in "UTF8::encode()"
- fix error handling in "UTF8::str_split()" for PHP >= 8.0
- fix error handling in "UTF8::str_capitalize_name_helper()" for PHP >= 8.0

### 6.0.5 (2022-08-10)

- PHP 8.2: Fix ${var} string interpolation deprecation | thanks @Ayesh

### 6.0.4 (2022-03-08)

- "To people of Russia": There is a war in Ukraine right now. The forces of the Russian Federation are attacking civilians.

### 6.0.3 (2022-01-30)

- optimize phpdocs

### 6.0.2 (2022-01-24)

- update vendor (ASCII) lib

### 6.0.1 (2022-01-24)

- add "UTF8::levenshtein()" | thanks @NewEXE
- update vendor (ASCII) lib

### 6.0.0 (2021-12-08)

Removed:
- Bootup::filterRequestInputs();
- Bootup::filterRequestUri();
- UTF8::GRAPHEME_CLUSTER_RX
- UTF8::chr_to_int() (please use UTF8::chr_to_decimal())
- UTF8::hasBom() (please use UTF8::string_has_bom())
- UTF8::html_decode() (please use UTF8::html_entity_decode())
- UTF8::int_to_chr() (please use UTF8::decimal_to_chr())
- UTF8::isAscii() (please use UTF8::is_ascii())
- UTF8::isBase64() (please use UTF8::is_base64())
- UTF8::isBinary() (please use UTF8::is_binary())
- UTF8::isBom() (please use UTF8::is_bom())
- UTF8::isHtml() (please use UTF8::is_html())
- UTF8::isJson() (please use UTF8::is_json())
- UTF8::isUtf16() (please use UTF8::is_utf16())
- UTF8::isUtf32() (please use UTF8::is_utf32())
- UTF8::isUtf8() (please use UTF8::is_utf8())
- UTF8::lcword() (please use UTF8::lcfirst())
- UTF8::lowerCaseFirst() (please use UTF8::lcfirst())
- UTF8::normalizeEncoding() (please use UTF8::normalize_encoding())
- UTF8::removeBOM() (please use UTF8::remove_bom())
- UTF8::split() (please use UTF8::str_split())
- UTF8::str_begins() (please use UTF8::str_starts_with())
- UTF8::str_ends() (please use UTF8::str_ends_with())
- UTF8::str_ibegins() (please use UTF8::str_istarts_with())
- UTF8::str_iends() (please use UTF8::str_iends_with())
- UTF8::str_iindex_first() (please use UTF8::stripos())
- UTF8::str_iindex_last() (please use UTF8::strripos())
- UTF8::str_index_first() (please use UTF8::strpos())
- UTF8::str_index_last() (please use UTF8::strrpos())
- UTF8::str_transliterate() (please use UTF8::to_ascii())
- UTF8::str_upper_first() (please use UTF8::ucfirst())
- UTF8::strchr() (please use UTF8::strstr()) 
- UTF8::strichr() (please use UTF8::stristr())
- UTF8::toAscii() (please use UTF8::to_ascii())
- UTF8::toIso8859() (please use UTF8::to_iso8859())
- UTF8::toLatin1() (please use UTF8::to_iso8859())
- UTF8::to_latin1() (please use UTF8::to_iso8859())
- UTF8::toUTF8() (please use UTF8::to_utf8())
- UTF8::ucword() (please use UTF8::ucfirst())
- UTF8::urldecode_fix_win1252_chars() (please use UTF8::urldecode())
- UTF8::utf8_fix_win1252_chars() (please use UTF8::fix_simple_utf8())

Fixed:
- "UTF8::is_binary()" -> fix for strict usage

### 5.4.51 (2020-12-02)

- "UTF8::rawurldecode()" -> optimize performance
- "UTF8::urldecode()" -> optimize performance
- "UTF8::to_boolean() -> fix phpdoc

### 5.4.50 (2020-11-12)

- "UTF8::normalize_whitespace()" -> can now also normalize "control characters" if needed

### 5.4.49 (2020-11-12)

- "UTF8::remove_invisible_characters()" -> can now also remove "control characters" if needed
- "UTF8::is_printable()" -> fix for non-printable control characters

### 5.4.48 (2020-11-07)

- fix "UTF8::chr()" handling of non "int" code points
- fix "UTF8::html_encode()" (parameter mismatch for "mb_encode_numericentity()")
- fix "UTF8::str_ireplace()" (TypeError of "preg_replace()")
- fix "UTF8::strtonatfold()" (TypeError of "preg_replace()")
- fix "UTF8::substr_count()" return type if "$haystack" is an empty string
- compatibility with PHP 8.0 behaviour for:
    - UTF::strstr()
    - UTF::stristr()
    - UTF::strpos()
    - UTF::stripos()
    - UTF::strrpos()
    - UTF::strripos()
- use native functions from PHP 8.0
    - \str_contains()
    - \str_starts_with()
    - \str_ends_with()

### 5.4.47 (2020-07-26)

- optimize performance by re-using the result of "strlen()" 

### 5.4.46 (2020-07-18)

- add "UTF8::str_obfuscate()"
- optimize phpdocs

### 5.4.45 (2020-05-26)

- fix UTF8::(l|r)trim | thanks @pmacko

### 5.4.44 (2020-05-24)

- update vendor (ASCII) lib

### 5.4.43 (2020-05-14)

- fix auto-generate the "API documentation" in the README (via generate_docs.php)
- optimize phpdoc comments
- fix autoloader for test files

### 5.4.42 (2020-05-05)

- add "UTF8::css_identifier()"
- optimize phpdoc comments
- use more template phpdoc-annotation (supported by phpstan and psalm)
- move code examples into the code
- auto-generate the "API documentation" in the README (via generate_docs.php)

### 5.4.41 (2020-03-06)

- fix "UTF8::is_utf8*" -> detecting when last byte is incomplete multibyte character | big thanks @daniel-jeffery 

### 5.4.40 (2020-02-23)

- fix php notices (run all tests with E_ALL ^ E_USER_WARNING)

### 5.4.39 (2020-01-30)

- "GRAPHEME_CLUSTER_RX" -> is not used anymore and is now deprecated
- fix "UTF8::decode_mimeheader" fallback -> now we always use the symfony polyfill (mb_decode_mimeheader has different results)
- fix "UTF8::get_unique_string()" -> use "mt_rand" as fallback
- fix "UTF8::strtr()" -> now it works also with arrays
- fix phpdoc for "UTF8::normalize_line_ending()"
- fix phpdoc for "UTF8::split()" & "UTF8::str_split()"
- add "UTF8::str_split_array()"
- add "UTF8::stripos_in_byte()"
- add "UTF8::emoji_from_country_code()"
- add many new tests
- optimize "UTF8::is_url()" + fix deprecated php (>= 7.3) constants
- optimize "UTF8::str_limit_after_word()" -> optimize the regex
- optimize "UTF8::substr()" -> combine "if"-statements
- optimize "UTF8::str_capitalize_name_helper()" -> performance -> use break
- code style: fix for "UTF8::filter()"
- code style: do not use "=== false" | "=== true" for "bool" types

### 5.4.38 (2020-01-14)

- add "UTF8::is_url()"

### 5.4.37 (2019-12-30)

- fix nesting function error from "UTF8::substr()"

### 5.4.36 (2019-12-30)

- add "is_punctuation()" && "is_printable()"

### 5.4.35 (2019-12-27)

- add "UTF8::to_int()" && "UTF8::to_string()"

### 5.4.34 (2019-12-19)

- use "@psalm-pure"

### 5.4.33 (2019-12-18)

- use "@psalm-immutable"

### 5.4.32 (2019-12-13)

- fix "UTF8::str_contains_all" -> "strpos(): Empty needle"

### 5.4.31 (2019-12-13)

- update vendor (ASCII) lib
- optimize phpstan config

### 5.4.30 (2019-12-04)

- fix "UTF8::str_contains_all" -> fix the loop

### 5.4.29 (2019-12-02)

- add "UTF8::has_whitespace()"

### 5.4.28 (2019-11-17)

- use "mb_str_split" with PHP >= 7.4 + mbstring support (performance++)
- improve performance from "UTF8::string()" (use UTF8::html_entity_decode() for the full string)
- fix errors reported by phpstan (level 7) / psalm

### 5.4.27 (2019-11-11)

- "UTF8::clean() / UTF8::cleanup()" -> do not remove invisible urlencoded strings by default

      -> problem with e.g. pdf content: "%1b;" -> ...p(CRnAYOD*9a1>VAk^mH%1b;?ZVuX$`P[%...

### 5.4.26 (2019-11-05)

- disable "Bootup::filterRequestUri()" && "Bootup::filterRequestInputs()" by default
      
      Since version 5.4.26 this library will NOT force "UTF-8" by "bootstrap.php" anymore.
      If you need to enable this behavior you can define "PORTABLE_UTF8__ENABLE_AUTO_FILTER", 
      before requiring the autoloader.
      
      ```
      define('PORTABLE_UTF8__ENABLE_AUTO_FILTER', 1);
      ```
      
      Before version 5.4.26 this behavior was enabled by default and you could disable it via "PORTABLE_UTF8__DISABLE_AUTO_FILTER", but the code had potential security vulnerabilities via injecting code while redirecting via ```header('Location ...```.
      This is the reason I decided to add this BC in a bug fix release, so that everybody using the current version will receive the security-fix.

### 5.4.25 (2019-10-14)

- update vendor (ASCII) lib

### 5.4.24 (2019-10-06)

- improve performance from "UTF8::UTF8::str_titleize_for_humans()" (use "array_merge" only if needed)
- improve performance from "UTF8::ucwords()" + "UTF8::lcwords()" (don't use "implode()" if it's not needed)

### 5.4.23 (2019-09-27)

- improve performance from "UTF8::chr_to_decimal()" (now we use iconv if it's available)
- improve performance from "UTF8::html_entity_decode()" (code cleanup, remove dead code)

### 5.4.22 (2019-09-26)

- improve performance by replacing ```count($a) === 0``` with ```$a === []```
   -> so we don't need a function call to check if an array is empty

### 5.4.21 (2019-09-17)

- improve performance by not using return types for ```private``` methods
   -> the code is already checked via phpstan + psalm + phpstorm, ...
      so no need to check it every time at runtime at this point

### 5.4.20 (2019-09-16)

- fix "preg_quote()" usage
- fix return type from "mb_encode_numericentity" & "mb_decode_numeric_entity" usage
- add "@deprecated" for all alias methods

### 5.4.19 (2019-09-05)

- move ASCII functions into a separated package "Portable ASCII"

### 5.4.18 (2019-08-21)

- optimize "UTF8::str_titleize()" + clean-up

### 5.4.17 (2019-08-21)

- fix "UTF8::get_file_type()" -> do not add too simple comparisons, because of false-positive results
- extend "UTF8::str_titleize()" -> allow to add "word"-chars as new parameter

### 5.4.16 (2019-08-15)

- optimize "UTF8::str_detect_encoding()"

### 5.4.15 (2019-08-06)

- extend "UTF8::range()" -> support for different steps
- fix "UTF8::str_detect_encoding()" detecting of "UTF-32"
- revert: use "mb_detect_order()" for "UTF8::str_detect_encoding()"

### 5.4.14 (2019-08-02)

- extend "UTF8::range()" -> support for e.g. "a-z" + different encoding

### 5.4.13 (2019-08-01)

- extend "UTF8::wordwrap_per_line()" -> split the input by "$delimiter"

### 5.4.12 (2019-07-31)

- fix "UTF8::wordwrap_per_line()" -> use unicode version of "wordwrap"

### 5.4.11 (2019-07-19)

- use "mb_detect_order()" for "UTF8::str_detect_encoding()"
- code clean-up + optimize regex usage

### 5.4.10 (2019-07-05)

- fix "UTF8::str_contains_any()" -> thanks @drupalista-br

### 5.4.9 (2019-06-19)

- sync with "PHP RFC: Add str begin and end functions"
- fix "UTF8::rawurldecode()" and "UTF8::urldecode()" -> for non multi usage

### 5.4.8 (2019-06-08)

- fix typo in "win1252_to_utf8.php"
- optimize "UTF8::fix_simple_utf8()"

### 5.4.7 (2019-05-28)

- optimize performance for "bootstrap.php"

### 5.4.6 (2019-04-25)

- fix "UTF8::to_latin1()" for non-char input + optimize performance

### 5.4.5 (2019-04-21)

- fix unicode support for regex 

### 5.4.4 (2019-04-15)

- optimize performance for UTF8::rawurldecode() and UTF8::urldecode()
- optimize "UTF8::str_split_pattern()" with limit usage
- fix warnings detected by psalm && phpstan && phpstorm

### 5.4.3 (2019-03-05)

- optimize "UTF8::strrev()" with support for emoji chars
- added "UTF8::emoji_encode()" + "UTF8::emoji_decode()"

### 5.4.2 (2019-02-11)

- optimize html-encoding for unicode surrogate pairs (e.g. UTF-16)

### 5.4.1 (2019-02-10)

- optimize some RegEx
- fix html-encoding for unicode surrogate pairs (e.g. UTF-16)

### 5.4.0 (2019-01-22)
- optimize performance | thx @fe3dback
  -> e.g. use "\mb_"-functions without encoding parameter
  -> e.g. simplify logic of "UTF8::str_pad()"
- no more 100% support for "mbstring_func_overload", it's already deprecated in php
- move "UTF8::checkForSupport()" into "bootstrap.php"
- fix output from "UTF8::str_pad()" + empty input string
- add more "encoding" parameter e.g. for "UTF8::str_shuffle()"
- remove really old fallback for breaking-changes
- do not use aliases for internal processing

### 5.3.3 (2019-01-11)
- update "UTF8::is_json()" + tests

### 5.3.2 (2019-01-11)
- update "UTF8::is_base64()" + tests

### 5.3.1 (2019-01-11)
- update "UTF8::str_truncate_safe()" + tests

### 5.3.0 (2019-01-10)
- use autoloader + namespace for "tests/"
- fixes suggested by "phpstan" level 7
- fixes suggested by "psalm" 
- use variable references whenever possible
- use types for callback functions
- sync "UTF8::strcspn()" with native "strcspn()"
- sync "UTF8::strtr()" with native "strtr()"

### 5.2.16 (2019-01-02)
- update phpcs fixer config
- optimizing via "rector/rector"

### 5.2.15 (2018-12-18)
- optimize "getData()"
- use phpcs fixer

### 5.2.14 (2018-12-07)
- optimize "UTF8::str_replace_beginning()" && "UTF8::str_replace_ending()"
- added "UTF8::str_ireplace_beginning()" && "UTF8::str_ireplace_ending()"

### 5.2.13 (2018-11-29)
- "UTF8::get_file_type()" is now public + tested

### 5.2.12 (2018-11-29)
- optimize "UTF8::ord()" performance

### 5.2.11 (2018-10-19)
- merge UTF8::titlecase() && UTF8::str_titleize()
- add new langage + keep-string-length arguments for string functions

### 5.2.10 (2018-10-19)
- test with PHP 7.3

### 5.2.9 (2018-10-01)
- fix binary check for UTF16 / UTF32

### 5.2.8 (2018-09-29)
- "composer.json" -> remove extra alias
- UTF8::substr_replace() -> optimize performance
- UTF8::clean() -> add tests with "\00"
- update "UTF8::get_file_type()"
- fix fallback for "UTF8::encode()"

### 5.2.7 (2018-09-15)
- simplify "UTF8::encode()"

### 5.2.6 (2018-09-15)
- use more vanilla php fallbacks
- new encoding-from / -to parameter for "UTF8::encode()"
- optimize "mbstring_func_overload" fallbacks

### 5.2.5 (2018-09-11)
- more fixes for "mbstring_func_overload"

### 5.2.4 (2018-09-11)
- optimize performance for "UTF8::remove_bom()"
- optimize performance for "UTF8::is_binary()"
- fix tests with "mbstring_func_overload"

### 5.2.3 (2018-09-07)
- fix some breaking changes from "strict_types=1"

### 5.2.2 (2018-09-06)
- use "UTF8::encode()" internal ...

### 5.2.1 (2018-08-06)
- add more php-warnings
- optimize native php fallback
- fix tests without "mbstring"-ext
- UTF8::strlen() can return "false", if "mbstring" is not installed
    
### 5.2.0 (2018-08-05)
- use phpstan (+ fixed many code smells)
- added more tests

### 5.1.0 (2018-08-03)
- merge methods from "Stringy" into "UTF8"
- added many new tests

### 5.0.6 (2018-05-02)
- fix "UTF8::to_ascii()"
- update encoding list for "UTF8::str_detect_encoding()"
- use root namespaces for php functions


### 5.0.5 (2018-02-14)
- update -> "require-dev" -> "phpunit"


### 5.0.4 (2018-01-07)
- performance optimizing
  -> use "UTF8::normalize_encoding()" if needed
  -> use "CP850" encoding only if needed
  -> don't use "UTF8::html_encode()" in a foreach-loop


### 5.0.3 (2018-01-02)
- fix tests without "finfo" (e.g. appveyor - windows)
- optimize "UTF8::str_detect_encoding()"
  -> return "false" if we detect binary data, but not for UTF-16 / UTF-32


### 5.0.2 (2018-01-02)
- optimize "UTF8::is_binary()" v2
- edit "UTF8::clean()" -> do not remote diamond question mark by default
  -> fix for e.g. UTF8::file_get_contents() + auto encoding detection


### 5.0.1 (2018-01-01)
- optimize "UTF8::is_binary()" + new tests


### 5.0.0 (2017-12-10)
- "Fixed symfony/polyfill dependencies"

-> this is a breaking change, because "symfony/polyfill" contains more dependencies as we use now

before:
    "symfony/polyfill-apcu": "~1.0",
    "symfony/polyfill-php54": "~1.0",
    "symfony/polyfill-php55": "~1.0",
    "symfony/polyfill-php56": "~1.0",
    "symfony/polyfill-php70": "~1.0",
    "symfony/polyfill-php71": "~1.0",
    "symfony/polyfill-php72": "~1.0",
    "symfony/polyfill-iconv": "~1.0",
    "symfony/polyfill-intl-grapheme": "~1.0",
    "symfony/polyfill-intl-icu": "~1.0",
    "symfony/polyfill-intl-normalizer": "~1.0",
    "symfony/polyfill-mbstring": "~1.0",
    "symfony/polyfill-util": "~1.0",
    "symfony/polyfill-xml": "~1.0"
        
after:
    "symfony/polyfill-php72": "~1.0",
    "symfony/polyfill-iconv": "~1.0",
    "symfony/polyfill-intl-grapheme": "~1.0",
    "symfony/polyfill-intl-normalizer": "~1.0",
    "symfony/polyfill-mbstring": "~1.0"


### 4.0.1 (2017-11-13)
- update php-unit to 6.x


### 4.0.0 (2017-11-13)
- "php": ">=7.0"
  * drop support for PHP < 7.0
  * use "strict_types"
  * "UTF8::number_format()" -> removed deprecated method 
  * "UTF8::normalize_encoding()" -> change $fallback from bool to empty string
