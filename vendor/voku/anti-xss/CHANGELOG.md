# Changelog

### 4.1.42 (2023-07-03)

- add more js events e.g. "onbeforetoggle"

### 4.1.41 (2023-02-12)

- remove debug call (thx @gharlan [Gregor Harlan]

### 4.1.40 (2023-02-11)

- fix xss bypass (issue 115 | thx @thanosgn [Thanos Giannopoulos])
- add more blacklisted js calls (thx @peter-mw [Peter Ivanov])
- fix false positive on self-close tags (issue 111 | thx @genetus [Maxim Antonov])

### 4.1.39 (2022-03-08)

- "To people of Russia": There is a war in Ukraine right now. The forces of the Russian Federation are attacking civilians.

### 4.1.38 (2022-03-07)

- optimize "_sanitize_naughty_javascript" (issue 99 | thx @Fahl-Design)
- optimize "_do_never_allowed_afterwards", at least for strings in tags

### 4.1.37 (2022-02-15)

- move more static data into the object
  - allow to modify the "_never_allowed_call_strings"-data"
  - allow to modify the "_never_allowed_js_callback_regex"-data"

### 4.1.36 (2022-01-27)

- update "portable-utf8"

### 4.1.35 (2021-12-08)

- update "portable-utf8"

### 4.1.34 (2021-11-29)

- allow e.g. "< 1 year" (issue 83)
- fix false-positive issue (issue 85 | thx @gharlan)

### 4.1.33 (2021-10-04)

- fix errors in large strings
- fix "_xss_found" if xss string was found in array value

### 4.1.32 (2021-03-29)

- micro-optimize performance
- optimize phpdocs + use phpstan-syntax


### 4.1.31 (2020-12-02)

- optimize performance (thx @staabm)
- update vendor lib (Portable UTF-8)


### 4.1.30 (2020-11-12)

- update vendor lib (Portable UTF-8)


### 4.1.29 (2020-11-09)

- allow e.g. "<35%" (issue #62)
- allow to skip some html tags from auto closing (issue #63)
- run tests with PHP 8.0 rc3


### 4.1.28 (2020-08-28)

- fix allow base64 encoded images in <img>-tags (issue #61)
- fix performance issue of regex with "preg_match_all"


### 4.1.27 (2020-08-23)

- allow e.g. "< $2.20" (issue #60)
- optimize protection against HTML "script" tag stripping evasion
- auto-generate the api documentation into the README


### 4.1.26 (2020-08-08)

- allow base64 encoded images in <img>-tags (issue #59)


### 4.1.25 (2020-06-12)

- fix false-positive (issue #58)


### 4.1.24 (2020-03-08)

- allow to change the "_never_allowed_str_afterwards" (issue #56)
- fix false-positive (issue #55)


### 4.1.23 (2020-03-06)

- use some more bad strings from "https://github.com/s0md3v/AwesomeXSS"
- optimize some regex (use strpos before the regex)


### 4.1.22 (2020-02-06)

- fix false-positive (issue #54)
- optimize internal caching of strings


### 4.1.21 (2019-12-30)

- fix false-positive (issue #53)
- fix for "server-sent events"
- optimize regex for encoded script-tags (%3C && %3E)


### 4.1.20 (2019-12-07)

- fix additional false positives in string (issue #52)
- remove support for "Netscape 4 JS entities"


### 4.1.19 (2019-11-11)

- keep more non XSS content from html input


### 4.1.18 (2019-11-11)

- fix open tags problem e.g. "<img/"


### 4.1.17 (2019-11-08)

- add "addNeverAllowedRegex()"
- add "removeNeverAllowedRegex()"


### 4.1.16 (2019-11-03)

- fix replacing of "-->" (issue #50)
- update vendor lib (Portable UTF-8)


### 4.1.15 (2019-09-26)

- optimize regex
- update vendor lib (Portable UTF-8)


### 4.1.14 (2019-06-27)

- add "removeNeverAllowedOnEventsAfterwards()" && "addNeverAllowedOnEventsAfterwards()"
- update "_never_allowed_on_events_afterwards" -> add "onTouchend" + "onTouchLeave" + "onTouchMove" (thx @DmytroChymyrys)
- optimize phpdoc for array => string[]


### 4.1.13 (2019-06-08)

- fix replacing of false-positive xss words e.g. "<script@gmail.com>" (issue #44)


### 4.1.12 (2019-05-31)

- fix replacing of false-positive xss words e.g. "<video@gmail.com>" (issue #44)


### 4.1.11 (2019-05-28)

- fix replacing of false-positive xss words e.g. "<styler_tester@gmail.com>" (issue #44)


### 4.1.10 (2019-04-19)

- fix replacing of false-positive xss words e.g. "ANAMNESI E VAL!DEFINITE BREVI ORTO" (issue #43)


### 4.1.9 (2019-04-19)

- optimize the spacing regex


### 4.1.8 (2019-04-19)

- fix replacing of false-positive xss words e.g. "MONDRAGÓN" (issue #43)


### 4.1.7 (2019-04-19)

- fix replacing of false-positive xss words e.g. "DE VAL HERNANDEZ" (issue #43)


### 4.1.6 (2019-04-12)

- fix replacing of false-positive xss words e.g. "Mondragon" (issue #43)


### 4.1.5 (2019-02-13)

- fix issue with "()" in some html attributes (issue #41)


### 4.1.4 (2019-01-22)

- use new version of "Portable UTF8"


### 4.1.3 (2018-10-28)

- fix for url-decoded stored-xss
- fix return type (?string -> string)


### 4.1.2 (2018-09-13)

- use new version of "Portable UTF8"
- add some more event listener
- use PHPStan


### 4.1.1 (2018-04-26)

- "UTF7 repack corrected" | thx @alechner #34


### 4.1.0 (2018-04-17)

- keep the input value (+ encoding), if no xss was detected #32


### 4.0.3 (2018-04-12)

- fix "href is getting stripped" #30


### 4.0.2 (2018-02-14)

- fix "URL escaping bug" #29


### 4.0.1 (2018-01-07)

- fix usage of "Portable UTF8"


### 4.0.0 (2017-12-23)
- update "Portable UTF8" from v4 -> v5

  -> this is a breaking change without API-changes - but the requirement
     from "Portable UTF8" has been changed (it no longer requires all polyfills from Symfony)


### 3.1.0 (2017-11-21)
- add "_evil_html_tags" -> so you can remove / add html-tags


### 3.0.1 (2017-11-19)
- "php": ">=7.0"
  * use "strict_types"
- simplify a regex


### 3.0.0 (2017-11-19)
- "php": ">=7.0"
  * drop support for PHP < 7.0
