Testing
-------

This document describes the project's approach to testing. Essentially we do not require tests
for all code but do strive to maintain reasonable test coverage.


Local testing
-------------

To run local tests, simply execute the appropriate group of tests in the `utils` subdirectory.

For example:

```sh
$ php utils/test.php          # Run main tests
$ php utils/ootest.php        # Run object-oriented wrapper tests
$ php utils/other-test.php    # Run additional tests
```

Automated testing
-----------------

The project uses the free Travis Continuous Integration (CI) service to test new code.

The service runs automatically every time code is committed, and the results will be emailed
and displayed publicly on the project's github page.
