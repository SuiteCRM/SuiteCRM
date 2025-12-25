HOW TO CONTRIBUTE TO SuiteCRM
=============================

Please visit our [Getting Started][getting started] page for other ideas about how to contribute code, 
and our [Community Docs][suitecrm contributing code issues docs sprints] for further details on how to
contribute code, report issues, contribute to the documentation, and nominate open issues from github
for our periodic code sprints.

Development is done on GitHub in the [salesagility/SuiteCRM][suitecrm github] repository.

To request new features or report bugs, please [open an issue][open issue] here on GitHub.

To submit a patch, please [open a pull request][open pull] on GitHub.  If you are thinking
of making a large contribution, [open an issue][open issue] for it before starting work,
to get comments from the community.  Someone may be already working on
the same thing, or there may be reasons why that feature isn't implemented.

To make it easier to review and accept your pull request, please follow these
guidelines:

 1. Anything other than a trivial contribution requires a [Contributor
   License Agreement][cla] (CLA), giving us permission to use your code.
   If your contribution is too small to require a CLA (e.g. fixing a spelling
   mistake), place the text "`CLA: trivial`" on a line by itself separated by
   an empty line from the rest of the commit message. It is not sufficient to
   only place the text in the GitHub pull request description.

   To amend a missing "`CLA: trivial`" line after submission, do the following:

   ```bash
       git commit --amend
       [add the line, save and quit the editor]
       git push -f
   ```

 2. All source files should start with the following text (with
   appropriate comment characters at the start of each line and the
   year(s) updated):  <!-- update to match the real SuiteCRM php file header -->

   ```php
      // Copyright 20xx-20yy The SuiteCRM Project Authors. All Rights Reserved.
      //
      // Licensed under the GNU Affero Geenral Public License 3.0 ... (the "License").  You may not use
      // this file except in compliance with the License.  You can obtain a copy
      // in the file license.txt in the source distribution or at
      // https://github.com/salesagility/SuiteCRM/blob/hotfix/LICENSE.txt
   ```

 3. Patches should be as current as possible; expect to have to rebase
   often. We do not accept merge commits, you'll have to remove them
   (usually by rebasing) before your code submission will be acceptable.

 4. Patches should follow our [coding style][coding style] and run without PHP Warnings.
   You should enable all PHP error reporting, to show your uninitialized
   variables and catch variable name misspellings.  How?  Add this as first line of your code: 
   `error_reporting(E_ALL);`
   SuiteCRM runs on many varied platforms: try to ensure you only use
   portable PHP features.  Clean builds via `GitHub Actions`, `Travis-CI` and `Codacy` are required.
   These test builds are started automatically whenever you create or update your PR.

 5. When at all possible, code patches should include tests! You can
   either add to an existing test, or create a completely new test.  Please see
   [tests/README.md](tests/README.md) or the [SuiteCRM automated test docs][suitecrm auto test docs] 
   for how to write your tests for the test frameworks.

 6. New features or changed functionality must [include documentation][suitecrm contributing to docs]. 
   Please have a look at other similar relevant files for
   examples of our style. Make sure that your
   documentation changes are clean.

 7. For user visible changes (API changes, behaviour changes, ...),
   consider adding a note in [CHANGES.md](CHANGES.md).
   This could be a summarising description of the change, and could
   explain the grander details.
   Have a look through existing entries for inspiration.
   Please note that this is NOT simply a copy of `git-log` one-liners.
   Also note that security fixes get an entry in [CHANGES.md](CHANGES.md).
   This file helps users get more in depth information of what comes
   with a specific release without having to sift through the higher
   noise ratio in `git-log`.

 8. For larger or more important user visible changes, as well as
   security fixes, please add a line in [NEWS.md](NEWS.md).
   On exception, it might be worth adding a multi-line entry (such as
   the entry that announces all the backward compatibility changes that 
   came with SuiteCRM 8.0 API).
   This file helps users get a very quick summary of what comes with a
   specific release, to see the effort involved in the upgrade.

 9. Guidelines on how to add error reporting compatible 
    with the PHP runtime environment of SuiteCRM, 
    and how to properly handle PHP errors and exceptions
    both in your code contribution, and in your tests for your code,
    are in [err/README.md](err/README.md).

<!-- Links -->

[getting started]: https://docs.suitecrm.com/user/introduction/getting-started/
[suitecrm github]: https://github.com/salesagility/SuiteCRM
[cla]: https://cla.suitecrm.com/salesagility/SuiteCRM
[coding style]: https://docs.suitecrm.com/community/contributing-code/coding-standards/
[open pull]: https://github.com/salesagility/SuiteCRM/compare
[open issue]: https://github.com/salesagility/SuiteCRM/issues/new
[suitecrm auto test docs]: https://docs.suitecrm.com/developer/automatedtesting/
[suitecrm contributing code issues docs sprints]: https://docs.suitecrm.com/community/
[suitecrm contributing to docs]: https://docs.suitecrm.com/community/contributing-to-docs/
