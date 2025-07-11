SuiteCRM User Support resources
===============================

See the [SuiteCRM Enterprise Support Plans][suitecrm enterprise support plans] for details on how to
obtain paid commercial technical support.

If you have general questions about using SuiteCRM
--------------------------------------------------

In this case, the [community forum][community forum] is the right place for you.
The forum is not only watched by the SuitCRM team members, but also by many
other SuiteCRM users. Here you will most likely get the answer to your questions.

If you think you found a Bug
----------------------------

*NOTE: this section assumes that you want to report it or figure it out and
fix it.  What's written here is not to be taken as a recipe for how to get a
working production installation*

If you have any problems with SuiteCRM then please take the following steps
first:

  - Search the community forum and/or the GitHub issues to find out whether
  the problem has already been reported.
  
  - Download the latest version from the repository to see if the problem
  has already been addressed.
  
  - Make note of the series of steps you take to get the problem to 
  reproduce consistently.

Please keep in mind: Just because something doesn't work the way you expect
does not mean it is necessarily a bug in SutieCRM. If you are not sure,
consider searching the community forum and posting a question to the
[community forum][community forum] first.

### Open an Issue

If you wish to report a bug, please [open an issue][open-github-issue] on GitHub
and include the following information:

  - SuiteCRM version: contents of `suitecrm_version.php`
  - Configuration data: contents of `config.php` and `config_override.php` NOTE: You must redact private info such as: names, passwords, API keys, and API secrets, from these files!
  - OS Name, OS Version, Hardware platform: On Linux, the output of `uname -a`
  - PHP Version: the output of `php -v`
  - SuiteCRM application details (Community, Assured, etc)
  - Problem Description (steps that will reproduce the problem, if known)
  - Stack Traceback (if any, in `php_error.log`, and in `suitecrm.log`)

Not only errors in the software, but also errors in the documentation,
you can report as issues on github.

### Submit a Pull Request

The fastest way to get a bug fixed is to fix it yourself :wink: . If you are
experienced in programming and know how to fix the bug, you can open a
pull request. The details are covered in the [Contributing][contributing] section.

Don't hesitate to open a pull request, even if it's only a small change
like a grammatical or typographical error in the language files, or in the documentation.

<!-- Links -->
[suitecrm enterprise support plans]: https://suitecrm.com/enterprise/support-plans/
[community forum]: https://community.suitecrm.com
[open-github-issue]: https://github.com/salesagility/SuiteCRM/issues/new
[contributing]: https://github.com/salesagility/SuiteCRM/blob/master/CONTRIBUTING.md
