# Change Log

### 4.10.1 - 13 Dec 2024

- Support PHP 8.4

### 4.10.0 - 5 Apr 2024

- Support Symfony 7

### 4.9.2 - 26 Dec 2023

- Avoid infinite loop in \Consolidation\AnnotatedCommand\Parser\CommandInfo::getName (#306)

### 4.9.1, 4.9.0 - 19 May 2023

- Inject command metadata: make AnnotationData and FormatterOptions available (#301)
- Only call setHidden() if value is true. (#299)

### 4.8.2 - 11 March 2023

- Fix bug with simple options with dashes in their names. (#295)
- Add new public ->addOption() method (#294)
- Attributes - Topic needs path (#293)
- More robust hook attribute, and misc (#291)

### 4.8.0, 4.8.1 - 28 February 2023

- Add HookSelector attribute and adjust Hook attribute accordingly (#290)
- Allow for just one param to be passed in Help and Topics Attributes (#289)

### 4.7.1 - 6 December 2022

- PHP 8.2 fixes

### 4.7.0 - 22 November 2022

- Allow command help and descriptions to be defined via docblock comments (#281)
- Fix hidden via Attribute (#283)
- Fix command completion (#282)

### 4.6.1 - 9 November 2022

- Use Attributes to set suggestedValues for arg/option completion (#275)
- Default value and therefore data type of parameter-defined cli options was being dropped (#280)
- Make nullable properties in Attributes actually nullable (#278)
- Only ignore commands in parent classes if flag is set (reverts backwards-compatibility break) (#277)

### 4.6.0 - 30 October 2022

- Add support for command completion (#274)

### 4.5.7 - 20 October 2022

- Stop loading commands from inherited classes (#273)

### 4.5.6 - 22 June 2022

- PHP 8.2 compatibility: dynamic properties are deprecated (#271)

### 4.5.5 - 26 April 2022

- No functional change; new release to fix false positives in b/c check caused by lockfile problem.

### 4.5.4 - 5 April 2022

- Allow psr/log ^3

### 4.5.3 - 1 April 2022

- Check the type of the reflection object before attempting to call isBuiltin(). (#265)

### 4.5.2 - 20 February 2022

- Do not pass null to Symfony Command methods (#262)
- CommandResult inheritance (#260)

### 4.5.1 - 29 December 2021

- PHP 8.1

### 4.5.0 - 27 December 2021

- Symfony 6 / Symfony 5.2 compatibility
- Make addUsageOrExample() public

### 4.4.0 - 29 September 2021

- Add support for providing command information via php8 Attributes. (#239)

### 4.3.3 - 26 September 2021

- Back out change from 4.3.2.  Will restore in 4.4.0, but with a switch that defaults to "off" (backwards-compatible).

### 4.3.2 - 19 September 2021

- Less parsing by ignoring Traits and IO.php (for Drush) (#237)

### 4.3.1 - 29 August 2021

- Fix bc break in 4.3.0. (#232)

### 4.3.0 - 27 August 2021

- Allow options to be passed in as regular method parameters. (#224)

### 4.2.4 - 10 December 2020

- PHP 8

### 4.2.3 - 3 October 2020

- Add ability to ignore methods using regular expressions. (#212)

### 4.2.2 - 30 September 2020

- PHP 8 / Composer 2 support (#210)
- Add @ignored-command annotation. (#211)
- Address deprecation of ReflectionType::getClass() (#209)

### 4.2.1 - 30 August 2020

- Give command handlers the ability to save and restore their state (#208)
- Do not inject $input and $output into the command instance unless it supports saving and restoring state.

### 4.2.0 - 27 August 2020

DEPRECATED RELEASE. Do not use.

- Inject $input and $output into the command instance if it is set up to receive them. (#207)

### 4.1.1 - 27 May 2020

- Fix bugs with Symfony 5. (#204)

### 4.1.0 - 6 Feb 2020

- Test with PHP 7.4.

### 4.0.0 - 29 Oct 2019

- Compatible with the 2.x branch, but removes support for old PHP versions and requires Symfony 4.

### 2.12.0 - 8 Mar 2019

- Allow annotated args and options to specify their default values in their descriptions. (#186)

### 2.11.2 - 1 Feb 2019

- Fix handling of old caches from 2.11.1 that introduced upgrade errors.

### 2.11.1 - 31 Jan 2019

- Cache injected classes (#182)

### 2.11.0 - 27 Jan 2019

- Make injection of InputInterface / OutputInterface general-purpose (#179)

### 2.10.2 - 20 Dec 2018

- Fix commands that have a @param annotation for their InputInterface/OutputInterface params (#176)

### 2.10.1 - 13 Dec 2018

- Add stdin handler convenience class
- Add setter to AnnotationData to suppliment existing array acces
- Update to Composer Test Scenarios 3

### 2.10.0 - 14 Nov 2018

- Add a new data type, CommandResult (#167)

### 2.9.0 & 2.9.1 - 19 Sept 2018

- Improve commandfile discovery for extensions installed via Composer. (#156)

### 2.8.5 - 18 Aug 2018

- Add dependencies.yml for dependencies.io
- Fix warning in AnnotatedCommandFactory when getCommandInfoListFromCache called with null.

### 2.8.4 - 25 May 2018

- Use g1a/composer-test-scenarios for better PHP version matrix testing.

### 2.8.3 - 23 Feb 2018

- BUGFIX: Do not shift off the command name unless it is there. (#139)
- Use test scenarios to test multiple versions of Symfony. (#136, #137)

### 2.8.2 - 29 Nov 2017

- Allow Symfony 4 components.

### 2.8.1 - 16 Oct 2017

- Add hook methods to allow Symfony command events to be added directly to the hook manager, givig better control of hook order. (#131)

### 2.8.0 - 13 Oct 2017

- Remove phpdocumentor/reflection-docblock in favor of using a bespoke parser (#130)

### 2.7.0 - 18 Sept 2017

- Add support for options with a default value of 'true' (#119)
- BUGFIX: Improve handling of options with optional values, which previously was not working correctly. (#118)

### 2.6.1 - 18 Sep 2017

- Reverts to contents of the 2.4.13 release.

### 2.5.0 & 2.5.1 - 17 Sep 2017

- BACKED OUT. These releases accidentally introduced breaking changes.

### 2.4.13 - 28 Aug 2017

- Add a followLinks() method (#108)

### 2.4.12 - 24 Aug 2017

- BUGFIX: Allow annotated commands to directly use InputInterface and OutputInterface (#106)

### 2.4.11 - 27 July 2017

- Back out #102: do not change behavior of word wrap based on STDOUT redirection.

### 2.4.10 - 21 July 2017

- Add a method CommandProcessor::setPassExceptions() to allow applicationsto prevent the command processor from catching exceptions thrown by command methods and hooks. (#103)

### 2.4.9 - 20 Jul 2017

- Automatically disable wordwrap when the terminal is not connected to STDOUT (#102)

### 2.4.8 - 3 Apr 2017

- Allow multiple annotations with the same key. These are returned as a csv, or, alternately, can be accessed as an array via the new accessor.
- Unprotect two methods for benefit of Drush help. (#99)
- BUGFIX: Remove symfony/console pin (#100)

### 2.4.7 & 2.4.6 - 17 Mar 2017

- Avoid wrapping help text (#93)
- Pin symfony/console to version < 3.2.5 (#94)
- Add getExampleUsages() to AnnotatedCommand. (#92)

### 2.4.5 - 28 Feb 2017

- Ensure that placeholder entries are written into the commandfile cache. (#86)

### 2.4.4 - 27 Feb 2017

- BUGFIX: Avoid rewriting the command cache unless something has changed.
- BUGFIX: Ensure that the default value of options are correctly cached.

### 2.4.2 - 24 Feb 2017

- Add SimpleCacheInterface as a documentation interface (not enforced).

### 2.4.1 - 20 Feb 2017

- Support array options: multiple options on the commandline may be passed in to options array as an array of values.
- Add php 7.1 to the test matrix.

### 2.4.0 - 3 Feb 2017

- Automatically rebuild cached commandfile data when commandfile changes.
- Provide path to command file in AnnotationData objects.
- Bugfix: Add dynamic options when user runs '--help my:command' (previously, only 'help my:command' worked).
- Bugfix: Include description of last parameter in help (was omitted if no options present)
- Add Windows testing with Appveyor


### 2.3.0 - 19 Jan 2017

- Add a command info cache to improve performance of applications with many commands
- Bugfix: Allow trailing backslashes in namespaces in CommandFileDiscovery
- Bugfix: Rename @topic to @topics


### 2.2.0 - 23 November 2016

- Support custom events
- Add xml and json output for replacement help command. Text / html format for replacement help command not available yet.


### 2.1.0 - 14 November 2016

- Add support for output formatter wordwrapping
- Fix version requirement for output-formatters in composer.json
- Use output-formatters ~3
- Move php_codesniffer back to require-dev (moved to require by mistake)


### 2.0.0 - 30 September 2016

- **Breaking** Hooks with no command name now apply to all commands defined in the same class. This is a change of behavior from the 1.x branch, where hooks with no command name applied to a command with the same method name in a *different* class.
- **Breaking** The interfaces ValidatorInterface, ProcessResultInterface and AlterResultInterface have been updated to be passed a CommandData object, which contains an Input and Output object, plus the AnnotationData.
- **Breaking** The Symfony Command Event hook has been renamed to COMMAND_EVENT.  There is a new COMMAND hook that behaves like the existing Drush command hook (i.e. the post-command event is called after the primary command method runs).
- Add an accessor function AnnotatedCommandFactory::setIncludeAllPublicMethods() to control whether all public methods of a command class, or only those with a @command annotation will be treated as commands. Default remains to treat all public methods as commands. The parameters to AnnotatedCommandFactory::createCommandsFromClass() and AnnotatedCommandFactory::createCommandsFromClassInfo() still behave the same way, but are deprecated. If omitted, the value set by the accessor will be used.
- @option and @usage annotations provided with @hook methods will be added to the help text of the command they hook.  This should be done if a hook needs to add a new option, e.g. to control the behavior of the hook.
- @option annotations can now be either `@option type $name description`, or just `@option name description`.
- `@hook option` can be used to programatically add options to a command.
- A CommandInfoAltererInterface can be added via AnnotatedCommandFactory::addCommandInfoAlterer(); it will be given the opportunity to adjust every CommandInfo object parsed from a command file prior to the creation of commands.
- AnnotatedCommandFactory::setIncludeAllPublicMethods(false) may be used to require methods to be annotated with @commnad in order to be considered commands. This is in preference to the existing parameters of various command-creation methods of AnnotatedCommandFactory, which are now all deprecated in favor of this setter function.
- If a --field option is given, it will also force the output format to 'string'.
- Setter methods more consistently return $this.
- Removed PassThroughArgsInput. This class was unnecessary.


### 1.4.0 - 13 September 2016

- Add basic annotation hook capability, to allow hook functions to be attached to commands with arbitrary annotations.


### 1.3.0 - 8 September 2016

- Add ComandFileDiscovery::setSearchDepth(). The search depth applies to each search location, unless there are no search locations, in which case it applies to the base directory.


### 1.2.0 - 2 August 2016

- Support both the 2.x and 3.x versions of phpdocumentor/reflection-docblock.
- Support php 5.4.
- **Bug** Do not allow an @param docblock comment for the options to override the meaning of the options.


### 1.1.0 - 6 July 2016

- Introduce AnnotatedCommandFactory::createSelectedCommandsFromClassInfo() method.


### 1.0.0 - 20 May 2016

- First stable release.
