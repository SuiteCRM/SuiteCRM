# Change Log

### 4.6.0 - 18 Oct 2024

- Add setTableDefaultFields method to FormatterOptions

### 4.5.0 - 2 Apr 2024

- Support Symfony 7 (#111)

### 4.4.0 - 20 Mar 2024

- Make table headers optional when table is empty (#108)

### 4.3.2 - 5 Jul 2023

- Make var-dump and var-export usable for non-arrays (#109)
- Wordwrap fix for tables (#107)

### 4.3.1, 4.3.0 - 19 May 2023

- Add an accessor to get selected fields from the formatter object. (#104)
- Add convenience function FormatterOptions::fieldsContain() (#105)

### 4.2.4 - 23 Feb 2023

- Minor dynamic property fix for PHP 8.2.

### 4.2.3 - 16 Oct 2022

- Various PHP 8.1 compatibility fixes (warnings)

### 4.2.2 - 13 Feb 2022

- Allow dflydev/dot-access-data ^2 and ^3 (#98)

### 4.2.1 - 29 Dec 2021

- PHP 8.1

### 4.2.0 - 27 Dec 2021

- Symfony 6 support

### 4.1.3 - 11 Mar 2021

- No significant code changes, but removal of a method with a typo caused a b/c check failure.

### 4.1.2 - 10 Dec 2020

- PHP 8

### 4.1.1 - 27 May 2020

- Fix Symfony 5 bugs. (#85)

### 4.1.0 - 6 Feb 2020

- Test with PHP 7.4.

### 4.0.0 - 29 Oct 2019

- Compatible with the 3.x branch, but removes support for old PHP versions and requires Symfony 4.

### 3.5.0 - 30 May 2019

- Add `@default-table-fields` to specify the fields to use with the table formatter and other "human readable" output formats.

### 3.4.1 - 13 March 2019

- Add enclosure and escape character options for CsvFormatter. (#79)

### 3.4.0 - 19 October 2018

- Add an UnstucturedInterface marker interface, and update the 'string' format to not accept data types that implement this interface unless they also implement StringTransformationInterface.

### 3.3.2 - 18 October 2018

- Add a 'null' output formatter that accepts all data types and never produces output

### 3.3.0 & 3.3.1 - 15 October 2018

- Add UnstructuredListData and UnstructuredData to replace deprecated ListDataFromKeys
- Support --field and --fields in commands that return UnstructuredData / UnstructuredListData
- Support field remapping, e.g. `--fields=original as remapped`
- Support field addressing, e.g. `--fields=a.b.c`
- Automatically convert from RowsOfFields to UnstruturedListData and from PropertyList to UnstructuredData when user utilizes field remapping or field addressing features.

### 3.2.1 - 25 May 2018

- Rename g1a/composer-test-scenarios

### 3.2.0 - 20 March 2018

- Add RowsOfFieldsWithMetadata: allows commands to return an object with metadata that shows up in yaml/json (& etc.) formats, but is not shown in table/csv (& etc.).
- Add NumericCellRenderer: allows commands to attach a renderer that will right-justify and add commas to numbers in a column.
- Add optional var_dump output format.

### 3.1.13 - 29 November 2017

- Allow XML output for RowsOfFields (#60).
- Allow Symfony 4 components and add make tests run on three versions of Symfony.

### 3.1.12 - 12 October 2017

- Bugfix: Use InputOption::VALUE_REQUIRED instead of InputOption::VALUE_OPTIONAL
  for injected options such as --format and --fields.
- Bugfix: Ignore empty properties in the property parser.

### 3.1.11 - 17 August 2017

- Add ListDataFromKeys marker data type.

### 3.1.10 - 6 June 2017

- Typo in CalculateWidths::distributeLongColumns causes failure for some column width distributions

### 3.1.9 - 8 May 2017

- Improve wrapping algorithm

### 3.1.7 - 20 Jan 2017

- Add Windows testing

### 3.1.6 - 8 Jan 2017

- Move victorjonsson/markdowndocs to require-dev

### 3.1.5 - 23 November 2016

- When converting from XML to an array, use the 'id' or 'name' element as the array key value.

### 3.1.4 - 20 November 2016

- Add a 'list delimiter' formatter option, so that we can create a Drush-style table for property lists.

### 3.1.1 ~ 3.1.3 - 18 November 2016

- Fine-tune wordwrapping.

### 3.1.0 - 17 November 2016

- Add wordwrapping to table formatter.

### 3.0.0 - 14 November 2016

- **Breaking** The RenderCellInterface is now provided a reference to the entire row data. Existing clients need only add the new parameter to their method defnition to update.
- Rename AssociativeList to PropertyList, as many people seemed to find the former name confusing. AssociativeList is still available for use to preserve backwards compatibility, but it is deprecated.


### 2.1.0 - 7 November 2016

- Add RenderCellCollections to structured lists, so that commands may add renderers to structured data without defining a new structured data subclass.
- Throw an exception if the client requests a field that does not exist.
- Remove unwanted extra layer of nesting when formatting an PropertyList with an array formatter (json, yaml, etc.).


### 2.0.0 - 30 September 2016

- **Breaking** The default `string` format now converts non-string results into a tab-separated-value table if possible.  Commands may select a single field to emit in this instance with an annotation: `@default-string-field email`.  By this means, a given command may by default emit a single value, but also provide more rich output that may be shown by selecting --format=table, --format=yaml or the like.  This change might cause some commands to produce output in situations that previously were not documented as producing output.
- **Breaking** FormatterManager::addFormatter() now takes the format identifier and a FormatterInterface, rather than an identifier and a Formatter classname (string).
- --field is a synonym for --fields with a single field.
- Wildcards and regular expressions can now be used in --fields expressions.


### 1.1.0 - 14 September 2016

Add tab-separated-value (tsv) formatter.


### 1.0.0 - 19 May 2016

First stable release.
