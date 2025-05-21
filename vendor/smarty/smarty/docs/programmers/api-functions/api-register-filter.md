registerFilter()

dynamically register filters

Description
===========

void

registerFilter

string

type

mixed

callback

Use this to dynamically register filters to operate on a templates. It
uses the following parameters:

NOTE.PARAMETER.FUNCTION

A [prefilter](#plugins.prefilters.postfilters) runs through the template
source before it gets compiled. See [template
prefilters](#advanced.features.prefilters) for more information on how
to setup a prefiltering function.

A [postfilter](#plugins.prefilters.postfilters) runs through the
template code after it was compiled to PHP. See [template
postfilters](#advanced.features.postfilters) for more information on how
to setup a postfiltering function.

A [outputfilter](#plugins.outputfilters) operates on a template\'s
output before it is [displayed](#api.display). See [template output
filters](#advanced.features.outputfilters) for more information on how
to set up an output filter function.

See also [`unregisterFilter()`](#api.unregister.filter),
[`loadFilter()`](#api.load.filter),
[`$autoload_filters`](#variable.autoload.filters), [template pre
filters](#advanced.features.prefilters) [template post
filters](#advanced.features.postfilters) [template output
filters](#advanced.features.outputfilters) section.
