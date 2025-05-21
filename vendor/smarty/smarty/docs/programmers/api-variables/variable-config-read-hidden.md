\$config\_read\_hidden {#variable.config.read.hidden}
======================

If set to TRUE, hidden sections ie section names beginning with a
period(.) in [config files](#config.files) can be read from templates.
Typically you would leave this FALSE, that way you can store sensitive
data in the config files such as database parameters and not worry about
the template loading them. FALSE by default.
