rebar3_coverme
=====

A rebar plugin for generating a coverage report for a vim-coverme plugin (link will be added lately)

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_coverme, ".*", {git, "git://github.com/evilbluebeaver/rebar3_coverme.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 coverme
    ===> Fetching rebar3_coverme
    ===> Compiling rebar3_coverme
    <Plugin Output>

Configuration
-------------
There is a 'verbose' option available
    $ rebar3 coverme --verbose

It can also be set in rebar.config
    {coverme_opts, [verbose]}
