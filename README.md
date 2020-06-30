rebar3_cobertura
=====

A rebar plugin for generating a cobertura coverage report

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_cobertura, ".*", {git, "git://github.com/evilbluebeaver/rebar3_cobertura.git", {tag, "0.2.0"}}}
    ]}.

Then just call a plugin directly:


    $ rebar3 cobertura
    ===> Fetching rebar3_cobertura
    ===> Compiling rebar3_cobertura
    <Plugin Output>

Configuration
-------------
There is a 'verbose' option available

    $ rebar3 rebar3_cobertura --verbose

It can also be set in rebar.config

    {cobertura_opts, [verbose]}

