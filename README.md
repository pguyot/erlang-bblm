Erlang Language Module for BBEdit v1.4, (2018/01/14)
========================

The Erlang Language Module introduces syntax colorization, auto-completion,
function navigation and code folding for the Erlang programming language to
BBEdit 11 and higher.

It recognizes the following Erlang files:
- erlang source code (.erl)
- erlang include files (.hrl)
- escript scripts (.escript)
- application files (.app)
- application upgrade files (.appup).

Please use Erlang Language Module v1.2 for BBEdit 9 and BBEdit 10.

Inline help through auto-completion
-----------------------------------

Erlang Language Module main feature consists in providing inline help through
BBEdit completion feature. When typing calls to functions in OTP modules,
Erlang Language Module displays the actual argument order with the names of the
arguments as well as the return values as described in the documentation.

![alt text](https://raw.githubusercontent.com/pguyot/erlang-bblm/master/completion_example.png)

Download
--------

The binary can be downloaded from GitHub:
https://github.com/pguyot/erlang-bblm/releases/tag/v1.4

Installation
------------

To install the plug-in, simply double-click it or drop it on BBEdit. This will
launch BBEdit (or bring it to the front) which will then ask if you want to
install the plug-in and relaunch BBEdit.

Alternatively, you can put it manually into "~/Library/Application Support/BBEdit/Language Modules/".

Source code and compilation
---------------------------

The plug-in can be compiled with XCode and BBEdit SDK.

Source code is available on GitHub:
http://github.com/pguyot/erlang-bblm

The plug-in is now compiled with the SDK as [published on GitHub](https://github.com/siegel/LanguageModuleSDK), as a submodule.


[![Build Status](https://travis-ci.org/pguyot/erlang-bblm.png)](https://travis-ci.org/pguyot/erlang-bblm)

Current limitations
-------------------

* The module does not detect Erlang content. This would be nice to automatically
detect Erlang term files such as rebar configuration files.

* The module parses functions and attributes properly as long as the code
follows standard Erlang practice of declaring functions and attributes
immediately after a new line, and having the rest of the code somewhat indented.
In other words, the parser is just a quick and dirty implementation that should
usually work as expected. As you might expect, the language module is not robust
to interesting uses of parse_transform/2.

* The module simply ignores pre-processor directives. This means it can be
confused by pre-processor magic. Macro invocations are colorized, even if they
are not properly defined.

* edoc tags are detected and colored, but not their arguments. Actually,
anything starting with @ in a %% comment is colored (triple % works, too). Maybe
at some point, the module will only color valid @ tags.

* There is no completion with parameters of functions in the current module,
especially those with specifications.

* I didn't exploit the folding types yet. Plus folding always happen for the
whole function, not for individual clauses.

Change history
--------------

* 1.0 (2008/08/30)

    - Initial build.

* 1.1 (2009/02/15)

    - [FEA]   Functions are now predefined words and are therefore colorized.
    - [ENH]   @todo in %% comments are reported as ToDo callout items.
    - [FEA]   Macro invocations are now colorized (?\_something).
    - [ENH]   Added all keywords known by erl\_parse:reserved_words/1.
    - [ENH]   Added functions from the math module.
    - [ENH]   Recognize .rel, .app and .appup files.

* 1.2 (2011/07/16)

    - [ENH]   Improved support for .rel, .app and .appup files.
    - [ENH]   Added completion for some missing functions and for R13B new functions.
    - [ENH]   Added keywords such as opaque and export_type.
    - [ENH]   Added built-in type parsing and colorizing.
    - [FIX]   Fixed a bug that would yield to a crash when editing an attribute with no parameter.

* 1.3 (2014/10/27)

    - [ENH]   Upgraded for BBEdit 11.
    - [ENH]   Updated completion lists for Erlang 17
    - [FEA]   Comments can now be spell-checked (BBEdit 11 addition).
    - [FEA]   include and include_lib files can be opened from the popup (BBEdit 11 addition).
    - [FIX]   Fixed algorithmic bugs revelead by Clang analyzer.

* 1.4 (2018/01/14)

    - [ENH]   Upgraded for BBEdit 12.1 (64 bits).
    - [ENH]   Updated completion lists for Erlang OTP 20
    - [FEA]   Added faceless helper application for reference lookups which either opens the man page in Terminal.app (if found) or performs a lookup on erlang.org in the default browser.

License
-------

Copyright (c) 2008-2018 by Paul Guyot, Semiocast.

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
