nodejs-repl.el
===================================

Run Node.js REPL in Emacs

Description
-----------

This program is derived from comint-mode and provides the following features.

  * token completion, same as Node.js REPL
  * file name completion in string
  * incremental history search


Usage
-----

Put this file in your Emacs lisp path (e.g. ~/.emacs.d/site-lisp)
and add the following line to your .emacs:

    (require 'nodejs-repl)

Type `M-x nodejs-repl` to run Node.js REPL.
See also `comint-mode` to check key bindings.


Author
------

Takeshi Arabiki (abicky)


Copyright and License
---------------------

Copyright (C) 2012-2015  Takeshi Arabiki (abicky)

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
