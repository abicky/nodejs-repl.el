nodejs-repl.el
===================================

![](https://github.com/abicky/nodejs-repl.el/workflows/CI/badge.svg)

Run Node.js REPL in Emacs

Description
-----------

This program is derived from comint-mode and provides the following features.

* Token completion, same as Node.js REPL
* File name completion in string
* Incremental history search
* Sending JavaScript code to REPL

Usage
-----

Put this file in your Emacs Lisp path (e.g. ~/.emacs.d/site-lisp)
and add the following line to your .emacs:

```elisp
(require 'nodejs-repl)
```

Type `M-x nodejs-repl` to run Node.js REPL.
See also `comint-mode` to check key bindings.

You can define key bindings to send JavaScript code to REPL as follows:

```elisp
(add-hook 'js-mode-hook #'nodejs-repl-minor-mode)
```

When a version manager such as nvm is used to run different versions
of Node.js, it is often desirable to start the REPL of the version
specified in the .nvmrc file per project.  In such case, customize the
`nodejs-repl-command` variable with a function symbol.  That function
should query nvm for the Node.js command to run.  For example:

```elisp
(require 'nodejs-repl)
(defun nvm-which ()
  (let* ((shell (concat (getenv "SHELL") " -l -c 'nvm which'"))
         (output (shell-command-to-string shell)))
    (cadr (split-string output "[\n]+" t))))
(setq nodejs-repl-command #'nvm-which)
```

The `nvm-which` function can be simpler, and perhaps can run faster,
too, if using Bash:

```elisp
(defun nvm-which ()
  (let ((output (shell-command-to-string "source ~/.nvm/nvm.sh; nvm which")))
    (cadr (split-string output "[\n]+" t))))
```

Author
------

Takeshi Arabiki (abicky)


Copyright and License
---------------------

Copyright (C) 2012-2024  Takeshi Arabiki (abicky)

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
