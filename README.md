# flymake-vale
> Flymake support for Vale

[Vale](https://github.com/ValeLint/vale) is a natural language linter. So with
`flymake-vale` you get on-the-fly natural language linting.

## Quickstart

Install `flymake-vale` from MELPA using `package-install` or something equivalent.

To use `flymake-vale` just `require` it and run `flymake-vale-setup`:

```emacs-lisp
(require 'flymake-vale)
(flymake-vale-setup)
```


## :floppy_disk: Installation

The instruction to use this plugin.

1. Install vale with package manager or download from https://github.com/errata-ai/vale/releases and
   place in your $PATH.
2. Configure vale following its documentation https://docs.errata.ai/vale/config 
3. Consider adding the following snippet to your configuration.

```el
(use-package flymake-vale
  :ensure t
  :hook ((text-mode       . flymake-vale-load)
         (latex-mode      . flymake-vale-load)
         (org-mode        . flymake-vale-load)
         (markdown-mode   . flymake-vale-load)
         (message-mode    . flymake-vale-load)))
```

4. :tada: Done! Now open a text file and hit `M-x flymake-mode`!

otherwise you can call `flymake-vale-maybe-load` like the snippet below.

```el
(add-hook 'find-file-hook 'flymake-vale-maybe-load)
```
