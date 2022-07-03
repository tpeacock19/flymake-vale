# flymake-vale
> Flymake support for Vale

[Vale](https://github.com/ValeLint/vale) is a natural language linter. So with `flymake-vale` you get on-the-fly natural language linting.

## Installation

The instruction to use this plugin.

1. Install vale with package manager or download from https://github.com/errata-ai/vale/releases and place in your $PATH.

2. Configure vale following its documentation https://docs.errata.ai/vale/config 

3. Install `flymake-vale`. Until it is submitted to Elpa and/or Melpa, you can install it with [Straight.el](https://github.com/radian-software/straight.el):

```emacs-lisp
(straight-use-package
 '(flymake-vale :type git :host github :repo "tpeacock19/flymake-vale"))
```

or cloning this repository: 

```bash
cd path/to/custom/lisp
git clone https://github.com/tpeacock19/flymake-vale
(add-to-list 'load-path "path/to/custom/lisp/flymake-vale")
```

4. Consider adding the following snippet to your configuration.

```el
(use-package flymake-vale
  :ensure t
  :hook ((text-mode       . flymake-vale-load)
         (latex-mode      . flymake-vale-load)
         (org-mode        . flymake-vale-load)
         (markdown-mode   . flymake-vale-load)
         (message-mode    . flymake-vale-load)))
```

5. Now open a text file and hit `M-x flymake-mode`.

otherwise you can call `flymake-vale-maybe-load` like the snippet below.

```el
(add-hook 'find-file-hook 'flymake-vale-maybe-load)
```

## File Extensions

Vale has support for text markup formats ([including org-mode
support](https://github.com/errata-ai/vale/issues/330) soon!), but it
needs to know the file extension so it can parse the content of the
file, while ignoring the parts we don't want to check. By default, if
the buffer is a file buffer, `flymake-vale` will use the file's
extension.

You can set the extension manually with the `flymake-vale-file-ext`
buffer local variable, and of particular note: you can combine this
with a hook to provide flymake-vale support for new buffers.

```el
(add-hook 'org-mode-hook '(lambda ()
  (setq flymake-vale-file-ext ".org")
  (flymake-vale-load)))

(add-hook 'org-msg-mode-hook '(lambda ()
  (setq flymake-vale-file-ext ".org")
  (flymake-vale-load)))

(add-hook 'markdown-mode-hook '(lambda ()
  (setq flymake-vale-file-ext ".md")
  (flymake-vale-load)))

(add-hook 'html-mode-hook '(lambda ()
  (setq flymake-vale-file-ext ".html")
  (flymake-vale-load)))
```
