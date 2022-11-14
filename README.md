# flymake-vale
> Flymake support for Vale

[Vale](https://github.com/ValeLint/vale) is a natural language linter.
So with `flymake-vale` you get on-the-fly natural language linting.

## Installation

The instruction to use this plugin.

1. Install Vale with package manager or download from
   [Releases](https://github.com/errata-ai/vale/releases) and place in
   your $PATH.

2. Configure Vale following its documentation [Vale
   Config](https://docs.errata.ai/vale/config)

3. Install `flymake-vale`. Until it is submitted to Elpa and/or Melpa,
   you can install it with
   [Straight.el](https://github.com/radian-software/straight.el):

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

## Enabling Backend


You can add `flymake-vale` as a backend to specific modes by adding
`flymake-vale-load` to the relevant hook:

```el
(add-hook 'text-mode-hook #'flymake-vale-load)
(add-hook 'latex-mode-hook #'flymake-vale-load)
(add-hook 'org-mode-hook #'flymake-vale-load)
(add-hook 'markdown-mode-hook #'flymake-vale-load)
(add-hook 'message-mode-hook #'flymake-vale-load)
```

You can also have `flymake-vale` load automatically upon opening a
file when that file's major mode is in `flymake-vale-modes`:

```el
(add-hook 'find-file-hook 'flymake-vale-maybe-load)
;; flymake-vale-modes defaults to: 
;;  => (text-mode latex-mode org-mode markdown-mode message-mode)

(add-to-list 'flymake-vale-modes 'adoc-mode)
```

Now open a text file and enable flymake (`M-x flymake-mode`).


## File Extensions

Vale has support for text markup formats ([including org-mode
support](https://github.com/errata-ai/vale/releases/tag/v2.20.0)), but
it needs to know the file extension so it can parse the content of the
file, while ignoring the parts we don't want to check. The current
logic is to use in order of preference:

1. The buffer-local variable `flymake-vale-file-ext` if it is non nil.
   This can be used to override and send your preferred extension.
```el
(add-hook 'org-msg-mode-hook (lambda ()
                               (setq flymake-vale-file-ext "org")
                               (flymake-vale-load)))
```

2. If the buffer is visiting a file, use that file's extension.

3. If the buffer's `major-mode` is in the
   `flymake-vale-mode-file-exts` alist then use its corresponding
   extension. This currently includes all built-in scoping formats and
   can be customized to include new modes like this:
```el
(add-to-list 'flymake-vale-mode-file-exts '(rust-mode . "rs"))
```


<!-- Local Variables: -->
<!-- fill-column: 70 -->
<!-- End: -->
