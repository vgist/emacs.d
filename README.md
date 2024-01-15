A reasonable Emacs config

## Installation

```
git clone https://github.com/vgist/dot-emacs.git ~/.config/emacs
```

Upon starting up Emacs for the first time, further third-party
packages will be automatically downloaded and installed. If you
encounter any errors at that stage, try restarting Emacs, and possibly
running `M-x package-refresh-contents` before doing so.


## Updates

Update the config with `git pull`. You'll probably also want/need to update
the third-party packages regularly too:

<kbd>M-x package-list-packages</kbd>, then <kbd>U</kbd> followed by <kbd>x</kbd>.

You should usually restart Emacs after pulling changes or updating
packages so that they can take effect.

## tree-sitter warning

> Warning (treesit): Cannot activate tree-sitter, because language grammar for rust is unavailable (not-found)

    M-x treesit-install-language-grammar RET rust

more: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter

## Thanks

<https://github.com/purcell/emacs.d>
