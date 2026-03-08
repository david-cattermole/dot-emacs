# EMACS Configuration

Emacs configuration files for my personal preferences.

NOTE: The files in the directory ".emacs.d/lisp" are not mine (unless
stated), I have made no changes but are only stored for convenience.

## Building / Byte-compiling Elisp

Byte-compile all Emacs Lisp packages in `.emacs.d/lisp/`:

```bash
./build.sh
```

Requires `emacs` on `PATH`. Override with `EMACS=/path/to/emacs ./build.sh`.
See `build.el` for details.
