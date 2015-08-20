zop-to-char
===========

You can [![Donate](https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=thierry.volpiatto@gmail.com&lc=US&currency_code=EUR&bn=PP-DonationsBF:btn_donateCC_LG.gif:NonHosted) to help this project.

## Description
A visual zap-to-char command for Emacs.

## Install
If you install from emacs packaging system, you probably have nothing to do
apart binding zop-to-char to a key (See below).

If you install from git, you will have to add this to your init file:

    (autoload 'zop-to-char "zop-to-char.el" nil t)
    (autoload 'zop-up-to-char "zop-to-char.el" nil t)

## Bind zop-to-char to a key
You will probably want to use zop-to-char instead of zap-to-char:

    (global-set-key [remap zap-to-char] 'zop-to-char)

## Usage
You will have all needed infos in minibuffer or in mode-line if minibuffer is in use.

## Features

- Works in minibuffer
- You can change direction with `C-b` and `C-f`.
  When starting at end of buffer zop-to-char search automatically backward.
- You can use zop-to-char to move to a place (use C-q).
- Hit repetitively the character you are searching will move to next.
- You can copy or kill region from point to last search point.
- C-g will quit and bring you back to initial position.

## Other packages

zop-to-char fits well in minibuffer with [eldoc-eval](https://github.com/thierryvolpiatto/eldoc-eval)
