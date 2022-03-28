# Sonic Pi for Emacs π=-

A Emacs plugin to enable live coding music in Ruby communicating with SonicPi.

SonicPi (http://sonic-pi.net/) was created by Sam Aaron.

![](http://s3.postimg.org/x7x6am6mb/Screen_Shot_2014_09_26_at_14_11_47.png)

## Install

Prerequisites:

* A checkout of sonic-pi https://github.com/samaaron/sonic-pi
* The `dash` package
* The `osc` package
* Highlight from EmacsWiki https://www.emacswiki.org/emacs/highlight.el
* SuperCollider
* Ruby 1.9.3+

```shell
git clone git@github.com:samaaron/sonic-pi.git
cd sonic-pi && app/server/ruby/bin/compile-extensions.rb
git clone git@github.com:repl-electric/sonic-pi.el.git ~/.sonic-pi.el
```

Install the required packages

```
M-x package-install dash
M-x package-install osc
```

Add to your emacs init.el

```lisp
(add-to-list 'load-path "~/.sonic-pi.el/")
(require 'sonic-pi)
(setq sonic-pi-path "SONIC-PI-INSTALL-DIR/") ; Must end with "/"

;; Optionally define a hook
(add-hook 'sonic-pi-mode-hook
          (lambda ()
            ;; This setq can go here instead if you wish
            (setq sonic-pi-path "SONIC-PI-INSTALL-DIR/")
            (define-key ruby-mode-map "\C-c\C-b" 'sonic-pi-stop-all)))
```

Start emacs `emacs my-first-sonic-pi.rb`

``` 
M-x sonic-pi-mode

M-x sonic-pi-jack-in

;;OR if you are already running sonic-pi-server

M-x sonic-pi-connect

;;Send buffer to sonic-pi

C-c C-k 
```
