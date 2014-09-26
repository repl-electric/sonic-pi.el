# Sonic Pi for Emacs π=-

A Emacs plugin to enable live coding music in Ruby communicating with SonicPi.

SonicPi (http://sonic-pi.net/) was created by Sam Aaron.

![](http://s3.postimg.org/x7x6am6mb/Screen_Shot_2014_09_26_at_14_11_47.png)

## Install

Prerequisites:

* A checkout of sonic-pi https://github.com/samaaron/sonic-pi
* Ruby 1.9.3+

```
git clone git@github.com:samaaron/sonic-pi.git
cd sonic-pi && app/server/bin/compile-extensions.rb
git clone git@github.com:repl-electric/sonic-pi.el.git ~/.sonic-pi.el`
```

Add to your emacs init.el

```lisp
(add-to-list 'load-path "~/.sonic-pi.el/")
(require 'sonic-pi)
(setq sonic-pi-path "SONIC-PI-INSTALL-DIR")
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
