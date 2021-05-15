# My Emacs Config

This is a WIP emacs config inspired by spacemacs. 

I really liked the ergonomics of spacemacs but I had a hard time tweaking it because I didn't know how anything worked. I decided to learn emacs from scratch and build up my own config to mimic some of the functionality I liked about spacemacs. I'm pretty happy with it so far, and I'd recommend others to try something similar if they feel that emacs starter kits are too overwhelming.

I don't really recommend anyone use this as-is, because it is highly specific to my tastes. The best part about emacs is how configurable it is, so you should make your emacs exactly how **you** want it to be. If you want to create something similar, check out the sections below.

## Learning Resources

The following resources were a big help getting started:

* [Mastering Emacs](https://www.masteringemacs.org/) book
* [System Crafters](https://www.youtube.com/c/SystemCrafters/videos) youtube channel

I recommend reading "Mastering Emacs" from start to finish before you tweak things too much and add too many extensions. Once you have a good feel for the basics, and most importantly the "discoverability" aspect emphasized in the book, then you can move on to installing lots of extensions.

## Favorite Packages

At this time my config is heavily based on the following packages:

* [use-package](https://github.com/jwiegley/use-package)
  * package management
* [which-key](https://github.com/justbur/emacs-which-key)
  * displays available keybindings in popup (useful for discovery and also for not having to remember as much)
* [evil](https://github.com/emacs-evil/evil)
  * vim keybindings
* [ivy + counsel](https://github.com/abo-abo/swiper)
  * minibuffer completion
* [general.el](https://github.com/noctuid/general.el)
  * binding keys and creating command maps behind a prefix key (similar to spacemac's SPC key prefix system)

Of course I use other packages like org, magit, lsp, etc. but the above represent the core of my emacs configuration.
