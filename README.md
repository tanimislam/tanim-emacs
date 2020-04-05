# tanim-emacs: my own customizations

Hello World! Here I have a grab-bag of `elisp` files I need to get my [emacs](https://www.gnu.org/software/emacs/) environment to look as nice as I want it to.

This repository should live in a `~/.xemacs` directory. The easiest way to do this is to run the command,
```bash
git clone https://tanim_islam@bitbucket.org/tanim_islam/tanim-emacs.git .xemacs
```
All in your home directory.

Second, you should have a `~/.emacs` file, which should look like this.
```elisp
;;; XEmacs backwards compatibility file
(setq user-init-file
      (expand-file-name "init.el"
			(expand-file-name ".xemacs" "~")))
(setq custom-file
      (expand-file-name "custom.el"
			(expand-file-name ".xemacs" "~")))

(load-file user-init-file)
(load-file custom-file)
```
Have fun and edit on!
