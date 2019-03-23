(add-hook 'go-mode-hook 'eglot-ensure)

(add-to-list 'eglot-server-programs '(go-mode . ("/Users/paulalvarez/code/go/bin/bingo")))
