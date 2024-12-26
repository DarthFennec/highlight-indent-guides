(package "highlight-indent-guides"
         "0.9.2"
         "Minor mode to highlight indentation")

(website-url "https://github.com/DarthFennec/highlight-indent-guides")
(keywords "convenience")

(package-file "highlight-indent-guides.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)

(depends-on "emacs" "26.1")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(add-hook 'eask-before-compile-hook
           (lambda ()
             (setq byte-compile-error-on-warn t)))
