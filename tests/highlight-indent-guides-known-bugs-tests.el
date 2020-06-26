;;; highlight-indent-guides-known-bugs-tests.el --- tests against known bugs in highlight-indent-guides mode  -*- lexical-binding: t; -*-

;;; Succeeding tests here means bug still exists

;; When failing and bug is fixed, move them into highlight-indent-guides-tests.el

;;; Code:

(require 'highlight-indent-guides)
(require 'highlight-indent-guides-tests)

(ert-deftest higlight-indent-guides-python-bitmap-test-diuB24 ()
  (higlight-indent-guides-method-test
      "def main():
    if len(sys.argv) == 1:
        usage()"
    'python-mode
    higlight-indent-guides-debug-p
    (let ((highlight-indent-guides-method 'bitmap))
      (highlight-indent-guides-mode)
      (beginning-of-line)
      (should (listp (get-text-property (point) 'highlight-indent-guides-prop)))
      ;; change that to "should" when bug is fixed
      (should-not (eq  (get-char-property (point) 'face) 'highlight-indent-guides-character-face))
      )))

(provide 'highlight-indent-guides-known-bugs-tests)
;;; highlight-indent-guides-known-bugs-tests.el ends here
