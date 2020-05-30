;;; highlight-indent-guides-tests.el --- test highlight-indent-guides mode  -*- lexical-binding: t; -*-

;;; Code:

(defvar higlight-indent-guides-debug-p nil
    "When t, switch into test-buffer.")
;; (setq higlight-indent-guides-debug-p t)

(defcustom higlight-indent-guides-debug-p nil
  "When t, switch into test-buffer.

Default is nil"
  :type 'boolean
  :group 'highlight-indent-guides)

(defun higlight-indent-guides-toggle-switch-p ()
  "Toggle `higlight-indent-guides-debug-p'. "
  (interactive)
  (setq higlight-indent-guides-debug-p (not higlight-indent-guides-debug-p))
  (message "higlight-indent-guides-debug-p: %s"  higlight-indent-guides-debug-p))

(defmacro higlight-indent-guides-test (contents mode debug &rest body)
  "Create temp buffer inserting CONTENTS.

MODE the emacs major mode
DEBUG if test when running should by shown - maybe running under edebug
BODY is code to be executed within the temp buffer "
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     ;; (load "../highlight-indent-guides.el" nil t)
     (let (hs-minor-mode)
       (insert ,contents)
       (funcall ,mode)
       (highlight-indent-guides-mode)
       (when ,debug
	 (switch-to-buffer (current-buffer))
	 (save-excursion (font-lock-fontify-region (point-min) (point-max)))
	 ,@body))))

(ert-deftest higlight-indent-guides-python-test-diuB24 ()
  (higlight-indent-guides-test
      "def main():
    if len(sys.argv) == 1:
        usage()"
    'python-mode
    higlight-indent-guides-debug-p
    (beginning-of-line)
    (should (listp (get-text-property (point) 'highlight-indent-guides-prop)))))

(provide 'highlight-indent-guides-tests)
;;; highlight-indent-guides-tests.el ends here
