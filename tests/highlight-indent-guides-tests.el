;;; highlight-indent-guides-tests.el --- regression tests for highlight-indent-guides mode  -*- lexical-binding: t; -*-

;;; Comment

;; When adding tests, suggest to test against modes shipped with Emacs
;; only. Loading stuff from Melpa for instance might contribute to
;; complexity of test-setup.

;;; Code:

(require 'highlight-indent-guides)
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

(defmacro higlight-indent-guides-method-test (contents mode debug &rest body)
  "Create temp buffer inserting CONTENTS.

MODE the emacs major mode
DEBUG if test when running should by shown - maybe running under edebug
BODY is code to be executed within the temp buffer 

higlight-indent-guides mode must be specified from test, which allows setting vars different from defcustom.
"
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     ;; (load "../highlight-indent-guides.el" nil t)
     (let (hs-minor-mode)
       (insert ,contents)
       (funcall ,mode)
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

(ert-deftest higlight-indent-guides-python-fill-test-diuB24 ()
  (higlight-indent-guides-test
      "def main():
    if len(sys.argv) == 1:
        usage()"
    'python-mode
    higlight-indent-guides-debug-p
    (let ((highlight-indent-guides-method 'fill))
      (beginning-of-line)
      (should (listp (get-text-property (point) 'highlight-indent-guides-prop))))))

(ert-deftest higlight-indent-guides-python-character-test-diuB24 ()
  (higlight-indent-guides-method-test
      "def main():
    if len(sys.argv) == 1:
        usage()"
    'python-mode
    higlight-indent-guides-debug-p
    (let ((highlight-indent-guides-method 'character))
      (highlight-indent-guides-mode)
      (beginning-of-line)
      (should (listp (get-text-property (point) 'highlight-indent-guides-prop)))
      (should (eq  (get-char-property (point) 'face) 'highlight-indent-guides-character-face))
      )))

(ert-deftest higlight-indent-guides-77-comment-face-test-kGiOk1 ()
  (higlight-indent-guides-test
      "(defun foo1 (\&optional beg end)
  \"asdf \"
  (interactive \"*\")
  )"
    'emacs-lisp-mode
    higlight-indent-guides-debug-p
    (goto-char (point-max)) 
    (search-backward "int")
    (beginning-of-line)
    (should (listp (get-text-property (point) 'highlight-indent-guides-prop)))))

(ert-deftest higlight-indent-guides-77-comment-face-test-lhKKTF ()
  (higlight-indent-guides-test
      "(defun foo1 (\&optional beg end)
  \"asdf \"
  (interactive \"*\")
  )"
    'emacs-lisp-mode
    higlight-indent-guides-debug-p
    (goto-char (point-max)) 
    (search-backward "int")
    (beginning-of-line)
    (insert ";;")
    (should-not (get-text-property (point) 'highlight-indent-guides-prop))))

(provide 'highlight-indent-guides-tests)
;;; highlight-indent-guides-tests.el ends here
