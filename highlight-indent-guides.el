;;; highlight-indent-guides.el --- Minor mode to highlight indentation
;;
;; Copyright (c) 2015 DarthFennec
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;; Author: DarthFennec <darthfennec@derpymail.org>
;; Version: 0.5
;; URL: https://github.com/DarthFennec/highlight-indent-guides

;;; Commentary:
;; This minor mode highlights indentation levels using a pair of alternating
;; faces. Indent widths are dynamically discovered, which means this correctly
;; highlights in any mode, regardless of indent width, even in languages with
;; non-uniform indentation such as Haskell. This mode works properly around hard
;; tabs and mixed indentation, and it behaves well in large buffers.
;;
;; To install, put this file in your load-path, and require it:
;;
;;   (require 'highlight-indent-guides)
;;
;; Then, do M-x highlight-indent-guides-mode to enable it. To enable it
;; automatically in most programming modes, use the following:
;;
;;   (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;;
;; To change the colors used for highlighting, use:
;;
;;   (set-face-background 'highlight-indent-guides-odd-face "color")
;;   (set-face-background 'highlight-indent-guides-even-face "color")

;;; Code:

(defgroup highlight-indent-guides nil
  "Indentation highlighting."
  :group 'basic-faces)

(defface highlight-indent-guides-odd-face
  '((t (:background "#303030")))
  "Face to highlight odd indent levels."
  :group 'highlight-indent-guides)

(defface highlight-indent-guides-even-face
  '((t (:background "#3A3A3A")))
  "Face to highlight even indent levels."
  :group 'highlight-indent-guides)

(defun highlight-indent-guides--calc-guides (prev-guides indent)
  "Calculate the indent guides for a line.
PREV-GUIDES are the previous line's indent guides,
and INDENT is this line's indent width."
  (let ((guides prev-guides))
    (while (and guides (< indent (car guides)))
      (setq guides (cdr guides)))
    (when (and (< 0 indent) (or (null guides) (> indent (car guides))))
      (setq guides (cons indent guides)))
    guides))

(defun highlight-indent-guides--get-guides ()
  "Extract the indent guides from a line, by reading the faces."
  (save-excursion
    (let ((face (get-text-property (point) 'highlight-indent-guides-prop))
          (invalid nil)
          (newface nil)
          (guides nil))
      (while (and (not invalid) (looking-at "[[:space:]]"))
        (setq newface (get-text-property (point) 'highlight-indent-guides-prop))
        (unless (or (eq newface 'odd) (eq newface 'even))
          (setq invalid t))
        (unless (equal face newface)
          (setq guides (cons (current-column) guides))
          (setq face newface))
        (forward-char))
      (let ((col (current-column)))
        (when (< 0 col) (setq guides (cons col guides))))
      (or invalid guides))))

(defun highlight-indent-guides--get-prev-guides ()
  "Scan up the buffer to find a starting point to calculate guides from."
  (let ((guides t))
    (while (and (nlistp guides) (< 1 (line-number-at-pos)))
      (forward-line -1)
      (unless (or (let ((s (syntax-ppss))) (or (nth 3 s) (nth 4 s)))
                  (looking-at "[[:space:]]*$"))
        (setq guides (highlight-indent-guides--get-guides))))
    (if (listp guides) guides nil)))

(defun highlight-indent-guides--guide-line (guides)
  "Draw the indent guides specified by GUIDES on the current line."
  (let ((guides (reverse guides))
        (face nil))
    (while guides
      (add-text-properties
       (point) (1+ (point))
       `(highlight-indent-guides-prop ,(if face 'odd 'even)))
      (forward-char)
      (while (and guides (<= (car guides) (current-column)))
        (setq guides (cdr guides))
        (setq face (not face))))
    (remove-text-properties (point) (line-end-position)
                            '(highlight-indent-guides-prop nil))))

(defun highlight-indent-guides--guide-region (start end)
  "Add or update indent guides in the buffer region from START to END."
  (with-silent-modifications
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (let ((eof nil)
            (guides (highlight-indent-guides--get-prev-guides))
            (newguides nil))
        (while (and (not eof) (< (point) end))
          (if (or (let ((s (syntax-ppss))) (or (nth 3 s) (nth 4 s)))
                  (looking-at "[[:space:]]*$"))
              (remove-text-properties (point) (line-end-position)
                                      '(highlight-indent-guides-prop nil))
            (setq guides (highlight-indent-guides--calc-guides
                          guides (current-indentation)))
            (highlight-indent-guides--guide-line guides))
          (setq eof (< 0 (forward-line))))
        (while (and (not eof) (not (eq newguides t))
                    (not (equal guides newguides)))
          (unless (or (let ((s (syntax-ppss))) (or (nth 3 s) (nth 4 s)))
                      (looking-at "[[:space:]]*$"))
            (setq guides (highlight-indent-guides--calc-guides
                          guides (current-indentation)))
            (setq newguides (highlight-indent-guides--get-guides))
            (unless (equal guides newguides)
              (highlight-indent-guides--guide-line guides)))
          (setq eof (< 0 (forward-line))))
        (font-lock-fontify-region start (point))))))

(defun highlight-indent-guides--unguide-region (start end)
  "Remove all indent guides in the buffer region from START to END."
  (with-silent-modifications
    (remove-text-properties start end '(highlight-indent-guides-prop nil))))

(defun highlight-indent-guides--keyword-matcher (limit)
  "A font-lock-keywords matcher, which searches for indent guides between the
point and LIMIT, so that they can be properly highlighted."
  (let ((match (point))
        (prop 'highlight-indent-guides-prop)
        odd even end oddm evenm endm)
    (while (and (< match limit) (null even))
      (if (not (eq 'even (get-text-property match prop)))
          (setq match (next-single-property-change match prop nil limit))
        (setq even match)
        (setq match (next-single-property-change match prop nil limit))
        (setq odd match)
        (when (eq 'odd (get-text-property match prop))
          (setq match (next-single-property-change match prop nil limit)))
        (setq end match)
        (setq evenm (copy-marker even))
        (setq oddm (copy-marker odd))
        (setq endm (copy-marker end))
        (set-match-data (list evenm endm evenm oddm oddm endm))
        (goto-char end)))
    end))

;;;###autoload
(define-minor-mode highlight-indent-guides-mode
  "Display indent guides in a buffer."
  nil nil nil
  (let ((keywords
         '((highlight-indent-guides--keyword-matcher
           (1 'highlight-indent-guides-even-face t)
           (2 'highlight-indent-guides-odd-face t)))))
    (if highlight-indent-guides-mode
        (progn
          (font-lock-add-keywords nil keywords)
          (jit-lock-register 'highlight-indent-guides--guide-region))
      (font-lock-remove-keywords nil keywords)
      (jit-lock-unregister 'highlight-indent-guides--guide-region)
      (highlight-indent-guides--unguide-region (point-min) (point-max)))))

(provide 'highlight-indent-guides)

;;; highlight-indent-guides.el ends here
