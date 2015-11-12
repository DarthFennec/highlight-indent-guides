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
;; Version: 0.4
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
    (let ((face (get-text-property (point) 'font-lock-face))
          (invalid nil)
          (newface nil)
          (guides nil))
      (while (and (not invalid) (looking-at "[[:space:]]"))
        (setq newface (get-text-property (point) 'font-lock-face))
        (unless (or (eq newface 'highlight-indent-guides-odd-face)
                    (eq newface 'highlight-indent-guides-even-face))
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
      (unless (looking-at "[[:space:]]*$")
        (setq guides (highlight-indent-guides--get-guides))))
    (if (listp guides) guides nil)))

(defun highlight-indent-guides--guide-line (guides)
  "Draw the indent guides specified by GUIDES on the current line."
  (let ((guides (reverse guides))
        (face nil))
    (while guides
      (add-text-properties
       (point) (1+ (point))
       `(font-lock-face
         ,(if face 'highlight-indent-guides-odd-face
            'highlight-indent-guides-even-face)
         rear-nonsticky t))
      (forward-char)
      (while (and guides (<= (car guides) (current-column)))
        (setq guides (cdr guides))
        (setq face (not face))))
    (remove-text-properties (point) (line-end-position)
                            '(font-lock-face nil rear-nonsticky nil))))

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
          (if (looking-at "[[:space:]]*$")
              (remove-text-properties (point) (line-end-position)
                                      '(font-lock-face nil rear-nonsticky nil))
            (setq guides (highlight-indent-guides--calc-guides
                          guides (current-indentation)))
            (highlight-indent-guides--guide-line guides))
          (setq eof (< 0 (forward-line))))
        (while (and (not eof) (not (eq newguides t))
                    (not (equal guides newguides)))
          (unless (looking-at "[[:space:]]*$")
            (setq guides (highlight-indent-guides--calc-guides
                          guides (current-indentation)))
            (setq newguides (highlight-indent-guides--get-guides))
            (unless (equal guides newguides)
              (highlight-indent-guides--guide-line guides)))
          (setq eof (< 0 (forward-line))))))))

(defun highlight-indent-guides--unguide-region (start end)
  "Remove all indent guides in the buffer region from START to END."
  (with-silent-modifications
    (remove-text-properties
     start end '(font-lock-face nil rear-nonsticky nil))))

;;;###autoload
(define-minor-mode highlight-indent-guides-mode
  "Display indent guides in a buffer."
  nil nil nil
  (if highlight-indent-guides-mode
      (jit-lock-register 'highlight-indent-guides--guide-region)
    (jit-lock-unregister 'highlight-indent-guides--guide-region)
    (highlight-indent-guides--unguide-region (point-min) (point-max))))

(provide 'highlight-indent-guides)

;;; highlight-indent-guides.el ends here
