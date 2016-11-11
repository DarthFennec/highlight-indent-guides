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
;; Version: 0.6.3
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/DarthFennec/highlight-indent-guides

;;; Commentary:
;; This minor mode highlights indentation levels via font-lock. Indent widths
;; are dynamically discovered, which means this correctly highlights in any
;; mode, regardless of indent width, even in languages with non-uniform
;; indentation such as Haskell. This mode works properly around hard tabs and
;; mixed indentation, and it behaves well in large buffers.
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
;; To set the display method, use:
;;
;;   (setq highlight-indent-guides-method METHOD)
;;
;; Where METHOD is either 'fill, 'column, or 'character.
;;
;; To change the colors used for highlighting, use:
;;
;;   (set-face-background 'highlight-indent-guides-odd-face "color")
;;   (set-face-background 'highlight-indent-guides-even-face "color")
;;
;; To change the character and face used for drawing guide lines, use:
;;
;;   (setq highlight-indent-guides-character ?ch)
;;   (set-face-foreground 'highlight-indent-guides-character-face "color")

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

(defface highlight-indent-guides-character-face
  '((t (:foreground "#3A3A3A")))
  "Face to highlight guide line characters."
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-character ?\x2502
  "Character to use to display guide lines."
  :type 'character
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-method 'fill
  "Method to use when displaying indent guides.
This can be `fill', `column', or `character'."
  :type '(choice (const fill) (const column) (const character))
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
  "Extract the indent guides from a line, by reading the text properties."
  (save-excursion
    (let (prop face seg nface nseg invalid guides)
      (while (and (not invalid) (looking-at "[[:space:]]"))
        (setq prop (get-text-property (point) 'highlight-indent-guides-prop))
        (setq nface (car prop) nseg (nth 1 prop))
        (unless (or (eq nface 'odd) (eq nface 'even)) (setq invalid t))
        (unless (or invalid (and (equal face nface) (equal seg nseg)))
          (when (and face (not (equal face nface)))
            (setq guides (cons (current-column) guides)))
          (dolist (segment nseg)
            (setq guides (cons (+ segment (current-column)) guides))
            (setq nface (pcase nface (`odd 'even) (`even 'odd))))
          (setq face nface seg nseg))
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
  (unless (equal guides (highlight-indent-guides--get-guides))
    (let ((guides (reverse guides))
          (column (current-column))
          (currpt (point))
          (starter t)
          currcol currface nextface props oldprop newprop face)
      (while guides
        (setq props nil)
        (setq currcol column)
        (setq currface (if face 'odd 'even))
        (setq nextface (if face 'even 'odd))
        (setq currpt (point))
        (forward-char)
        (setq column (current-column))
        (while (and guides (< (car guides) column))
          (setq props (cons (- (car guides) currcol) props))
          (setq guides (cdr guides))
          (setq face (not face)))
        (setq props (reverse props))
        (when (and props (zerop (car props)))
          (setq props (cdr props))
          (setq currface nextface)
          (setq starter t))
        (setq oldprop (get-text-property currpt 'highlight-indent-guides-prop))
        (setq newprop (list currface props starter (- column currcol) nil))
        (when (and oldprop
                   (equal (seq-subseq newprop 0 4) (seq-subseq oldprop 0 4)))
          (setcar (cddr (cddr newprop)) (nth 4 oldprop)))
        (add-text-properties
         currpt (1+ currpt) `(highlight-indent-guides-prop ,newprop))
        (setq starter nil))
      (remove-text-properties
       currpt (line-end-position) '(highlight-indent-guides-prop nil)))))

(defun highlight-indent-guides--guide-region (start end)
  "Add or update indent guides in the buffer region from START to END."
  (with-silent-modifications
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (let ((guides (highlight-indent-guides--get-prev-guides))
            newguides eof)
        (while (and (not eof) (< (point) end))
          (if (or (let ((s (syntax-ppss))) (or (nth 3 s) (nth 4 s)))
                  (looking-at "[[:space:]]*$"))
              (remove-text-properties
               (point) (line-end-position) '(highlight-indent-guides-prop nil))
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

(defun highlight-indent-guides--fill-keyword-matcher (limit)
  "Search for indent guides between the point and LIMIT.
Find the next character that is part of any indentation. This is meant to be
used as a `font-lock-keywords' matcher."
  (let* ((pos (point))
         (prop 'highlight-indent-guides-prop)
         (face (car (get-text-property pos prop))))
    (while (and (not (eq face 'odd)) (not (eq face 'even)) (< pos limit))
      (setq pos (next-single-property-change pos prop nil limit))
      (setq face (car (get-text-property pos prop))))
    (when (< pos limit)
      (set-match-data (list (copy-marker pos) (copy-marker (1+ pos))))
      (goto-char (1+ pos)))))

(defun highlight-indent-guides--column-keyword-matcher (limit)
  "Search for indent guides between the point and LIMIT.
Find the next character that contains the first column of an indentation level.
This is meant to be used as a `font-lock-keywords' matcher."
  (let* ((pos (point))
         (prop 'highlight-indent-guides-prop)
         (propval (get-text-property pos prop)))
    (while (and (not (and (or (eq (car propval) 'odd) (eq (car propval) 'even))
                          (or (nth 2 propval) (nth 1 propval)))) (< pos limit))
      (setq pos (1+ pos))
      (setq propval (get-text-property pos prop))
      (while (and (< pos limit) (not (eq (car propval) 'odd))
                  (not (eq (car propval) 'even)))
        (setq pos (next-single-property-change pos prop nil limit))
        (setq propval (get-text-property pos prop))))
    (when (< pos limit)
      (set-match-data (list (copy-marker pos) (copy-marker (1+ pos))))
      (goto-char (1+ pos)))))

(defmacro highlight-indent-guides--cache-highlight (type prop &rest body)
  "Memoize the highlighter results in the character's properties.
If a cached result with the right TYPE (`fill', `column', or `character') is
contained in PROP, return that result instead of calculating a new one.
Otherwise, calculate a new result by running BODY, cache it in PROP, and return
it."
  `(if (eq ,type (car (nth 4 ,prop))) (cdr (nth 4 ,prop))
     (let ((result (progn ,@body)))
       (setcar (cddr (cddr ,prop)) (cons ,type result))
       result)))

(defun highlight-indent-guides--fill-highlighter ()
  "Apply highlighting to the indentation.
Return highlighting information for the character at START. Highlights all
indentation characters in alternating colors. This is meant to be used as a
`font-lock-keywords' face definition."
  (let ((prop (get-text-property start 'highlight-indent-guides-prop)))
    (highlight-indent-guides--cache-highlight
     'fill prop
     (let* ((oddface 'highlight-indent-guides-odd-face)
            (evenface 'highlight-indent-guides-even-face)
            (faceval (car prop)) (segs (nth 1 prop)) (cwidth (nth 3 prop))
            (face (pcase faceval (`even evenface) (`odd oddface)))
            (opface (pcase faceval (`even oddface) (`odd evenface)))
            segstart segend showstr)
       (if (null segs) face
         (setq showstr (make-string cwidth ?\s))
         (while segs
           (setq segstart (pop segs))
           (setq segend (if segs (pop segs) cwidth))
           (add-text-properties segstart segend `(face ,opface) showstr))
         `(face ,face display ,showstr))))))

(defun highlight-indent-guides--column-highlighter ()
  "Apply highlighting to the indentation.
Return highlighting information for the character at START. Highlights the first
column of each indentation level in alternating colors. This is meant to be used
as a `font-lock-keywords' face definition."
  (let ((prop (get-text-property start 'highlight-indent-guides-prop)))
    (highlight-indent-guides--cache-highlight
     'column prop
     (let* ((oddface 'highlight-indent-guides-odd-face)
            (evenface 'highlight-indent-guides-even-face)
            (faceval (car prop)) (segs (nth 1 prop))
            (starter (nth 2 prop)) (cwidth (nth 3 prop))
            (face (pcase faceval (`even evenface) (`odd oddface)))
            (opface (pcase faceval (`even oddface) (`odd evenface)))
            showstr altface)
       (if (and (null segs) (eq cwidth 1)) face
         (setq showstr (make-string cwidth ?\s))
         (when starter (add-text-properties 0 1 `(face ,face) showstr))
         (dolist (seg segs)
           (if altface (add-text-properties seg (1+ seg) `(face ,face) showstr)
             (add-text-properties seg (1+ seg) `(face ,opface) showstr))
           (setq altface (not altface)))
         `(face nil display ,showstr))))))

(defun highlight-indent-guides--character-highlighter ()
  "Apply highlighting to the indentation.
Return highlighting information for the character at START. Displays a character
in place of the first column of each indentation level. This is meant to be used
as a `font-lock-keywords' face definition."
  (let ((prop (get-text-property start 'highlight-indent-guides-prop)))
    (highlight-indent-guides--cache-highlight
     'character prop
     (let* ((face 'highlight-indent-guides-character-face)
            (faceval (car prop)) (segs (nth 1 prop))
            (starter (nth 2 prop)) (cwidth (nth 3 prop))
            showstr)
       (if (and (null segs) (eq cwidth 1))
           `(face ,face display
                  ,(char-to-string highlight-indent-guides-character))
         (setq showstr (make-string cwidth ?\s))
         (when starter
           (aset showstr 0 highlight-indent-guides-character)
           (add-text-properties 0 1 `(face ,face) showstr))
         (dolist (seg segs)
           (aset showstr seg highlight-indent-guides-character)
           (add-text-properties seg (1+ seg) `(face ,face) showstr))
         `(face nil display ,showstr))))))

;;;###autoload
(define-minor-mode highlight-indent-guides-mode
  "Display indent guides in a buffer."
  nil nil nil
  (let ((fill-method-keywords
         '((highlight-indent-guides--fill-keyword-matcher
            0 (highlight-indent-guides--fill-highlighter) t)))
        (column-method-keywords
         '((highlight-indent-guides--column-keyword-matcher
            0 (highlight-indent-guides--column-highlighter) t)))
        (character-method-keywords
         '((highlight-indent-guides--column-keyword-matcher
            0 (highlight-indent-guides--character-highlighter) t))))
    (if highlight-indent-guides-mode
        (progn
          (add-to-list 'font-lock-extra-managed-props 'display)
          (font-lock-add-keywords
           nil
           (pcase highlight-indent-guides-method
             (`fill fill-method-keywords)
             (`column column-method-keywords)
             (`character character-method-keywords)))
          (jit-lock-register 'highlight-indent-guides--guide-region))
      (delete 'display 'font-lock-extra-managed-props)
      (font-lock-remove-keywords nil fill-method-keywords)
      (font-lock-remove-keywords nil column-method-keywords)
      (font-lock-remove-keywords nil character-method-keywords)
      (jit-lock-unregister 'highlight-indent-guides--guide-region)
      (highlight-indent-guides--unguide-region (point-min) (point-max))
      (font-lock-flush))))

(provide 'highlight-indent-guides)

;;; highlight-indent-guides.el ends here
