;;; cite-apa.el --- org-mode formatting for APA reference library -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Felipe Tavares

;; Author: Felipe Tavares <felipe.oltavares@gmail.com>
;; Keywords: APA, reference, citation, library, research, org-mode
;; Package-Requires: ((emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; APA 7th Edition Bibliography Management
;; For use with org-mode
;; It is meant to be comprehensive but we are not there yet!

;;; Code:

(require 'ert)
(require 'cl)

;; Functions useful to formatting in many places
(defun cite-apa--trim-dot (str)
  (if (> (length str) 0)
      (if (equal (substring str (1- (length str))) ".")
          (substring str 0 (1- (length str)))
        str)))

(defun cite-apa--error (msg)
  (format "*%s*" (upcase msg)))

(load! "format-authors.el")
(load! "format-dates.el")
(load! "format-title.el")

(defun file->lines (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun line-is-definition (line)
  (not (equal (substring line 0 1) " ")))

(defun line->key (line)
  (let ((str (string-trim line)))
    (substring str 0 (string-match " " str))))

(defun line->value (line)
  (let ((str (string-trim line)))
    (substring str (+ 1 (string-match " " str)))))

(defun line->reference (line)
  (let ((reference (make-hash-table :test 'equal)))
    (puthash "title" (list (line->value line)) reference)
    (puthash "kind" (list (line->key line)) reference)
    reference))

(defmacro make-reference (line buffer refs)
  `(progn
     (setq ,buffer (line->reference ,line))
     (push ,buffer ,refs)))

(defmacro add-to-reference (reference line)
  `(let* ((key (line->key ,line))
          (new-value (line->value ,line))
          (current-value (gethash key ,reference)))
     (puthash key (append current-value (list new-value)) ,reference)))

;; TODO: allow empty lines
(defun file->references (file-path)
  "Parses the file on file-path and returns the contained references"
  (let ((references (list)) (buffer))
    (dolist (line (file->lines file-path) references)
      (if (line-is-definition line)
          (make-reference line buffer references)
        (add-to-reference buffer line)))))

;; FIXME: return a map to improve performance
(defun load-references (dir)
  ;; List all files in the config directory
  (let ((ref-files-list (f-files dir)))
    ;; Extract references with file->references and merge them all in a single
    ;; list
    (mapcan 'file->references ref-files-list)))

(defun cite-apa--build-author-list (ref)
  ;; FIXME: keep ordering from the reference file
  (append (gethash "author" ref)
          (mapcar
           (lambda (org) `(t . ,org))
           (gethash "author-org" ref))))

(defun format-authors (ref)
  (cite-apa--format-authors (cite-apa--build-author-list ref)))

(defun format-apa-book (ref)
  "Returns a reference in APA book format"
  (format "%s. (%s). /%s/. %s: %s."
          (cite-apa--trim-dot (format-authors ref))
          (car (gethash "year" ref))
          (car (gethash "title" ref))
          (car (gethash "location" ref))
          (car (gethash "publisher" ref))))

;; TODO
(defun format-apa-article (ref)
  "Returns a reference in APA article format")

(defun format-apa-webpage (ref)
  "Returns a reference in APA book format"
  (format "%s. (%s). %s. /%s/. Retrieved from %s"
          (cite-apa--trim-dot (format-authors ref))
          (car (gethash "year" ref))
          (car (gethash "title" ref))
          (car (gethash "publication" ref))
          (car (gethash "url" ref))))

;; FIXME: handle missing information (locations, publishers, dates)
;; FIXME: handle editors
;; FIXME: handle non-existent kinds
(defun reference->string (ref)
  (let ((types '(("book" . format-apa-book)
                 ("webpage" . format-apa-webpage)
                 ("article" . format-apa-article))))
    (apply (cdr (assoc (car (gethash "kind" ref)) types)) (list ref))))


(defun find-reference-by-title (references title)
  (seq-find (lambda (ref) (equal (car (gethash "title" ref)) title)) references))

(defun references->titles (references)
  (mapcar (lambda (ref) (car (gethash "title" ref))) references))

;; Searchs the bibliography
(defun insert-apa-reference ()
  "Searchs the bibliography and inserts an (org-mode) APA-formatted entry at point"
  (interactive)
  ;; Reference (bibliography) files can be added at this directory
  (defvar cite-apa-ref-root "~/.local/share/cite-apa/")

  (let* ((references (load-references cite-apa-ref-root))
         (ref-title (completing-read "Reference: " (references->titles references))))
    (insert (reference->string (find-reference-by-title references ref-title)))))

(defun cite-apa--make-reference (props)
  "Create a reference entry from alist PROPS."
  (let ((ref (make-hash-table :test 'equal)))
    (dolist (kv props ref)
      (puthash (car kv) (cdr kv) ref))))

;  (,(cite-apa--make-reference
;     '(("kind" . ("article"))
;       ("author" . ())
;       ("year" . ())
;       ("journal" . ())
;       ("volume" . ())
;       ("issue" . ())
;       ("page-from" . ())
;       ("page-to" . ())
;       ("url" . ())
;       ("title" . ())))
;   "")

;; A direct translation of the examples section from "Publication Manual Of The APA".
;; This dataset enables us to check our implementation against known-good data.
(defconst cite-apa--tests
  `(
    ;; 1
    (,(cite-apa--make-reference
       '(("kind" . ("article"))
         ("author" . ("S M McCauley", "M H Christiansen"))
         ("year" . ("2019"))
         ("journal" . ("Psychological Review"))
         ("volume" . ("126"))
         ("issue" . ("1"))
         ("page-from" . ("1"))
         ("page-to" . ("51"))
         ("doi" . ("https://doi.org/10.1037/rev0000126")) ; FIXME: maybe DOI should be specified in another format?
         ("title" . ("Language learning as language use: A cross-linguistic model of child language development"))))
     "McCauley, S. M., & Christiansen, M. H. (2019). Language learning as language use: A cross-linguistic model of child language development. Psychological Review, 126(1), 1–51. https://doi.org/10.1037/rev0000126")

    ;; 2
    (,(cite-apa--make-reference
       '(("kind" . ("article"))
         ("author" . ("E Ahmann", "L J Tuttle", "M Saviet", "S D Wright"))
         ("year" . ("2018"))
         ("journal" . ("Journal of Postsecondary Education and Disability"))
         ("volume" . ("31"))
         ("issue" . ("1"))
         ("page-from" . ("17"))
         ("page-to" . ("39"))
         ("url" . ("https://www.ahead.org/professional-resources/publications/jped/archived-jped/jped-volume-31"))
         ("title" . ("A descriptive review of ADHD coaching research: Implications for college students."))))
     "Ahmann, E., Tuttle, L. J., Saviet, M., & Wright, S. D. (2018). A descriptive review of ADHD coaching research: Implications for college students. Journal of Postsecondary Education and Disability, 31(1), 17–39. https://www.ahead.org/professional-resources/publications/jped/archived-jped/jped-volume-31")

    ;; 3
    (,(cite-apa--make-reference
       '(("kind" . ("article"))
         ("author" . ("M Anderson"))
         ("year" . ("2018"))
         ("journal" . ("Educational Leadership"))
         ("volume" . ("76"))
         ("issue" . ("1"))
         ("page-from" . ("26"))
         ("page-to" . ("33"))
         ("title" . ("Getting consistent with consequences"))))
     "Anderson, M. (2018). Getting consistent with consequences. Educational Leadership, 76(1), 26–33.")
    (,(cite-apa--make-reference
       '(("kind" . ("article"))
         ("author" . ("C Goldman"))
         ("year" . ("2018"))
         ("month" . ("November"))
         ("day" . ("28"))
         ("newspaper" . ("Chicago Tribune"))
         ("title" . ("The complicated calibration of love"))))
     "Goldman, C. (2018, November 28). The complicated calibration of love, especially in adoption. Chicago Tribune.")
    )
  )

(defun cite-apa--test (input-output)
  "Given a INPUT-OUTPUT list in (input . output) format, return t if cite-apa produces output for input."
  (let ((input (car input-output))
        (output (car (cdr input-output))))
    (equal (reference->string input) output)))

(defun cite-apa--test-all ()
  "Check if every cite-apa test does return true."
  (cl-every 'cite-apa--test cite-apa--tests))

(ert-deftest cite-apa-test ()
  (should (cite-apa--test-all)))

(provide 'cite-apa)
;;; cite-apa.el ends here
