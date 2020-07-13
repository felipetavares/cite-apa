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

;; FIXME: escape org-mode characters in input strings!

(require 'cl)
(require 'ert)

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

(defun cite-apa--author-list (ref)
  ;; FIXME: keep ordering from the reference file
  (append (gethash "author" ref)
          (mapcar
           (lambda (org) `(t . ,org))
           (gethash "author-org" ref))))

(defun cite-apa--date-list (ref)
  (cite-apa--reference->dates ref))

(defun format-authors (ref)
  (cite-apa--format-authors (cite-apa--author-list ref)))

(defun format-date (ref)
  (cite-apa--format-dates (cite-apa--date-list ref)))

(defun cite-apa--title-info (ref)
  (let ((info (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (puthash k (car v) info)) ref)
    info))

(defun cite-apa--part-of-greater-whole (ref)
  (let ((kind (car (gethash "kind" ref))))
    (or (equal kind "article")
        (equal kind "chapter"))))

;; TODO
(defun cite-apa--non-academic-work (ref)
  (let ((kind (car (gethash "kind" ref))))
    nil))

(defun format-title (ref)
  (cite-apa--format-id (car (gethash "title" ref))
                       (cite-apa--title-info ref)
                       (cite-apa--part-of-greater-whole ref)
                       (cite-apa--non-academic-work ref)))

(defun format-source (ref)
  "Source formatting TBD")

(defun format-apa-book (ref)
  "Returns a reference in APA book format"
  (format "%s. (%s). /%s/. %s: %s."
          (cite-apa--trim-dot (format-authors ref))
          (car (gethash "year" ref))
          (car (gethash "title" ref))
          (car (gethash "location" ref))
          (car (gethash "publisher" ref))))

(defun format-apa-article (ref)
  "Return an APA styled string from reference REF."
  (string-join (list (format-authors ref)
                     (format-date ref)
                     (format-title ref)
                     (format-source ref))
               " "))

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
  (defvar cite-apa-library "~/.local/share/cite-apa/")

  (let* ((references (load-references cite-apa-library))
         (ref-title (completing-read "Reference: " (references->titles references))))
    (insert (reference->string (find-reference-by-title references ref-title)))))

;; Tests, derived from the APA Manual

;; Templates to help while translating
; (cite-apa--make-reference
;    '(("kind" . ("article"))
;      ("author" . ())
;      ("year" . ())
;      ("journal" . ())
;      ("volume" . ())
;      ("issue" . ())
;      ("page-from" . ())
;      ("page-to" . ())
;      ("url" . ())
;      ("title" . ())))
;
; (ert-deftest cite-apa-ex5 ()
;   (should (equal (reference->string (cite-apa--make-reference
;                                      '(("kind" . ("article"))
;                                        ("author" . ())
;                                        ("year" . ())
;                                        ("journal" . ())
;                                        ("volume" . ())
;                                        ("issue" . ())
;                                        ("page-from" . ())
;                                        ("page-to" . ())
;                                        ("url" . ())
;                                        ("title" . ()))))
;                  "")))

(defun cite-apa--make-reference (props)
  "Create a reference entry from alist PROPS."
  (let ((ref (make-hash-table :test 'equal)))
    (dolist (kv props ref)
      (puthash (car kv) (cdr kv) ref))))

(ert-deftest cite-apa-test-ex1 ()
  "Journal article with a DOI."
  (should (equal (reference->string (cite-apa--make-reference
                                     '(("kind" . ("article"))
                                       ("author" . ("S M McCauley" "M H Christiansen"))
                                       ("year" . ("2019"))
                                       ("journal" . ("Psychological Review"))
                                       ("volume" . ("126"))
                                       ("issue" . ("1"))
                                       ("page-from" . ("1"))
                                       ("page-to" . ("51"))
                                       ("doi" . ("https://doi.org/10.1037/rev0000126")) ; FIXME: maybe DOI should be specified in another format?
                                       ("title" . ("Language learning as language use: A cross-linguistic model of child language development")))))
                 "McCauley, S. M., & Christiansen, M. H. (2019). Language learning as language use: A cross-linguistic model of child language development. Psychological Review, 126(1), 1–51. https://doi.org/10.1037/rev0000126")))

(ert-deftest cite-apa-test-ex2 ()
  "Journal article without a DOI, with a nondatabase URL."
  (should (equal (reference->string (cite-apa--make-reference
                                     '(("kind" . ("article"))
                                       ("author" . ("E Ahmann" "L J Tuttle" "M Saviet" "S D Wright"))
                                       ("year" . ("2018"))
                                       ("journal" . ("Journal of Postsecondary Education and Disability"))
                                       ("volume" . ("31"))
                                       ("issue" . ("1"))
                                       ("page-from" . ("17"))
                                       ("page-to" . ("39"))
                                       ("url" . ("https://www.ahead.org/professional-resources/publications/jped/archived-jped/jped-volume-31"))
                                       ("title" . ("A descriptive review of ADHD coaching research: Implications for college students.")))))
                 "Ahmann, E., Tuttle, L. J., Saviet, M., & Wright, S. D. (2018). A descriptive review of ADHD coaching research: Implications for college students. Journal of Postsecondary Education and Disability, 31(1), 17–39. https://www.ahead.org/professional-resources/publications/jped/archived-jped/jped-volume-31")))

(ert-deftest cite-apa-test-ex3 ()
  "Journal, magazine, or newspaper article without a DOI, from most academic research databases or print version."
  (should (equal (reference->string (cite-apa--make-reference
                                     '(("kind" . ("article"))
                                       ("author" . ("M Anderson"))
                                       ("year" . ("2018"))
                                       ("journal" . ("Educational Leadership"))
                                       ("volume" . ("76"))
                                       ("issue" . ("1"))
                                       ("page-from" . ("26"))
                                       ("page-to" . ("33"))
                                       ("title" . ("Getting consistent with consequences")))))
                 "Anderson, M. (2018). Getting consistent with consequences. Educational Leadership, 76(1), 26–33."))
  (should (equal (reference->string (cite-apa--make-reference
                                     '(("kind" . ("article"))
                                       ("author" . ("C Goldman"))
                                       ("year" . ("2018"))
                                       ("month" . ("November"))
                                       ("day" . ("28"))
                                       ("newspaper" . ("Chicago Tribune"))
                                       ("title" . ("The complicated calibration of love")))))
                 "Goldman, C. (2018, November 28). The complicated calibration of love, especially in adoption. Chicago Tribune.")))

(ert-deftest cite-apa-ex4 ()
  "Journal article with a DOI, 21 or more authors"
  (should (equal (reference->string (cite-apa--make-reference
                                     '(("kind" . ("article"))
                                       ("author" . ("E Kalnay" "M Kanamitsu" "R Kistler"
                                                    "W Collins" "D Deaven" "L Gandin"
                                                    "M Iredell" "S Saha" "G White"
                                                    "J Woollen" "Y Zhu" "M Chelliah"
                                                    "W Ebisuzaki" "W Higgins" "J Janowiak"
                                                    "K C Mo" "C Ropelewski" "J Wang"
                                                    "A Leetmaa" "Filler Author" "D Joseph"))
                                       ("year" . ("1996"))
                                       ("journal" . ("Bulletin of the American Meteorological Society"))
                                       ("volume" . ("77"))
                                       ("issue" . ("3"))
                                       ("page-from" . ("437"))
                                       ("page-to" . ("471"))
                                       ("title" . ("The NCEP/NCAR 40-year reanalysis project"))
                                       ("doi" . ("http://doi.org/fg6rf9")))))
                 "Kalnay, E., Kanamitsu, M., Kistler, R., Collins, W., Deaven, D., Gandin, L., Iredell, M., Saha, S., White, G., Woollen, J., Zhu, Y., Chelliah, M., Ebisuzaki, W., Higgins, W., Janowiak, J., Mo, K. C., Ropelewski, C., Wang, J., Leetmaa, A., ... Joseph, D. (1996). The NCEP/NCAR 40-year reanalysis project. Bulletin of the American Meteorological Society, 77(3), 437–471. http://doi.org/fg6rf9")))

(ert-deftest cite-apa-ex5 ()
  "Journal article with a DOI, combination of individual and group authors"
  (should (equal (reference->string (cite-apa--make-reference
                                     '(("kind" . ("article"))
                                       ("author" . ("R De Vries" "M Nieuwenhuijze" "S E Nieuwenhuijze"))
                                       ("author-org" . ("the members of Midwifery Science Work Group"))
                                       ("year" . ("2013"))
                                       ("journal" . ("Midwifery"))
                                       ("volume" . ("29"))
                                       ("issue" . ("10"))
                                       ("page-from" . ("1122"))
                                       ("page-to" . ("1128"))
                                       ("doi" . ("https://doi.org/10.1016/j.midw.2013.07.007"))
                                       ("title" . ("What does it take to have a strong and independent profession of midwifery? Lessons from the Netherlands")))))
                 "De Vries, R., Nieuwenhuijze, M., yk, S. E., & the members of Midwifery Science Work Group. (2013). What does it take to have a strong and independent profession of midwifery? Lessons from the Netherlands. Midwifery, 29(10), 1122–1128. https://doi.org/10.1016/j.midw.2013.07.007")))

(ert-deftest cite-apa-ex6 ()
  "Journal article with an article number or eLocator"
  (should (equal (reference->string (cite-apa--make-reference
                                     '(("kind" . ("article"))
                                       ("author" . ("D Burin" "K Kilteni" "M Rabuffetti" "M Slater" "L Pia"))
                                       ("year" . ("2019"))
                                       ("journal" . ("PLOS ONE"))
                                       ("volume" . ("14"))
                                       ("issue" . ("1"))
                                       ("doi" . ("https://doi.org/10.1371/journal.pone.0209899"))
                                       ("elocator" . ("e0209899"))
                                       ("title" . ("Body ownership increases the interference between observed and executed movements")))))
                 "Burin, D., Kilteni, K., Rabuffetti, M., Slater, M., & Pia, L. (2019). Body ownership increases the interference between observed and executed movements. PLOS ONE, 14(1), Article e0209899. https://doi.org/10.1371/journal.pone.0209899")))

(ert-deftest cite-apa-ex7 ()
  "Journal article, advance online publication"
  (should (equal (reference->string (cite-apa--make-reference
                                     '(("kind" . ("article"))
                                       ("author" . ("S M Huestegge" "T Raettig" "L Huestegge"))
                                       ("year" . ("2019"))
                                       ("journal" . ("Experimental Psychology"))
                                       ("online-advance" . (""))
                                       ("doi" . ("https://doi.org/10.1027/1618-3169/a000440"))
                                       ("title" . ("Are face-incongruent voices harder to process? Effects of face–voice gender incongruency on basic cognitive information processing")))))
                 "Huestegge, S. M., Raettig, T., & Huestegge, L. (2019). Are face-incongruent voices harder to process? Effects of face–voice gender incongruency on basic cognitive information processing. Experimental Psychology. Advance online publication. https://doi.org/10.1027/1618-3169/a000440")))

(provide 'cite-apa)
;;; cite-apa.el ends here
