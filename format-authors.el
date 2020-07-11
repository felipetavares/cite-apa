;;; ~/.doom.d/cite-apa/format-author.el -*- lexical-binding: t; -*-

;; APA author formatting

;; TODO: support multipart author names
;; TODO: support suffixes (Jr. II, III)
;; TODO: support hyphenated names
;; TODO: support usernames
;; TODO: create a better data type for usernames
;; TODO: Specialized Roles
;; TODO: No author

;; REF: Section 9.8 of the APA Manual

(defun cite-apa--surname (author-name)
  (car (last (split-string author-name " "))))

(defun cite-apa--author-name (author)
  (if (listp author) (cdr author) author))

(defun cite-apa--is-author-org (author)
  (if (listp author) (car author) nil))

(defun cite-apa--is-author-solid (author)
  (or (cite-apa--is-author-org author)
      (< (length (split-string (cite-apa--author-name author) " ")) 2)))

(defun cite-apa--initials (author-name)
  (string-join
   (mapcar (lambda (name) (concat (substring name 0 1) "."))
           (butlast (split-string author-name " ")))
   " "))

(defun cite-apa--format-author (author &optional prefix-amp)
  (let* ((author-name (cite-apa--author-name author))
         (formatted (if (cite-apa--is-author-solid author)
                        author-name
                      (format "%s, %s"
                              (cite-apa--surname author-name)
                              (cite-apa--initials author-name)))))
    (if prefix-amp (concat "& " formatted) formatted)))

(defun cite-apa--should-prefix-amp (a authors no-amp)
  (and (equal (- (length authors) a) 1)
       (> (length authors) 1)
       (not no-amp)))

(defun cite-apa--author-separator (authors)
  ;; Do not use , to separate if we have two org authors
  (if (and (seq-reduce (lambda (acc a) (and acc (cite-apa--is-author-org a)))
                       authors t)
           (equal (length authors) 2))
      " " ", "))

(defun cite-apa--format-authors-20 (authors &optional no-amp)
  (let ((inv-formatted-authors))
    ;; Format each author and add a & before the last one
    (dotimes (a (length authors))
      (push (cite-apa--format-author (nth a authors)
                                     (cite-apa--should-prefix-amp a authors no-amp))
            inv-formatted-authors))

    ;; Join the list of authors
    (let ((formatted-authors (reverse inv-formatted-authors)))
      (string-join formatted-authors (cite-apa--author-separator authors)))))

(defun cite-apa--format-authors (authors)
  "Given a list of authors, return a single APA-formatted authors string"
  (if (> (length authors) 20)
      (format "%s... %s"
              (cite-apa--trim-dot (cite-apa--format-authors-20 (seq-subseq authors 0 19) t))
              (cite-apa--format-author (car (last authors))))
    (cite-apa--format-authors-20 authors)))
