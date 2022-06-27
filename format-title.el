;;; ~/.doom.d/cite-apa/format-title.el -*- lexical-binding: t; -*-

;; APA title formatting

(defun cite-apa--add-period (str)
  "Adds a period to str if it does not end with ? or ! or ."
  (if (let ((last-char (substring str (1- (length str)))))
        (or (equal last-char ".") (equal last-char "!") (equal last-char "?")))
      str
    (concat str ".")))

;; TODO
(defun cite-apa--sentence-case (sentence)
  sentence)

(defun cite-apa--italicize (str)
  (format "/%s/" str))

(defun cite-apa--format-description (title info naw)
  (let ((desc (gethash "description" info)))
    (if (or naw (not title))
        (format "[%s]" (or desc "Please provide a description for this work!")))))

(defun cite-apa--format-ord (n)
  (cond ((or (= n 1) (= n 21) (= n 31)) (format "%sst" n))
        ((or (= n 2) (= n 22) (= n 32)) (format "%snd" n))
        ((or (= n 3) (= n 23) (= n 33)) (format "%srd" n))
        (true (format "%s" n))))

(defun cite-apa--format-title (title part-of-greater-whole)
  (if title
      (let ((capitalized-title (cite-apa--sentence-case title)))
        (if part-of-greater-whole
            capitalized-title
          (cite-apa--italicize capitalized-title)))))

(defun cite-apa--info->str (key value)
  (cond ((equal key "volume-title") (format "%s" value)) ; FIXME: add in the
                                                         ; correct formatting
        ((equal key "volume") (format "Vol. %s" value))
        ((equal key "report") (format "Re. %s" value)) ; FIXME: is report = issue?
        ((equal key "version") (format "Version %s" value))
        ((equal key "edition") (format "%s ed." (cite-apa--format-ord
                                                 (string-to-number value))))))

(defun cite-apa--format-extra-id-info (info)
  ;; FIXME: sort in specific order?
  (let ((info-list))
    ; Build a list of formatted string properties
    ; (only for books and reports)
    (if (or (equal (gethash "kind" info) "book")
            (equal (gethash "kind" info) "report"))
        (maphash (lambda (k v)
                   (push (cite-apa--info->str k v) info-list)) info))
    ; Merge and return
    (let ((information (reverse (remove 'nil info-list))))
      (if information (string-join information ", ")))))

(defun cite-apa--merge-title-and-info (title-str info-str info)
  "Merges intermediary representations of title and info."
  (if title-str
      (if info-str
          (if (gethash "volume-title" info)
              (format "%s: %s" title-str info-str)
            (format "%s (%s)" title-str info-str))
        title-str)))

;; options is a hash containing (optionally)
;; - volume-title
;; - volume-number
;; - report-number
;; - edition
;; - version
;; - description
;;
;; TODO: non-academic-work & description may be automatically inserted depending
;; on the type of work.
(defun cite-apa--format-id (title
                            &optional
                            extra-info
                            part-of-greater-whole
                            non-academic-work)
  ; Defaults extra-info to an empty hash table
  (setq extra-info (or extra-info (make-hash-table :test 'equal)))

  (cite-apa--add-period
   (string-join
    (remove 'nil (list (cite-apa--merge-title-and-info
                        (cite-apa--format-title title part-of-greater-whole)
                        (cite-apa--format-extra-id-info extra-info)
                        extra-info)
                       (cite-apa--format-description title extra-info non-academic-work)))
    " ")))

(provide 'format-title)
