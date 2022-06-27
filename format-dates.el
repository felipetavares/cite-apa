;;; ~/.doom.d/cite-apa/format-date.el -*- lexical-binding: t; -*-

;; REF: Section 9.13 of APA manual

(defun cite-apa--valid-date? (date)
  (or (gethash "year" date)
      (and (gethash "year" date) (gethash "month" date) (gethash "day" date))
      (and (gethash "year" date) (gethash "month" date))
      (and (gethash "year" date) (gethash "season"))
      ; No date is also valid
      (and (not (gethash "year" date))
           (not (gethash "month" date))
           (not (gethash "seasson" date))
           (not (gethash "day" date)))))

(defun cite-apa--valid-dates? (dates)
  (and (<= (length dates) 2)
       (seq-reduce (lambda (acc date)
                     (and acc (cite-apa--valid-date? date)))
                   dates
                   t)))

;; TODO this function will build a list of date objects as defined by the
;; following schema
;;
;; date-set is a list of dates
; (let ((date (make-hash-table)))
;   (puthash "year" 0 date)
;   (puthash "month" "Jan" date)
;   (puthash "day" 0 date)
;   (puthash "season" "Summer" date)
;   date)
(defun cite-apa--reference->dates (ref)
  (let ((dates))
    ;; TODO: find a better way to group those together
    ; Insert years into the list
    (dolist (year (gethash "year" ref))
      (let ((date (make-hash-table :test 'equal)))
        (puthash "year" year date)
        (push date dates)))
    dates))

(defun cite-apa--multipart-date? (date)
  (or (gethash "month" date) (gethash "season" date)))

(defun cite-apa--format-date-complement (month/season &optional day)
  (if day
      (format "%s %s" month/season day)
    (format "%s" month/season)))

(defun cite-apa--format-date (date)
  (let ((year (gethash "year" date))
        (month (gethash "month" date))
        (season (gethash "season" date))
        (day (gethash "day" date)))
    (if (cite-apa--multipart-date? date)
        (format "%s, %s" (cite-apa--format-date-complement (or month season) day))
      (format "%s" year))))

;; TODO check the correct syntax for date ranges
(defun cite-apa--format-dates (dates)
  (if (cite-apa--valid-dates? dates)
      (format "(%s)." (string-join (mapcar 'cite-apa--format-date dates) "-"))
    (cite-apa--error "invalid dates in reference")))

(provide 'format-dates)
