;;; bn-core.el --- Core functions to display text in Bangla  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Keywords: convenience
;; Version: 0.0.1
;; Homepage: https://github.com/md-arif-shaikh/bn
;; URL: https://github.com/md-arif-shaikh/bn

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; These some core functions to display strings and numbers in Bangla

;;; Code:

(defvar bn-core-digits '(("0" . "০")
			 ("1" . "১")
			 ("2" . "২")
			 ("3" . "৩")
			 ("4" . "৪")
			 ("5" . "৫")
			 ("6" . "৬")
			 ("7" . "৭")
			 ("8" . "৮")
			 ("9" . "৯")
			 ("." . ".")
			 ("%" . "%")
			 (":" . ":")
			 ("+" . "+")
			 ("-" . "-"))
  "Alist with each element of the form English-digit . Bangla-digit.")

(defun bn-core-convert-digit (digit)
  "Translate DIGIT to Bangla."
  (cdr (assoc (if (numberp digit)
		  (number-to-string digit)
		digit)
	      bn-core-digits)))

(defun bn-core-numberstring-to-digits (numberstring)
  "Get a list of strings of the digits in a NUMBERSTRING."
  (if (= (length numberstring) 0)
      '()
    (cons (substring numberstring 0 1) (bn-core-numberstring-to-digits (substring numberstring 1 (length numberstring))))))

(defun bn-core-convert-number (number)
  "Translate a NUMBER to Bangla.
Remember that in the process any preceding zeros are lost.
This is due to the fact that the `number-to-string` remove preceding zeros."
  (let* ((numberstring (if (numberp number)
			   (number-to-string number)
			 number))
	 (digits (bn-core-numberstring-to-digits numberstring)))
    (mapconcat #'bn-core-convert-digit digits "")))

(defcustom bn-core-day-names '(("MON" . "সোম")
			       ("TUE" . "মঙ্গল")
			       ("WED" . "বুধ")
			       ("THU" . "বৃহস্পতি")
			       ("FRI" . "শুক্র")
			       ("SAT" . "শনি")
			       ("SUN" . "রবি"))
  "Assoc list of day names."
  :type 'alist
  :group 'bn-core)

(defun bn-core-convert-day-name (day-name)
  "Translate DAY-NAME to Bangla."
  (let ((day (upcase (substring day-name 0 3))))
    (cdr (assoc day bn-core-day-names))))

(defcustom bn-core-month-names '(("JAN" . "জানুয়ারী")
				 ("FEB" . "ফেব্রুয়ারী")
				 ("MAR" . "মার্চ")
				 ("APR" . "এপ্রিল")
				 ("MAY" . "মে")
				 ("JUN" . "জুন")
				 ("JUL" . "জুলাই")
				 ("AUG" . "আগস্ট")
				 ("SEP" . "সেপ্টেম্বর")
				 ("OCT" . "অক্টোবর")
				 ("NOV" . "নভেম্বর")
				 ("DEC" . "ডিসেম্বর"))
  "Assoc list of month names."
  :type 'alist
  :group 'bn-core)

(defun bn-core-convert-month-name (month-name)
  "Translate MONTH-NAME to Bangla."
  (let ((month (upcase (substring month-name 0 3))))
    (cdr (assoc month bn-core-month-names))))

(provide 'bn-core)
;;; bn-core.el ends here
