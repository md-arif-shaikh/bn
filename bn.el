;;; bn.el --- Display text and numbers in bangla     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Keywords: convenience
;; Version: 0.0.1
;; Homepage: https://github.com/md-arif-shaikh/bn
;; URL: https://github.com/md-arif-shaikh/bn
;; Package-Requires: ((emacs "24.1"))

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

;; Display text and numbers in time, date, battery status etc in Bangla

;;; Code:
(require 'bn-core)
(require 'battery)

(defun bn-add-preceding-zero-to-date-time (bn-date-or-time)
  "Add a preceding zero to BN-DATE-OR-TIME."
  (if (= (length bn-date-or-time) 1)
      (concat "০" bn-date-or-time)
    bn-date-or-time))

(defcustom bn-date-separator "/"
  "Separator in date string in modeline."
  :type 'string
  :group 'bn)

(defcustom bn-time-separator ":"
  "Separator in time string in modeline."
  :type 'string
  :group 'bn)

(defcustom  bn-add-preceding-zero-to-date-time? t
  "Add a preceding zero to date or time string in modeline."
  :type 'boolean
  :group 'bn)

(defcustom bn-display-time-string-forms
  '((if bn-add-preceding-zero-to-date-time?
	(bn-add-preceding-zero-to-date-time (bn-core-convert-number day))
      (bn-core-convert-number day))
    bn-date-separator
    (bn-core-convert-month-name monthname)
    bn-date-separator
    (bn-core-convert-number (substring year -2))
    " "
    (bn-core-convert-number 24-hours)
    bn-time-separator
    (bn-add-preceding-zero-to-date-time (bn-core-convert-number minutes)))
  "Display time string in modeline in Bangla."
  :type 'list
  :group 'bn)

(defun bn-battery-update ()
  "Update battery status information in the mode line in Bangla."
  (let* ((data (and battery-status-function (funcall battery-status-function)))
         (percentage (car (read-from-string (cdr (assq ?p data))))))
    (setq battery-mode-line-string
	  (propertize (if (and battery-mode-line-format
			       (numberp percentage)
                               (<= percentage battery-mode-line-limit))
			  (progn
			    (setf (cdr (assq ?p data)) (bn-core-convert-number percentage))
			    (battery-format battery-mode-line-format data))
			"")
		      'face
                      (and (numberp percentage)
                           (<= percentage battery-load-critical)
                           'error)
		      'help-echo "ব্যাটারী অবস্থা সম্পর্কিত তথ্য")))
  (force-mode-line-update))

(provide 'bn)
;;; bn.el ends here
