;;; bn.el --- Display text and numbers in bangla     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Keywords: convenience
;; Version: 0.0.1
;; Homepage: https://github.com/md-arif-shaikh/bn
;; URL: https://github.com/md-arif-shaikh/bn
;; Package-Requires: ((emacs "26.2") (tzc "0.0.1"))

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
(require 'tzc)

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

(defcustom  bn-add-dayname-in-display-time-string? t
  "Add the dayname to date or time string in modeline."
  :type 'boolean
  :group 'bn)

(defcustom  bn-add-zoneinfo-in-display-time-string? t
  "Add the zoneinfo to date or time string in modeline."
  :type 'boolean
  :group 'bn)

(defcustom bn-add-second-clock-in-display-time-string? t
  "Add a second clock to the display time string."
  :type 'boolean
  :group 'bn)

(defcustom bn-second-clock-time-zone '("Asia/Kolkata" . "কোলকাতা")
  "Add a second clock to the display time string.
The car is the time-zone name in Area/city format and the cdr is a label we
want to use to display on the modeline."
  :type 'alist
  :group 'bn)

(defface bn-face-first-clock
  `((t :foreground "#98C379"
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for first clock."
  :group 'bn-face)

(defface bn-face-second-clock
  `((t :foreground "#56B6C2"
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for second clock."
  :group 'bn-face)

(defface bn-face-date-string
  `((t :foreground "#C678DD"
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for date string."
  :group 'bn-face)

(defface bn-face-day-name
  `((t :foreground "#E5C07B"
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for day name."
  :group 'bn-face)

(defun bn--get-second-clock-time-string ()
  "Get the time string for second clock."
  (let* ((time-list (tzc--get-converted-time (format-time-string "%R" (current-time)) nil (car bn-second-clock-time-zone)))
	 (min (nth 0 time-list))
	 (hour (nth 1 time-list))
	 (day-change (nth 2 time-list)))
    (format "🕓 %s:%s%s"
	    (bn-core-convert-number hour)
	    (bn-add-preceding-zero-to-date-time (bn-core-convert-number min))
	    (if (equal day-change 0)
		""
	      (concat " " (bn-core-convert-number day-change) " দিন")))))

(defcustom bn-display-time-string-forms
  '("🗓️ "
    ;; add dayname
    (when bn-add-dayname-in-display-time-string?
      (propertize (concat (bn-core-convert-day-name dayname) " ") 'face 'bn-face-day-name))
    ;; add date
    (propertize
     (concat
      (if bn-add-preceding-zero-to-date-time?
	  (bn-add-preceding-zero-to-date-time (bn-core-convert-number day))
	(bn-core-convert-number day))
      bn-date-separator
      (bn-core-convert-month-name monthname)
      bn-date-separator
      (bn-core-convert-number (substring year -2)))
     'face 'bn-face-date-string)
    " "
    ;; add time
    "🕐 "
    (propertize
     (concat (bn-core-convert-number 24-hours)
	     bn-time-separator
	     (bn-add-preceding-zero-to-date-time (bn-core-convert-number minutes))) 'face 'bn-face-first-clock)
    ;; add zoneinfo
    (when bn-add-zoneinfo-in-display-time-string?
      (concat " "
	      (propertize time-zone 'face 'bn-face-first-clock)))
    ;; add second clock
    (when bn-add-second-clock-in-display-time-string?
      (propertize
       (concat " " (bn--get-second-clock-time-string)
	       (when bn-add-zoneinfo-in-display-time-string?
		 (concat " " (cdr bn-second-clock-time-zone)))) 'face 'bn-face-second-clock)))
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

(defcustom bn-major-names '((help-mode "হেল্প")
			    (org-mode "অর্গ")
			    (org-agenda-mode "অর্গ-এজেন্ডা")
			    (magit-status-mode "ম্যাগিট-স্ট্যাটাস")
			    (dired-mode "ডায়ার্ড")
			    (lisp-interaction-mode "লিস্প ইন্টারএক্শন")
			    (emacs-lisp-mode "ইমাক্স-লিস্প")
			    (python-mode "পাইথন")
			    (shell-mode "শেল")
			    (eshell-mode "ইশেল")
			    (text-mode "টেক্স্ট")
			    (latex-mode "লেটেক্স")
			    (pdf-view-mode "পিডিএফ")
			    (message-mode "মেসেজ"))
 "Major mode names to show 'major-mode' name in Bangla."
 :type 'cons
 :group 'bn)

(defcustom bn-minor-names '((auto-revert-mode " অটো-রিভার্ট ")
			    (company-search-mode " কোম্পানি-সার্চ ")
			    (company-mode " কোম্পানি ")
			    (global-company-mode " গ্লোবাল-কোম্পানি ")
			    (yas-minor-mode " ইয়াস ")
			    (which-key-mode " হুইচ-কী ")
			    (ivy-mode " আইভি ")
			    (flycheck-mode " ফ্লাইচেক ")
			    (autopair-mode " অটো-পেয়ার ")
			    (eldoc-mode " এল-ডক ")
			    (diff-minor-mode " ডিফ ")
			    (visual-line-mode " ভিজুয়াল-লাইন ")
			    (all-the-icons-dired-mode  " অল-দ্য-আইকনস্-ডায়ার্ড ")
			    (dired-omit-mode (:eval (if ... " অমিট" ""))))
  "Minor mode names to show 'minor-mode' name in Bangla."
  :type 'cons
  :group 'bn)

(defun bn-set-major-mode-name ()
  "Set 'major-mode' name to Bangla using the BN-MAJOR-NAMES."
  (let ((bn-major-name (cdr (assq major-mode bn-major-names))))
    (when bn-major-name
      (setq mode-name bn-major-name))))

(defun bn-set-minor-mode-names ()
  "Set 'minor-mode' names to Bangla using bn-minor-names."
  (dolist (bn-minor bn-minor-names)
    (assoc-delete-all (car bn-minor) minor-mode-alist)
    (add-to-list 'minor-mode-alist bn-minor)))

(defun bn-appt-mode-line (min-to-app &optional abbrev)
  "Return an appointment string suitable for use in the mode-line.
MIN-TO-APP is a list of minutes, as strings.  If ABBREV is non-nil, abbreviates some text."
  ;; All this silliness is just to make the formatting slightly nicer.
  (let* ((multiple (> (length min-to-app) 1))
	 (imin (if (or (not multiple)
		       (not (delete (car min-to-app) min-to-app)))
		   (car min-to-app))))
    (format "%s%s %s"
	    (if abbrev "এপয়েন্টমেন্ট" "এপয়েন্টমেন্ট")
	    (if multiple "স" "")
	    (if (equal imin "0") "এখন "
	      (format "%s %s"
		      (or (bn-core-convert-number imin) (mapconcat #'identity (mapcar #'bn-core-convert-number min-to-app) ","))
		      (if abbrev "মিনিটে "
			(format "মিনিটে " (if (equal imin "1") "" ""))))))))

(defun bn-vc-git-mode-line-string (x)
  "Display vc-git-mode-line-string that is X in Bangla."
  (let ((current-branch (substring x 4))
	(status (substring x 3 4)))
    (setq current-branch (cond ((string-equal current-branch "master") "মাস্টার")
			       ((string-equal current-branch "main") "মেন")
			       (t current-branch)))
    (concat "গিট্" status current-branch)))

(provide 'bn)
;;; bn.el ends here
