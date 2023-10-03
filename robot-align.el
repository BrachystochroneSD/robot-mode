;;; robot-indent.el --- robot mode for emacs         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Samuel Dawant

;; Author: Samuel Dawant <samueld@mailo.com>
;; Keywords: languages

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

;; Gather all function related to alignment

;;; Code:

(require 'align)

(defun robot-align-split-line ()
  "Split current line at point and continue in the next line.

Prefix the continuation with indentation, ellipsis and spacing."
  (interactive)
  ;; If point is between the indentation and beginning of line add the
  ;; ellipsis to the previous line. Otherwise add to the next line.
  (if (> (current-column) (current-indentation))
      (progn
	(delete-horizontal-space)
	(newline))
    (beginning-of-line)
    (newline)
    (forward-line -1))
  (insert "...")
  (indent-region (line-beginning-position) (line-end-position))
  (robot-align-new-argument))

(defun robot-align--get-biggest-list (&rest lists)
  (let ((res '()))
    (dolist (l lists)
      (when (length> l (length res)) (setq res l)))
    res))

(defun robot-align-new-argument ()
  (interactive)
  "Go the the point for a new argument to align with the previous line."
  (delete-horizontal-space)
  (let* ((cla (robot-align-get-line-alignment))
         (chosen-list (robot-align--get-biggest-list
                       (robot-align-get-line-alignment -1)
                       (robot-align-get-line-alignment 1)))
         (pot-col (+ robot-mode-argument-separator (current-column)))
         (res-col
          (cond
           ((or (not chosen-list) (< (length chosen-list) (length cla)))
            pot-col)
           ((= (length chosen-list) (length cla))
            (max pot-col
                 (+ robot-mode-argument-separator (cdar (last chosen-list)))))
           (t
            (let ((chosen-col 0))
              (while (and chosen-col (< chosen-col (current-column)))
                (setq chosen-col (car (pop chosen-list))))
              chosen-col)))))
    (robot-align--goto-col-forced (or res-col pot-col))))

(defun robot-align-region-or-block ()
  "Call `robot-align-region' if region is active, otherwise `robot-align-paragraph'."
  (interactive)
  (if (region-active-p)
      (robot-align-region (region-beginning) (region-end))
    (robot-align-paragraph)))

(defun robot-align-defun ()
  "Align the contents current defun."
  (interactive)
  (let ((beg (save-excursion
	       (beginning-of-defun)
	       (forward-line)
	       (point)))
	(end (save-excursion
	       (end-of-defun)
	       (point))))
    (robot-align-region beg end)))

(defun robot-align-region (beg end)
  "Align the contents of the region between BEG and END."
  (interactive
   (list (region-beginning) (region-end)))
  ;; Align only with spaces
  (let ((align-to-tab-stop nil))
    (align-regexp beg end "\\(\\s-\\s-+\\)"  1 robot-mode-argument-separator t))
  (indent-region beg end))

(defun robot-align-region-or-defun ()
  "Call `robot-align' if region is active, otherwise `robot-align-defun'."
  (interactive)
  (if (region-active-p)
      (robot-align-region (region-beginning) (region-end))
    (robot-align-defun)))

(defun robot-align--goto-col-forced (column)
  "Got to the given column and force it with spaces"
  (let ((line-length
         (length
          (save-excursion
            (beginning-of-line) (looking-at ".*") (match-string 0)))))
    (insert (make-string (- column (current-column)) ? ))))


(defun robot-align-get-line-alignment (&optional number)
  "Return list of all arguments START and END column number in the form of (START . END)"
  ;; CLEAME: This is not very elegant.
  (save-excursion
    (forward-line (or number 0))
    ;; (while (or (looking-at "[[:space:]]*\\(#.*\\)*$") (bobp)) ;; skip empty or comments lines
    ;; (forward-line (or number -1)))
    (if (looking-at "[^ ]") ;; No defun allowed
        nil
      (let ((i 0)
            (splitted (split-string
                       (buffer-substring (line-beginning-position) (line-end-position))
                       "\\(\\s-\\s-+\\)" t))
            (res '()))
        (while (re-search-forward "\\(\\s-\\s-+\\)" (line-end-position) t)
          (let ((curr-col (current-column)))
            (setq res (append res
                              (list
                               (cons curr-col
                                     (+ curr-col (length (nth i splitted))))))))
          (setq i (1+ i)))
        res))))



(provide 'robot-align)
;;; robot-align.el ends here
