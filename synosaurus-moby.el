;;; synosaurus-moby.el --- Moby backend for synosaurus  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Matt Beshara

;; Author: Matt Beshara <m@mfa.pw>

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

;;; An interface to the public domain Moby thesaurus.
;;; The Moby Thesaurus is ~24MB and can be downloaded from:
;;; https://www.gutenberg.org/files/3202/files/mthesaur.txt

;;; Code:

(require 'synosaurus)
(require 'synosaurus-wordnet)

(defcustom synosaurus-moby-path (concat user-emacs-directory "mthesaur.txt")
  "Path to the Moby thesaurus.")

(defcustom synosaurus-moby-more-results t
  "When non-nil, searches for more related words."
  :type 'boolean)

(defcustom synosaurus-moby-more-results-separate nil
  "When non-nil, display additional related words separately."
  :type 'boolean)

(defvar synosaurus-moby--buffer nil
  "Buffer contianing the Moby thesaurus.")

(defun synosaurus-moby-load (&optional path)
  "Loads data from Moby thesaurus file.
Loads from PATH, or ‘synosaurus-moby-path’ if nil."
  (let ((real-path (or path synosaurus-moby-path)))
    (unless (file-exists-p real-path)
      (user-error "Moby thesaurus file not found at %s"
                  synosaurus-moby-path))
    (setq synosaurus-moby--buffer
          (find-file-noselect real-path
                              t
                              t))
    (with-current-buffer synosaurus-moby--buffer
      (rename-buffer " *Synosaurus Moby thesaurus*"))))

(defun synosaurus-moby--main-synonyms (word)
  (let ((case-fold-search t))
    (goto-char (point-min))
    (when (search-forward-regexp (format "^%s," word) nil t)
      (synosaurus-wordnet--collect-list))))

(defun synosaurus-moby--referencing-synonyms (word found)
  (let ((case-fold-search t)
        (more-words))
    (goto-char (point-min))
    (while (search-forward-regexp (format ",%s[,\n\r]" word) nil t)
      (beginning-of-line)
      (let* ((start (point))
             (end (1- (search-forward-regexp "," nil t)))
             (word (buffer-substring-no-properties start end)))
        (unless (member word found)
          (push word more-words)))
      (forward-line 1))
    more-words))

;;;###autoload
(defun synosaurus-backend-moby (word)
  (when (or (null synosaurus-moby--buffer)
            (not (buffer-live-p synosaurus-moby--buffer)))
    (synosaurus-moby-load))
  (with-current-buffer synosaurus-moby--buffer
    (let* ((main (synosaurus-moby--main-synonyms word))
           (referencing
            (when synosaurus-moby-more-results
              (synosaurus-moby--referencing-synonyms word main))))
      (if synosaurus-moby-more-results-separate
          (list main (sort referencing 'string<))
          (sort (append main referencing) 'string<)))))

(provide 'synosaurus-moby)
;;; synosaurus-moby.el ends here
