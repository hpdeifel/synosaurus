;;; synosaurus-el.el --- dict.leo.org backend for synosaurus  -*- lexical-binding: t -*-

;; Copyright (C) 2021  hrdl
;; based on works
;; Copyright (C) 2019 Hans-Peter Deifel <hpd@hpdeifel.de>

;; Author: hrdl <emacs@hrdl.eu>

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

;; German -> English dictionary via dict.leo.org

;;; Code:

(require 'synosaurus)
(require 'url)

(require 'cl-lib)
(require 'xml)
(require 'dom)

(defvar synosaurus-leo--url
  "https://dict.leo.org/dictQuery/m-vocab/ende/query.xml?search=%s&side=right")

;;;###autoload
(defun synosaurus-backend-leo (word)
  "TODO"
  (let ((buf (url-retrieve-synchronously (format synosaurus-leo--url
                                                 (url-hexify-string word)))))
    (if (not buf)
        (error "Could not retrieve leo data")
      (with-current-buffer buf
        (goto-char (point-min))
	(let* ((tree (xml--parse-buffer t nil))
	       (nodes_en (dom-search (cdr tree)
				     (lambda (node) (and
						     (string= (dom-tag node) "side")
						     (string= (dom-attr node 'lang) "en"))))))
	  (kill-buffer)
	  (mapcar (lambda (node) (substring-no-properties (dom-text (dom-child-by-tag (dom-child-by-tag node 'words) 'word)))) nodes_en))))))


(provide 'synosaurus-leo)
;;; synosaurus-leo.el ends here
