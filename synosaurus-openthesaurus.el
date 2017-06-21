;;; synosaurus-openthesaurus.el --- OpenThesaurus backend for synosaurus

;; Copyright (C) 2017  Hans-Peter Deifel

;; Author: Hans-Peter Deifel <hpd@hpdeifel.de>
;; Keywords: wp

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

;; A german thesaurus

;;; Code:

(require 'synosaurus)
(require 'url)

(require 'cl-lib)

(defvar openthesaurus-url
  "http://www.openthesaurus.de/synonyme/search?q=%s&format=text/xml")

(defun openthesaurus-xml-collect (tree path fun)
  (when (and path (eq (car path) (car tree)))
    (if (null (cdr path))
        (funcall fun tree)
      (cl-loop for child in (cddr tree)
            for res = (openthesaurus-xml-collect child (cdr path) fun)
            when res collect res))))

;;;###autoload
(defun synosaurus-backend-openthesaurus (word)
  (let ((buf (url-retrieve-synchronously (format openthesaurus-url
                                                 (url-hexify-string word)))))
    (if (not buf)
        (error "could not retrieve openthesaurus data")
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "^$")        ;end of headers
        (forward-line)
        (let ((xml (libxml-parse-xml-region (point) (point-max))))
          (kill-buffer)
          (openthesaurus-xml-collect xml '(matches synset term)
                                     (lambda (x) (cdr (assoc 'term (cadr x))))))))))

(provide 'synosaurus-openthesaurus)
;;; synosaurus-openthesaurus.el ends here
