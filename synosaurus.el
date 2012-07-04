;;; synosaurus.el --- An Emacs frontend for thesauri

;; Copyright (C) 2012  Hans-Peter Deifel

;; Author: Hans-Peter Deifel <hpdeifel@gmx.de>
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

;; TODO

;;; Code:

(require 'button)

(defvar synosaurus-lookup-function nil)

(defun synosaurus-internal-lookup (word)
  (if synosaurus-lookup-function
      (funcall synosaurus-lookup-function word)
    (error "No thesaurus lookup function specified")))

(defun synosaurus-strip-properties (string)
  (set-text-properties 0 (length string) nil string)
  string)

(defun synosaurus-guess-default ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (synosaurus-strip-properties (thing-at-point 'word))))

(defvar synosaurus-history nil)

(defun synosaurus-interactive ()
  (let* ((default (synosaurus-guess-default))
         (res (read-string (if default
                               (format "Word (default %s): " default)
                             "Word: ")
                           nil 'synosaurus-history)))
    (list
     (if (not (string= res ""))
         res
       default))))

(defun synosaurus-button-action (arg)
  (synosaurus-lookup (button-label arg)))

(defvar synosaurus-list-mode-map
  (let ((map (copy-keymap button-buffer-map)))
    (set-keymap-parent map special-mode-map)
    map))

(define-derived-mode synosaurus-list-mode special-mode "Synosaurus")

(defun synosaurus-lookup (word)
  (interactive (synosaurus-interactive))
  (let ((inhibit-read-only t))
    (with-current-buffer (get-buffer-create "*Synonyms List*")
      (erase-buffer)
      (insert
       (propertize (format "Synonyms of %s:\n\n" word)
                   'face 'success))
      (flet ((ins (syn)
                  (unless (string= word syn)
                    (insert " ")
                    (insert-text-button syn
                                        'action 'synosaurus-button-action)
                    (insert "\n"))))
        (dolist (syn (synosaurus-internal-lookup word))
          (if (not (listp syn))
              (ins syn)
            (dolist (syn2 syn)
              (ins syn2))
            (insert "\n"))))
      (goto-char (point-min))
      (condition-case nil (forward-button 1 t nil)
        (error nil))
      (synosaurus-list-mode)))
  (display-buffer "*Synonyms List*"))

(defvar synosaurus-choose-method 'popup)

(defun synosaurus-choose (list)
  (let ((completion-prompt "Replacement: "))
   (case synosaurus-choose-method
     (popup (popup-menu* list))
     (ido   (ido-completing-read completion-prompt list))
     (otherwise (completing-read completion-prompt list)))))

(defun synosaurus-choose-and-replace ()
  (interactive "")
  (let* ((word (synosaurus-guess-default))
         (syns
          (loop for syn in (synosaurus-internal-lookup word)
                if (listp syn) append syn
                else append (list syn)))
         (res (synosaurus-choose syns)))
    (if (use-region-p)
        (delete-region (region-beginning) (region-end))
      (delete-region (beginning-of-thing 'word)
                     (end-of-thing 'word)))
    (insert res)))

(provide 'synosaurus)
;;; synosaurus.el ends here
