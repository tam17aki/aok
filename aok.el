;;; aok.el --- various useful ways to do `multi-occur'
;; Version: 20130815.756

;; Copyright (C) 2013 Akira Tamamori
;; Copyright (C) 2013 lynnux <lynnux.cn@gmail.com>
;; Copyright (C) 2004 Joe Corneli <jcorneli@math.utexas.edu>

;; This file is NOT part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; search all buffers, all buffers whose filenames have a certain
;; type, or all buffers in a certain mode, with `all-occur',
;; `type-occur', or `mode-occur', respectively.

;;; Changelog:
;;; 2013.8.6 updated for emacs 24.x, and occur-select by lynnux

;;; Code:

(eval-when-compile (require 'cl))

(defvar aok-buffer-name-exclusion-regexp (rx bol (or "*" "%" ":" " ")))
(defvar aok-buffer-name-inclusion-list '("*scratch*" "*Packages*"))
(defvar aok-buffer-name-inclusion-regexp
  (regexp-opt aok-buffer-name-inclusion-list))

(defun aok-buffer-name-filter ()
  (let ((excludes aok-buffer-name-exclusion-regexp)
        (includes aok-buffer-name-inclusion-regexp))
    (remove-if (lambda (buf)
                 (let* ((name (buffer-name buf))
                        (include-p (if (= (length includes) 0) nil
                                     (numberp (string-match includes name))))
                        (exclude-p (if (= (length excludes) 0) nil
                                     (numberp (string-match excludes name))))
                        (filter (cond (include-p nil)
                                      ((not exclude-p) nil)
                                      (t t))))
                   filter))
               (buffer-list))))

(defun aok-get-buffer-list (&optional mode)
  (cond (mode
         (remove-if (lambda (buf)
                      (with-current-buffer buf
                        (not (eq major-mode mode))))
                    (aok-buffer-name-filter)))
        (t
         (aok-buffer-name-filter))))

(defun aok-get-major-mode-list ()
  (let ((major-mode-list nil))
    (loop for buf in (aok-get-buffer-list)
          for m = (format "%s" (with-current-buffer buf major-mode))
          unless (member m major-mode-list)
          do (push m major-mode-list))
    major-mode-list))

;;;###autoload
(defun all-occur (arg regexp)
  "Search all buffers for REGEXP."
  (interactive "P\nMRegexp: ")
  (cond (arg
         (multi-occur (buffer-list) regexp))
        (t
         (multi-occur (aok-get-buffer-list) regexp))))

;; this one {c}/{sh}ould be a completing read that would read from a
;; predefined list of filetype extensions (without requiring a match).
;;;###autoload
(defun type-occur (extension regexp)
  "EXTENSION denotes a filetype extension to search.
Run occur in all buffers whose names match this type for REGEXP."
  (interactive "MExtension: \nMRegexp: ")
  (multi-occur-in-matching-buffers (concat ".*\." extension) regexp))

;;;###autoload
(defun mode-occur (mode regexp)
  "Search all buffers with major mode MODE for REGEXP."
  (interactive (list (completing-read "Mode: " (aok-get-major-mode-list))
                     (read-string "Regexp: ")))
  (multi-occur (aok-get-buffer-list (intern-soft mode)) regexp))

(defvar aok-occur-select-prompt
  "Occur in: [a]ll, [t]ype, [m]ode, or just this buffer (any other key)?")

;;;###autoload
(defun occur-select (more regexp &optional nothing)
  "select what you wan't to see occur"
  (interactive
   (cons
    (let* ((choice (read-char aok-occur-select-prompt))
           (more (list (case choice
                         (?a nil)
                         (?t (read-string "Extension: "))
                         (?m (completing-read
                              "Mode: " (aok-get-major-mode-list)))
                         (t  t)))))
      (add-to-list 'more choice)
      (nreverse more))
    (occur-read-primary-args)))
  (let ((choice (cadr more))
        (morearg (car more)))
    (case choice
      (?a (all-occur  morearg regexp))
      (?t (type-occur morearg regexp))
      (?m (mode-occur morearg regexp))
      (t  (occur regexp)))))

(provide 'aok)
;;; aok.el ends here
