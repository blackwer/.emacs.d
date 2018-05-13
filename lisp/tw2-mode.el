;;; tw2-mode.el --- Twee2 major mode for sugarcube syntax highlighting  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Robert Blackwell

;; Author: Robert Blackwell <robert.blackwell@protonmail.com>
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
;;; I have no idea what's going on, man
;; 

;;; Code:

(require 'generic-x)
(define-generic-mode 'tw2-mode
  '(("/\*" . "\*/"))
  '("true" "false")
  '(("<<[^\s>]*" . 'font-lock-function-name-face)
    (">>" . 'font-lock-function-name-face)
    ("\$[^\s>\n]+" . 'font-lock-variable-name-face)
    ("\\(^\\|\s\\)_[^\s>\n]+" . 'font-lock-variable-name-face)
    ("[A-Za-z0-9]*::[^\n]*" . font-lock-function-name-face)
    ("\\[\\[.*\\]\\]" . font-lock-type-face)
    )
  '("\\.tw2$")
  nil
  "A mode for twee2 files")


(provide 'tw2-mode)
;;; tw2-mode.el ends here
