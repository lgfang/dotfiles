;;; flg-ansi-color-mode.el --- applies ansi-color on the visible area -*- lexical-binding: t -*-

;; Author: Fang Lungang <lungang.fang@gmail.org>
;; Created: Fang Lungang 2024-05-04
;; Updated: Fang Lungang 2024-08-23 +1000
;; Version: 0.0.1
;; Package-Requires: (ansi-color)
;; Keywords: comm processes terminals services

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; A very primitive minor mode to view log files containing ANSI color codes.

;; Pros: this minor mode runs `ansi-color-apply-on-region' lazily, i.e. only the
;; visible part of the buffer. Hence, it does NOT freeze Emacs even if the log
;; file is huge.

;; Cons: a) when the minor code is toggled off, it does not undo what has
;; already been ansi colorized. b) assumes the buffer content etc. does not
;; change. c) jumping to random places within the buffer may incur
;; incorrect/incomplete colorization.

;; Setup: copy the file into your load path and then add the following to your
;; `init.el' file.

;; (use-package ansi-color-mode
;;   :after ansi-color
;;   )

;; Either run `M-x ansi-color-mode' manually in designated buffers
;; or enable it via mode hooks so that you needn't enable it manually. Note, for compilation, ansu

;;; Code:

(require 'ansi-color)

(defun ansi-color-after-scroll (window start)
  "Apply ANSI color from the START to the end of the WINDOW.

This function is not run manually but added to the hook
`window-scroll-functions' when the `ansi-minor-mode' is turned
on."
  (let ((ansi-color-context-region nil))
    (ansi-color-apply-on-region start (window-end window t) t)))

(define-minor-mode lgf-ansi-color-mode
  "A simple minor mode that translates and render ANSI color codes."
  :global nil
  :lighter ""
  (if lgf-ansi-color-mode
      (progn
        (ansi-color-apply-on-region (window-start) (window-end) t)
        (add-hook 'window-scroll-functions 'ansi-color-after-scroll 80 t))
    (remove-hook 'window-scroll-functions 'ansi-color-after-scroll t)))

(provide 'flg-ansi-color-mode)
;;; lgf-ansi-color-mode.el ends here

;; Local Variables:
;; time-stamp-pattern:"8/Updated[:][ \t]+%U %Y-%02m-%02d %5z$"
;; End:
