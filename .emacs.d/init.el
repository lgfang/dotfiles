;;; lgfang.init.el --- my configuration file

;; Created:  Fang lungang 2004
;; Modified: Lungang FANG 10/18/2016 11:33>

;;; Commentary:

;; My Emacs configure

;;; Code:

;;; Paths

(defvar my-emacs-base
  (file-name-as-directory (expand-file-name "~/.emacs.d")))
(defvar my-extension-path
  (file-name-as-directory (expand-file-name "~/.emacs.d/emacs-extensions")))
(defvar my-elisp-path
  (file-name-as-directory (expand-file-name "~/.emacs.d/my-elisp")))
(defvar my-personal-path
  (file-name-as-directory (expand-file-name "~/mynotes/personal")))
(defvar my-backward-path
  (file-name-as-directory (concat my-extension-path "backward-compatibility")))

;; load path
(add-to-list 'load-path my-extension-path)
(add-to-list 'load-path my-elisp-path)
(add-to-list 'load-path my-backward-path t)

;; exec path
(cond
 ((eq system-type 'cygwin)
  (setq exec-path
        (append '("/cygdrive/c/Program Files/Mozilla Firefox") exec-path)))
 ((eq system-type 'darwin)
  ;; cocoa emacs does not inherit PATH from Terminal setting
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (setq exec-path (append '("/usr/local/bin") exec-path))))

;; ;; woman path
;; (setq woman-manpath '("patha" "pathb"))

;;; package

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-user-dir "~/.emacs.d/emacs-extensions/elpa")
(package-initialize)

;;; cygwin wrappers
;; NOTE: put this section in the begining of my configure so that files I hacked
;; can make used of these functions.

(defun cygpath (path)
  "Convert the PATH to  Java etc. does not read the cygwin PATH."
  (if (eq system-type 'cygwin)
      (substring ;; delete trailing new-line added
       (shell-command-to-string (format "cygpath -m \"%s\"" path)) 0 -1)
  path))

;;; Personal Info
;; Note: Some values are place-holders, will be overwritten by those in
;; 'my-confidential.el'.
(setq user-full-name "My name"
      user-mail-address "nospam@company.com"
      ange-ftp-default-user "myname"
      ;; url-proxy-services '(("http" . "localhost:8888"))
      disqus_shortname "mydisqus"
      google_analytic_track_id_blog "my-google-track-id"
      calendar-latitude 36.06
      calendar-longitude 120.27
      calendar-location-name "QingDao"
      cn-weather-city "青岛"
      ;; world time 'M-x display-time-world', /usr/share/zoneinfo
      display-time-world-list '(("Asia/Shanghai" "Qingdao")
                                ("US/Eastern" "Westford")
                                ("US/Central" "Indianhill")
                                ("Europe/Paris" "Paris")
                                ("Europe/Warsaw" "Poland")
                                ("Australia/Sydney" "Sydney")
                                ))
;; Actual values of confidential information are in this file
(load (concat my-personal-path "my-confidential") t nil nil)

;;; Language Environment - no longer needed

;; (if (eq system-type 'windows-nt)
;;     (if (>= emacs-major-version 23)
;;         (set-language-environment 'Chinese-GB18030)
;;       (set-language-environment 'Chinese-GB))
;;   (set-language-environment 'utf-8))

;;; global key bindings

(when (eq system-type 'darwin)          ; OSX

  ;; ;; Obsoleted, remap mackbook modifier keys globally instead of this
  ;; (setq mac-option-modifier 'ctrl mac-command-modifier 'meta)

  (unless (display-graphic-p)
    ;; In OSX terminal, trackpad gestures for up/down mapped to mouse-4/5
    (define-key global-map [mouse-4] '(lambda () (interactive) (scroll-down 1)))
    (define-key global-map [mouse-5] '(lambda () (interactive) (scroll-up 1)))))

;; F1-F12
(define-key global-map [f1] 'lgfang-recentf-open)
(define-key global-map [f2] 'ido-goto-symbol)
;; f3/f4: define keyboard macros
(define-key global-map [f5] 'whitespace-cleanup)
(define-key global-map [f7] 'flyspell-mode)
(define-key global-map [f8] 'flyspell-prog-mode)
(define-key global-map [f9] 'org-clock-in-last)
(define-key global-map [f10] 'org-capture)
;; f11 : reserved for twm/tmux etc.
;; f12 : reserved for twm/tmux etc.

;; C-, M-, C-M- ... :(
(define-key global-map (kbd "C-c p") 'flymake-goto-prev-error)
(define-key global-map (kbd "C-c n") 'flymake-goto-next-error)
(define-key global-map (kbd "C-c m") 'flymake-display-err-menu-for-current-line)
(define-key global-map (kbd "C-x C-b") 'ibuffer)
(define-key global-map (kbd "C-x c l") 'org-store-link)
(define-key global-map (kbd "C-x c a") 'org-agenda)
(define-key global-map (kbd "C-x c o") 'org-open-at-point-global)
(define-key global-map (kbd "M-/") 'hippie-expand)
(define-key global-map (kbd "M-,") 'hs-toggle-hiding)
(define-key global-map (kbd "M-.") 'lgfang-toggle-selective-display)
(define-key global-map (kbd "M-g c") 'move-to-column)
(define-key global-map (kbd "M-g ]") 'lgfang-goto-page)
(define-key global-map (kbd "C-h d") 'sdcv-search-pointer)
(define-key global-map (kbd "C-h D") 'sdcv-search-pointer+)

;; Split & Resize Windows
(define-key global-map (kbd "C-x |") 'split-window-horizontally)
(define-key global-map (kbd "C-x _") 'split-window-vertically)
(define-key global-map (kbd "C-{") 'shrink-window-horizontally)
(define-key global-map (kbd "C-}") 'enlarge-window-horizontally)
(define-key global-map (kbd "C-^") 'enlarge-window)

;; Navgate using windmove
(define-key global-map (kbd "C-<up>"   ) 'windmove-up)
(define-key global-map (kbd "C-<down>" ) 'windmove-down)
(define-key global-map (kbd "C-<right>" ) 'windmove-right)
(define-key global-map (kbd "C-<left>") 'windmove-left)

;; Swap buffers: M-<up> etc.
(define-key global-map (kbd "M-<up>"   ) 'buf-move-up)
(define-key global-map (kbd "M-<down>" ) 'buf-move-down)
(define-key global-map (kbd "M-<right>") 'buf-move-right)
(define-key global-map (kbd "M-<left>" ) 'buf-move-left)

;; Change Layout
(define-key global-map (kbd "C-\\") 'tiling-cycle)
(define-key global-map (kbd "S-C-<up>") 'tiling-tile-up)
(define-key global-map (kbd "S-C-<down>") 'tiling-tile-down)
(define-key global-map (kbd "S-C-<right>") 'tiling-tile-right)
(define-key global-map (kbd "S-C-<left>") 'tiling-tile-left)
;; Another type of representation of same keys, in case your terminal
;; doesn't recognize above key-binding. Tip: C-h k C-up etc. to see into
;; what your terminal tranlated the key sequence.
(define-key global-map (kbd "M-[ a"     ) 'windmove-up)
(define-key global-map (kbd "M-[ b"     ) 'windmove-down)
(define-key global-map (kbd "M-[ c"     ) 'windmove-right)
(define-key global-map (kbd "M-[ d"     ) 'windmove-left)
(define-key global-map (kbd "ESC <up>"   ) 'buf-move-up)
(define-key global-map (kbd "ESC <down>" ) 'buf-move-down)
(define-key global-map (kbd "ESC <right>") 'buf-move-right)
(define-key global-map (kbd "ESC <left>" ) 'buf-move-left)
(define-key global-map (kbd "ESC M-[ a" ) 'tiling-tile-up)
(define-key global-map (kbd "ESC M-[ b" ) 'tiling-tile-down)
(define-key global-map (kbd "ESC M-[ c" ) 'tiling-tile-right)
(define-key global-map (kbd "ESC M-[ d" ) 'tiling-tile-left)

;;; abbrev - conflicts with auto-complete-mode, use yasnippet instead.

;;; ascii mode
(autoload 'ascii-display "ascii" "Toggle ASCII code display." t)

;;; asm mode
(setq-default asm-comment-char 35)      ; 35 -> ascii code for '#'

;;; auto-complete
(let ((my-path-to-auto-complete (concat my-extension-path "auto-complete")))
  (add-to-list 'load-path my-path-to-auto-complete)
  (when (require 'auto-complete-config nil t)
    (add-to-list 'ac-dictionary-directories
                 (concat my-path-to-auto-complete "/dict"))
    (add-to-list 'ac-dictionary-directories
                 (concat my-extension-path "/my-ac-dict"))
    (add-to-list 'ac-sources ac-source-yasnippet)
    (define-key ac-complete-mode-map "\C-s" 'ac-next)
    (define-key ac-complete-mode-map "\C-r" 'ac-previous)
    ;; (setq-default ac-sources ac-sources) ; works, though tricky
    ;; ;; common source
    ;; (setq-default ac-sources '(ac-source-imenu ac-source-abbrev
    ;;               ac-source-words-in-buffer
    ;;               ac-source-files-in-current-dir ac-source-filename
    ;;               ))
    (ac-config-default)
    (setq ac-dwim t
          ac-auto-start 2               ; start ac after 3 chars
          ;; modes that automatically startup auto-complete-mode
          ac-modes '(asm-mode
                     c++-mode ;; c-mode
                     cc-mode
                     emacs-lisp-mode
                     java-mode
                     lisp-interaction-mode
                     lisp-mode
                     makefile-mode
                     makefile-gmake-mode
                     org-mode
                     cperl-mode
                     python-mode
                     sh-mode
                     tcl-mode
                     org-mode
                     text-mode
                     conf-mode))
    ;; (global-set-key "\M-/" 'ac-start)
    (dolist (hook '(emacs-lisp-mode-hook
                    lisp-interaction-mode-hook
                    shell-mode-hook
                    tcl-mode-hook))
      (add-hook hook (lambda()
                       (add-to-list 'ac-sources ac-source-filename))))))

;;; auto insert
(setq auto-insert t
      auto-insert-directory (concat my-emacs-base "auto-insert/"))
(add-hook 'find-file-hooks 'auto-insert)

;;; auto mode list
(setq auto-mode-alist (append
                       '(("\\.[xX]\\'" . c-mode)
                         ("\\.mak\\'" . makefile-mode)
                         ("\\.make\\'" . makefile-mode)
                         ("\\.gdb\\'" . gdb-script-mode)
                         ("\\.v\\'" . verilog-mode)
                         ("\\.ldif\\'" . ldap-mode))
                       auto-mode-alist))

;;; auto-revert when file modified by other
(global-auto-revert-mode t)

;;; backup files
(setq make-backup-files t
      version-control 'never
      backup-by-copying-when-linked t)

;;; bbdb & bbdb-vcard-export
(add-to-list 'load-path (concat my-extension-path "bbdb/lisp"))
(when (require 'bbdb nil t)
  (require 'qp)
  (bbdb-initialize 'gnus 'message)
  (setq bbdb-default-area-code 532
        bbdb-default-country "China"
        bbdb-file (concat my-personal-path "my-bbdb")
        bbdb-north-american-phone-numbers-p nil
        bbdb-user-mail-names (regexp-opt
                              '("myname@163.com" "another@smth.org"))
        bbdb-complete-name-allow-cycling t
        bbdb-use-pop-up nil)
  ;;  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gns)

  (if (require 'bbdb-vcard nil t)
      (setq bbdb-vcard-export-coding-system 'utf-8-unix))

  (defun lgfang-bbdb-to-vcards()
    "Adapted to nokia E72"
    (interactive)
    (let ((path "~/tmp/exported-vcards/"))
      (bbdb "" nil)                       ; have to run this first
      (bbdb-vcard-export path t t)
      (mapc (lambda(file)
              (with-temp-buffer
                (insert-file-contents-literally file)
                (quoted-printable-encode-region (point-min) (point-max))
                ;; delete useless field
                (replace-regexp "^\\(FN\\|NICKNAME\\):.*$" ""
                                nil (point-min) (point-max))
                ;; corrections of quoted-printable-encode-region
                (replace-regexp "^VERSION:3.0" "VERSION:2.1"
                                nil (point-min) (point-max))
                (replace-string ";TYPE=3D" ";"
                                nil (point-min) (point-max))
                (replace-regexp "^EMAIL:" "EMAIL;INTERNET:"
                                nil (point-min) (point-max))
                (replace-regexp "^\\(ADR[^:]*\\|N\\|ORG\\):"
                                "\\1;CHARSET=UTF-8;ENCODING=QUOTED-PRINTABLE:"
                                nil (point-min) (point-max))

                (write-region (point-min) (point-max) file nil)))
            (file-expand-wildcards (concat path "*.vcf"))))))

;;; bookmark+ - breaks org-mode + flyspell

(when (require 'browse-kill-ring nil t)
  (browse-kill-ring-default-keybindings))

(unless (eq system-type 'darwin)          ; OSX
  (setq browse-url-browser-function 'browse-url-firefox))

(require 'buffer-move nil t)

;;; c mode configuration
(defconst lgfang-c-style
  '((c-tab-always-indent        . t)
    (c-basic-offset . 4)
    (c-ignore-auto-fill . nil)
    (c-comment-only-line-offset . (0 . 0))
    (c-hanging-braces-alist     . ((substatement-open after before)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist . ((knr-argdecl-intro . 5)
                        (arglist-intro . +)
                        (arglist-close . c-lineup-close-paren)
                        (inclass . +)
                        (member-init-intro . +)
                        (statement-block-intro . +)
                        (defun-block-intro . +)
                        (substatement-open . 0)
                        (label . 0)
                        (statement-case-open . +)
                        (statement-case-intro . +)
                        (case-label . 0)
                        (statement-cont . c-lineup-math)
                        (inline-open . 0)
                        (brace-list-open . +)
                        (topmost-intro-cont . 0)
                        (c . 1) ; "c" for continue of comment, not "c
                                ; programming language"
                        ))
    (c-special-indent-hook . c-gnu-impose-minimum)
    (c-block-comment-prefix . "lgf: ")
    (c-comment-prefix-regexp . ((awk-mode . "#+(lgf: )?")
                                (other ."lgf: \\|//+\\|\\**")))
    ;; go to this file and test if c block comments works
    ;; [[file:./patches/comments-test.c]]
    (c-echo-syntactic-information-p . t))
  "lgfang's C Programming Style")
(c-add-style "lgfang" lgfang-c-style nil)

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "lgfang")
            (c-toggle-hungry-state 1)
            (hs-minor-mode 1)
            (turn-on-cwarn-mode)
            ;; (eldoc-mode 1)
            ))
;;; Can't hook imenu-add-menubar-index to c-mode-common-hook since awk
;;; mode don't support it
(dolist (hook '(c-mode-hook c++-mode-hook java-mode-hook))
  (add-hook hook 'imenu-add-menubar-index))

;; calendar for Chinese
(when (require 'cal-china-x nil t)
  (setq mark-holidays-in-calendar t
        ;; calendar-chinese-all-holidays-flag t
        calendar-holidays
        (append cal-china-x-chinese-holidays
                holiday-christian-holidays)))

;;; clipboard
(defun lgfang-send-to-tmux ()
  "Send content of active region or HEAD of the kill-ring to
tmux's buffer"
  (interactive)
  (let ((file (make-temp-file "/tmp/emacs-to-tmux.clip")))
    (if (region-active-p) (kill-ring-save (region-beginning) (region-end)))
    (with-temp-file file (insert-for-yank (current-kill 0)))
    (call-process "tmux" nil nil nil "load-buffer" file)
    (delete-file file)))

(defun lgfang-get-from-tmux ()
  "Get current tmux buffer"
  (interactive)
  (call-process "tmux" nil t nil "show-buffer"))

;; aliases to type less characters
(fset 'to-tmux 'lgfang-send-to-tmux)
(fset 'from-tmux 'lgfang-get-from-tmux)

;;; color theme
(add-to-list 'custom-theme-load-path (concat my-extension-path "themes"))
(if (display-graphic-p) (load-theme 'wombat t) (load-theme 'wombat t))
;; Themes I recommend: wombat, soloarized-dark, tango-dark, tango, gnome2. For
;; themes ported to emacs24: https://github.com/emacs-jp/replace-colorthemes

(column-number-mode t)

(setq comment-style 'extra-line)

;;; compilation
(eval-after-load "compile"
  '(progn
     (setq compile-command "clang++ --std=c++11 "
           ;; compile-command "python -m unittest "
           compilation-scroll-output t)
     (define-key compilation-mode-map "n" 'next-error-no-select)
     (define-key compilation-mode-map "p" 'previous-error-no-select)
     (define-key compilation-mode-map " "
       (lambda () (interactive)
         (save-selected-window (compile-goto-error))))
     (define-key compilation-mode-map [return] 'compile-goto-error)
     (define-key compilation-mode-map "o"
       (lambda () (interactive)
         (compile-goto-error) (delete-other-windows)))
     (define-key compilation-mode-map "q" 'quit-window)))

;;; ccrypt: auto encrypt/decrypt files using ccrypt
(require 'ps-ccrypt nil t)

;;; delete selection typed text replaces the selection (marked region)
;; (delete-selection-mode 0)

;;; default major mode
;; (setq default-major-mode 'text-mode)

;;; desktop save, use savehist etc. instead
(desktop-save-mode -1)

;;; dired etc.
(setq dired-recursive-copies 'top dired-recursive-deletes 'top)
(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
;;; guess of '!' action
(add-to-list 'dired-guess-shell-alist-user
             (list "\\.\\(avi\\|mkv\\|mpg\\|rmvb\\|rm\\)\\'" "mplayer"))
(add-to-list 'dired-guess-shell-alist-user (list "\\.\\(rar\\)\\'" "7z x"))
(when (require 'dired-details nil t) (dired-details-install))

;;; ediff
(setq
 ;; ediff-diff-options "-w"
 ;; do not pop a frame for ediff
 ediff-window-setup-function 'ediff-setup-windows-plain
 ;; my screen is large enough
 ediff-split-window-function 'split-window-sensibly)

;;; elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda() (imenu-add-menubar-index) (hs-minor-mode 1)))

;; TODO:

;;; emms configure in another file
(load "lgfang.emms" t nil nil)

;;; eshell: restore arrows(up/down) to their orginal functions
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map [up] 'previous-line)
            (define-key eshell-mode-map [down] 'next-line)))
;; multi-eshell
(when (require 'multi-eshell nil t)
  (setq multi-eshell-name "*eshell*")
  (setq multi-eshell-shell-function (quote (eshell))))
;; commands for eshell
(defun eshell/ep ()
  "In eshell, `ep' to go to the path of the previous buffer"
  (cd (with-current-buffer (other-buffer) default-directory)))
(defun eshell/vi (&rest args)
  ;; from http://www.emacswiki.org/emacs/EshellFunctions
  "Invoke `find-file' on the file.
    \"vi +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (goto-line line))
      (find-file (pop args)))))

;;; face, add our own keywords. ctypes.el is too heavy-weight
(add-hook 'find-file-hooks
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(lgfang\\|TODO\\|FIXME\\|NOTE\\|IMPORTANT\\):"
                    . (0 font-lock-warning-face t))))))
(font-lock-add-keywords 'c-mode         ; for c mode only
                        '(("\\<\\(TRUE\\|FALSE\\)\\>"
                           . font-lock-constant-face)))

(require 'ffap)
;; (ffap-bindings) ; Don't bind to `C-x C-f' etc., explicitly `M-x ffap' etc.
;; (setq ffap-c-path (append ffap-c-path sourcepair-header-path))

;;; fill column
(setq-default fill-column 80 comment-fill-column 72)
(require 'fill-column-indicator nil t) ;; run "(fci-mode)"
;; (define-globalized-minor-mode
;;   global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode t)

;;; flymake & flycheck (Prefer flycheck when possible)
(if (require 'flycheck nil t)

    (progn
      ;; (setq-default flycheck-sh-shellcheck-executable "/path/to/shellcheck")
      (add-hook 'c++-mode-hook
                (lambda ()
                  (setq
                   ;; Depends on the compiler available, one of the two takes
                   ;; effect. But, setting both does not hurt.
                   flycheck-clang-language-standard "c++11"
                   flycheck-gcc-language-standard "c++11")))
      (global-flycheck-mode 1))

  (require 'flymake)
  (setq flymake-no-changes-timeout 2    ; don't grab too much cpu time
        flymake-allowed-file-name-masks
        (cons '("\\.cc\\'" flymake-simple-make-init) ; C++ source file
              ;; remember to add target in makefile
              ;; check-syntax:
              ;;       g++/gcc -o nul -Wall -S $(CHK_SOURCES)
              flymake-allowed-file-name-masks)))

;;; fonts
(when (and (>= emacs-major-version 23) window-system)

  ;; Recommended English fonts: "consolas", "DejaVu Sans Mono", "monofur"
  (set-face-attribute 'default nil :font "monaco-12:weight=normal")

  ;; Recommended Chinese fonts: "SimSun", "Microsoft YaHei", "WenQuanYi
  ;; Micro Hei Mono"
  (let ((zh-font-family "SimSun"))
    ;; Set scale of zh font so that width of one chinese char equals that of two
    ;; english chars. (Windows Emacs has a bug in "scale", hardcode zh font size
    ;; instead)
    (if (eq window-system 'w32)
        (dolist (each '(han cjk-misc))
          (set-fontset-font nil each
                            (font-spec :family zh-font-family :size 22)))
      (dolist (each '(han cjk-misc )) ;include kana, bopomofo, symbol?
        (set-fontset-font nil each
                          (font-spec :family zh-font-family)))
      (setq face-font-rescale-alist (list (cons zh-font-family 1.2)))))

  ;; Resize using mouse wheel
  (let ()
    (if (eq window-system 'w32)
        (setq up (kbd "<C-wheel-up>") down (kbd "<C-wheel-down>"))
      (setq up (kbd "<C-mouse-4>") down (kbd "<C-mouse-5>")))
    (define-key global-map up 'text-scale-increase)
    (define-key global-map down 'text-scale-decrease))
  )

;;; frame position/size; emacs22 or later supports 'emacs --fullscreen'
;; (when (> emacs-major-version 23)
;;   (setq init-frame-alist
;;         '((top . 1) (left . 1) (width . 80) (height . 24)
;;            ;; (alpha 90 50)                 ;transparency
;;           )))

;;; frame title: hostname:current file name
(setq frame-title-format
      (list (replace-regexp-in-string "\\..*$" "" system-name)
            ":" '(buffer-file-name
                  "%f" (dired-directory dired-directory "%b"))))

;;; gdb
;; (setq gdb-many-windows t)

;;; hide-ifdef-mode settings
(require 'hideif)

(defun lgfang-hide-if-0()
  "hide #if 0 blocks, inspired by internet."
  (interactive)
  (require 'hideif)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#if[ \t]*0" nil t)
      (hide-ifdef-block))))

(defun hif-overlay-at (position)
  "An imitation of the one in hide-show, used by
lgfang-hif-toggle-block"
  (let ((overlays (overlays-at position)) ov found)
    (while (and (not found) (setq ov (car overlays)))
      (setq found (eq (overlay-get ov 'invisible) 'hide-ifdef)
            overlays (cdr overlays)))
    found))

(defun lgfang-hif-toggle-block ()
  "toggle hide/show-ifdef-block"
  (interactive)
  (require 'hideif)
  (let* ((top-bottom (hif-find-ifdef-block)) (top (car top-bottom)))
    (goto-char top)
    (hif-end-of-line)
    (if (hif-overlay-at (point)) (show-ifdef-block)
      (hide-ifdef-block))))

(defun lgfang-hide-ifdef-use-define-alist (name)
  "A simple wrapper for `hide-ifdef-use-define-alist'"
  (interactive
   (list (let* ((prompt "Use MACRO define list: ")
                (symbol-names
                 (mapcar (lambda (a) (symbol-name (car a)))
                         hide-ifdef-define-alist)))
           (if (require 'ido nil t)
               (ido-completing-read prompt symbol-names)
             (completing-read prompt symbol-names)))))
  (setq hide-ifdef-initially t    ; also apply to buffers not opened yet
        my-define-alist name)
  (hide-ifdefs)                         ; for current file
  (hide-ifdef-use-define-alist name))

(setq hide-ifdef-initially nil)
(defvar my-define-alist nil)
(add-hook 'c-mode-hook
          (lambda ()
            (hide-ifdef-mode 1)
            (lgfang-hide-if-0)
            (when my-define-alist
              (hide-ifdef-use-define-alist my-define-alist))))
(eval-after-load "cc-mode"
  '(define-key c-mode-base-map (kbd "M-'") 'lgfang-hif-toggle-block))

;;; hide-show
(setq hs-allow-nesting t hs-isearch-open t)

;;; highlight-tail
;; (when (require 'highlight-tail)
;;   (setq highlight-tail-colors '(("black" . 0)
;;                                 ("#bc2525" . 25)
;;                                 ("black" . 66))
;;         highlight-tail-steps 14
;;         highlight-tail-timer 1
;;         highlight-tail-posterior-type 'const)
;;   (highlight-tail-mode 1))

;;; hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(require 'htmlize nil t)

;;; ido `C-r/C-s' for ido-next/previous-match, `C-f' to get out ido mode into
;;; "normal" find file mode
(ido-mode 1)
(setq ido-enable-flex-matching t)

;;; imenu
(setq imenu-sort-function 'imenu--sort-by-name
      imenu-auto-rescan t
      imenu-use-popup-menu 'on-mouse)

(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a
symbol to navigate to.  From emacswiki, by shjk"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '()) (symbol-names '()))
    (flet
        ((addsymbols
          (symbol-list)
          (when (listp symbol-list)
            (dolist (symbol symbol-list)
              (let ((name nil) (position nil))
                (cond
                 ((and (listp symbol) (imenu--subalist-p symbol))
                  (addsymbols symbol))

                 ((listp symbol)
                  (setq name (car symbol))
                  (setq position (cdr symbol)))

                 ((stringp symbol)
                  (setq name symbol)
                  (setq position
                        (get-text-property
                         1 'org-imenu-marker symbol))))

                (unless (or (null position) (null name))
                  (add-to-list 'symbol-names name)
                  (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
      (let* ((selected-symbol
              (ido-completing-read "jump to: " symbol-names nil nil
                                   (thing-at-point 'symbol)))
             (position (cdr (assoc selected-symbol name-and-pos))))
        (push-mark)
        (goto-char position))))

;;; init
(setq inhibit-startup-message t
      inhibit-splash-screen t)
(eval-after-load "outline"
  '(setq initial-scratch-message
         (if (file-exists-p "~/.tod.org")
             (with-temp-buffer
               (insert-file-contents "~/.tod.org")
               ;; jump to somewhere randomly, must after first heading
               (outline-next-heading)
               ;; set random seed, otherwise "emacs -nw" always get same
               ;; tip
               (random t)
               (goto-char (+ (random (- (point-max) (point))) (point)))
               (outline-mark-subtree)
               (let* ((beg (point))
                      (end (mark)))
                 (buffer-substring beg end)))
           "Hello My Buddy,\n
You may want to get a copy of 'Tip Of the Day'.\n
lgfang")))

;;; ispell - aspell instead
(setq ispell-program-name "aspell"
      ;; regardless locale settings, always use english refer to
      ;; ispell-dictionary-alist for details
      ispell-dictionary "english")

;;; kill/copy current line
;;; from http://blog.waterlin.org
(defadvice kill-ring-save (before slickcopy activate compile)
  "If region not active, copy current line."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "If region not active, kill current line."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2)))))

;;; ldap mode for ldif files
(autoload 'ldap-mode "ldap-mode" "Edit ldif files" t)

;;; linum
(when (not (window-system))
  (setq linum-format
        (lambda (linum)
          (propertize
           (format
            (concat
             "%"
             (number-to-string
              (length (number-to-string
                       (count-lines (point-min) (point-max)))))
             "d| ") linum) 'face 'linum))))
(require 'linum nil t)
;;(global-linum-mode t)

(autoload 'log-mode "log-mode" "my mode to view log files" t)
(add-to-list 'auto-mode-alist '("\\.log\\(\\.[0-9]+\\)?\\'" . log-mode) t)

;;; long lines
(setq
 longlines-wrap-follows-window-size t
 ;; for visual-line-mode, indicates lines are wrapped
 visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;;; markdown; remember to package-install RET markdown-mode
;; The "standard" markdown command line tool is not good enough
(setq markdown-command "/usr/local/bin/pandoc")
(add-hook 'markdown-mode-hook 'flyspell-mode)

(menu-bar-mode -1)

(setq messages-buffer-max-lines 500)    ; default value too small

(when (require 'midnight nil t)
  (midnight-delay-set 'midnight-delay "1:30am")
  (setq clean-buffer-list-delay-general 1))

;;; mouse
(setq mouse-yank-at-point t             ; instead of at mouse cursor
      mouse-drag-copy-region t)

(when (not window-system) (xterm-mouse-mode 1)) ;use mouse in xterm

;;; nXML mode
(add-to-list 'auto-mode-alist
             '("\\.\\((xml\\|xsd\\|sch\\|rng\\|xslt\\|svg\\|rss\\)\\'"
               . nxml-mode))
(setq magic-mode-alist
      (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
(fset 'html-mode 'nxml-mode)
(require 'rng-loc nil t)

(add-hook 'nxml-mode-hook (lambda() (hs-minor-mode 1)))

(add-to-list 'rng-schema-locating-files
             "~/mynotes/emacs/schema-locations.xml")

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               ;; "<!--\\|<[^/>]*[^/]>" ;; regexp for start block
               ;; "-->\\|</[^/>]*[^/]>" ;; regexp for end block
               "<!--\\|<[^/>][^>]*[^/]>" ;; our xml has names like calea/li
               "-->\\|</[^/>][^>]*[^/]>"
               "<!--" ;; regexp for comment start. (need this??)
               nxml-forward-element
               nil))

(defun lgfang-toggle-level ()
  "mainly to be used in nxml mode"
  (interactive) (hs-show-block) (hs-hide-level 1))
(eval-after-load "nxml-mode"
  '(progn
     (define-key nxml-mode-map (kbd "M-'") 'lgfang-toggle-level)
     (define-key nxml-mode-map [mouse-3] 'lgfang-toggle-level)))

(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a
path. from http://www.emacswiki.org/emacs/NxmlMode"
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while
            (and (< (point-min) (point)) ;; Doesn't error if point is at
                                         ;; beginning of buffer
                 (condition-case nil
                     (progn
                       (nxml-backward-up-element) ; always returns nil
                       t)
                   (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))

(require 'auto-complete-nxml nil t)

;;; occur
(define-key occur-mode-map "n" 'next-error-no-select)
(define-key occur-mode-map "p" 'previous-error-no-select)
(define-key occur-mode-map " " 'occur-mode-display-occurrence)
(define-key occur-mode-map "o" (lambda () (interactive)
                                 (occur-mode-goto-occurrence)
                                 (delete-other-windows)))

;;; org mode
(load "~/.org" t nil nil)

;;; parenthesis highlighting, Use highlight-parentheses instead
;; (show-paren-mode nil)(setq show-paren-style 'expression)
(when (require 'highlight-parentheses nil t)
  ;; M-x list-colors-display to see named colors
  (setq hl-paren-colors '("brown" "orange" "yellow" "forest green"
                          "cyan" "blue" "violet"))
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode
    (lambda ()
      (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t))

;;; perl: using cperl-mode instead
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook 'imenu-add-menubar-index)
(setq cperl-indent-level 4
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)

;;; Python related

;;; Python mode hook
(add-hook 'python-mode-hook
          (lambda()
            (hs-minor-mode 1)
            (outline-minor-mode 1)
            (setq imenu-create-index-function 'python-imenu-create-flat-index)
            (imenu-add-menubar-index)))

(when (functionp 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(require 'pydoc-info nil t)

;;; pylint
(unless (or (> emacs-major-version 24)
            (and (= emacs-major-version 24) (>= emacs-minor-version 3)))
  ;; for emacs24.3 or above, use flycheck, which does not require explicit
  ;; configuration

  (load "pylint" t nil nil)
  (load "pylint-flymake" t nil nil)
  (require 'cl)
  ;; From http://docs.pylint.org/ide-integration
  (defun show-fly-err-at-point ()
    "If the cursor is sitting on a flymake error, display the
message in the minibuffer"
    (interactive)
    (let ((line-no (line-number-at-pos)))
      (dolist (elem flymake-err-info)
        (if (eq (car elem) line-no)
            (let ((err (car (second elem))))
              (message "%s" (flymake-ler-text err)))))))
  (add-hook 'post-command-hook 'show-fly-err-at-point))

;;; ropemacs and pymacs
(setq ropemacs-enable-shortcuts t
      ropemacs-global-prefix nil        ; the default one already used
      ropemacs-guess-project t
      ropemacs-confirm-saving 'nil)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")
(defun load-ropemacs ()
  (interactive)
  (when (and (require 'pymacs nil t)
             (pymacs-load "ropemacs" "rope-" t))
    ;; (ac-ropemacs-setup)       ; this does not work?
    (define-key python-mode-map (kbd "M-/") 'rope-code-assist)))

;; PythonTidy: download and put it into PATH as "pythontidy"
(defun lgfang-python-tidy ()
  (interactive)
  (if (not (string= mode-name "Python"))
      (message "Buffer is not python mode")
    (let ((beg (if (region-active-p) (region-beginning) (point-min)))
          (end (if (region-active-p) (region-end) (point-max))))
      (save-excursion
        (shell-command-on-region beg end "pythontidy" nil t)))))

;;; recently opened file
(require 'recentf)
;; add at the front of list, don't conncect to remote hosts
(add-to-list 'recentf-keep 'file-remote-p)
(setq recentf-max-saved-items 666)
(recentf-mode 1)

;;; Always end a file with a newline
(setq require-final-newline t)

;;; rfcview
(add-to-list 'auto-mode-alist
             '("/\\(rfc[0-9]+\\|draft-.+\\)\\.txt\\(\\.gz\\)?\\'"
               . rfcview-mode))
(autoload 'rfcview-mode "rfcview")
;; ffap try find RFCs in ffap-rfc-directories before downloading
(setq ffap-rfc-directories '("~/rfc" "~/projects/rfc"))

;;; rnc mode - nxml mode uses rnc files
(add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode))
(autoload 'rnc-mode "rnc-mode")
(setq rnc-enable-imenu t
      rnc-jing-jar-file (expand-file-name
                         (concat my-extension-path "jing/bin/jing.jar")))

(when (require 'flycheck nil t)
  (flycheck-define-checker rnc
    "Check rnc files using jing.jar

See URL `https://github.com/TreeRex/rnc-mode' and
`http://www.thaiopensource.com/relaxng/jing.html'"
    :command ("java" "-jar" (eval (cygpath rnc-jing-jar-file)) "-c"
              (eval (cygpath (flycheck-save-buffer-to-temp
                              #'flycheck-temp-file-system "flycheck"))))
    :error-patterns
    ((error line-start (zero-or-more anything) ":" line ":"
            column ": error:" (message) line-end)) :modes rnc-mode)

  (add-to-list 'flycheck-checkers 'rnc))
;; ;; Had not been the java, it could be this:
;; (flycheck-define-checker rnc
;;   :command ("java" "-jar" (eval rnc-jing-jar-file) "-c" source)
;;   :error-patterns ((error line-start (file-name) ":" line ":"
;;   column (message) line-end)) :modes rnc-mode)

(defun rnc2rng ()
  (interactive)
  (let* ((rnc (buffer-file-name))
         (rng (concat (file-name-sans-extension rnc) ".rng")))
    (call-process "java" nil nil nil "-jar"
                  (cygpath
                   (expand-file-name
                    (concat my-extension-path "trang.jar")))
                  (cygpath rnc) (cygpath rng))))

;;; save minibuffer history and place of cursor between sessions
(savehist-mode t)
(setq-default save-place t)
(require 'saveplace)

(setq scroll-margin 0 scroll-conservatively 100) ;  scroll-step ?
(when (and (> emacs-major-version 21) window-system)
  (set-scroll-bar-mode nil))

(when (require 'sdcv nil t)
  (setq sdcv-dictionary-simple-list
        '(
          "牛津现代英汉双解词典"
          "朗道英汉字典5.0"
          "朗道汉英字典5.0"
          )
        sdcv-dictionary-complete-list nil ; use all available dicts
        ))

;;; selective display
(defun lgfang-toggle-selective-display()
  "set-selective-display to current column or toggle
selective-display"
  (interactive)
  (let ((arg (progn (back-to-indentation) (1+ (current-column)))))
    (set-selective-display (if (eq arg selective-display) nil arg))))

;;; sentence end
(setq sentence-end-double-space nil)
;; (setq sentence-end
;;       "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

;;; server (alternatively, you may use "emacs --daemon")
(require 'server)
(when (not (server-running-p))
  (server-start))

;;; sh-mode-hook. Note that mode for shell script is sh-mode, NOT shell-mode
(setq my-sh-imenu-generic-expression
      '((nil "^\\s-*\\(function\\s-+\\)?\\([A-Za-z_][A-Za-z_0-9]+\\)\\s-*()" 2)
        (nil "^\\s-*function\\s-+\\([A-Za-z_][A-Za-z_0-9]+\\)" 1)))
(add-hook 'sh-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (setq imenu-generic-expression
                  my-sh-imenu-generic-expression
                  outline-regexp "# [*\\f]+")
            (imenu-add-menubar-index)))

;;; sh-mode, the mode for shell scripts
(when (eq system-type 'gnu/linux)
  (setq sh-alias-alist
        '((csh . tcsh)
          (ksh . ksh88)                 ; flycheck doesn't handle pdksh
          (bash2 . bash)
          (sh5 . sh))))

;;; skeleton, use yasnippet instead
;; avoid skeleton/abbrev recursion
(setq-default skeleton-further-elements '((abbrev-mode nil)))
;; skeleton-pair-insert
(setq
 ;; turn on/off skeleton-pair-insert
 skeleton-pair nil
 ;; inhibit paired insertion before/inside a word
 skeleton-pair-on-word nil)
(when skeleton-pair                     ; if turned on
  (dolist (hook '(c-mode-common-hook
                  tcl-mode-hook org-mode-hook
                  latex-mode-hook nxml-mode-hook
                  rnc-mode-hook))
    (add-hook
     hook
     (lambda ()
       (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
       (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
       (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
       (local-set-key (kbd "'") 'skeleton-pair-insert-maybe)
       (local-set-key (kbd "`") 'skeleton-pair-insert-maybe)
       (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)))))
;; (load "lgfang-skeleton" t nil nil)

;;; split horizontally if screen wide enough
(setq split-width-threshold 150)

;;; subword-mode
(global-subword-mode)

;;; Tabbar
;; (when (and window-system (require 'tabbar nil t)) (tabbar-mode 1))

;;; Tcl & expect
(add-hook 'tcl-mode-hook
          (lambda ()
            (imenu-add-menubar-index)
            (hs-minor-mode 1)))
(add-to-list 'interpreter-mode-alist '("expect" . tcl-mode))

;;; thingatpt+ - breaks org-mode + flyspell
;; (eval-after-load "thingatpt" '(require 'thingatpt+))

;;; Tiling
(require 'tiling nil t)

;;; time stamp
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-format "%U %02m/%02d/%:y %02H:%02M"
      time-stamp-start "\\(Modified\\|last-edit\\): *\\\\?"
      time-stamp-end "\\\\?>"
      ;; no Chinese chars in time stamps even in Chinese locale.
      system-time-locale "C")

;;; toggle-window-dedicated.el
(load "toggle-window-dedicated" t nil nil)

;;; tool-bar - I need no tool bar
(tool-bar-mode -1)

;;; tramp
(require 'tramp)
(setq tramp-debug-buffer t)
(add-to-list 'tramp-default-method-alist '("localhost" nil "su"))

;;; highlight selected region
(setq-default transient-mark-mode t)

;;; trash
(when (>= emacs-major-version 23)
  (setq delete-by-moving-to-trash nil)
  ;; works for *nix only
  (setq trash-directory "~/.trashbin"))

(setq-default truncate-lines nil)

;;; Uniquify buffer name with more meaningful names
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-strip-common-suffix t
        uniquify-separator "@"))

;;; verilog mode
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )

;;; version control
(load "lgfang.vc" t nil nil)
(add-to-list 'load-path (concat my-extension-path "vc-clearcase"))
(load "vc-clearcase-auto" t nil nil)
(setq clearcase-use-external-diff t             ; the internal one sucks
      ;; vc-clearcase-diff-switches "-diff_format" ; if diff unavailable
      ;; vc-git-diff-switches "-w"         ; ignore diff of whitespace
      )

;;; vimrc mode
(autoload 'vimrc-mode "vimrc-mode")
(add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))

;;; viper, those who miss vi so bad please change "nil" to "t"
(when nil
  (setq viper-inhibit-startup-message t
        viper-expert-level '5
        viper-mode t)
  (require 'viper))

;;; Weather
(require 'cn-weather nil t)

(which-function-mode t)

;;; whitespace related
;; (setq-default show-trailing-whitespace t) ; use whitespace mode instead
(setq-default indent-tabs-mode nil ; inserts space instead of <tab> when indent
              tab-stop-list '(4) ; starts at 4th column, each stop + tab-width
              tab-width 4)
(setq whitespace-line-column nil
      whitespace-style '(face
                         trailing
                         tabs
                         indentation
                         space-before-tab
                         space-after-tab
                         lines-tail
                         empty))
;; Editting others' files with whitespace-mode turned on can be boresome as they
;; never clean up whitespace. Therefore, M-x whitespace-mode only when needed.
(global-whitespace-mode 0)

;;; windmove - autoloaded

;;; Winner-mode - autoloaded
(winner-mode 1)

;;; woman
(setq woman-use-own-frame nil
      woman-fill-frame t)

;;; word
(setq-default word-wrap t)

;;; to make the cursor as wide as the character it is over
(setq x-stretch-cursor t)

;;; xcscope for cscope
(when (require 'xcscope nil t)
  (require 'cscope-filter nil t)
  ;; for large code base, set it to t.
  (setq cscope-do-not-update-database t
        cscope-program "gtags-cscope")
  ;; use cscope for java etc. as well
  (add-hook 'java-mode-hook (function cscope:hook))
  (add-hook 'eshell-mode-hook (function cscope:hook))
  (add-hook 'python-mode-hook (function cscope:hook))

  (setq
   cscope-database-regexps
   '(("\\(sandbox/trunk\\)"
      (t) ;; local cscope.out first
      ("/home/lgfang/projects/vsg/sandbox/lcp_lite/")
      ("/home/lgfang/projects/vsg/sandbox/libsoap-1.1.0/libcsoap/")
      ("/home/lgfang/projects/vsg/sandbox/libxml2/")
      t ;; 't' doesn't work, comment out useless database-dir
      ;;("/remote/.../b2008.09_icc_us02/syn/icc_sh/cscope.out.bak")
      ))))

(add-to-list 'load-path (concat my-extension-path "yasnippet"))
;; NOTE: download http://elpa.gnu.org/packages/cl-lib.html if needed
(when (require 'yasnippet nil t)
  (setq yas/root-directory
        (list (concat my-extension-path "yasnippet/snippets")))
  (when (file-exists-p "~/.emacs.snippets")
    (add-to-list 'yas/root-directory "~/.emacs.snippets"))

  ;; Org-mode specific
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
  (add-hook
   'org-mode-hook
   (lambda ()
     ;; yasnippet (using the new org-cycle hooks)
     (make-variable-buffer-local 'yas/trigger-key)
     (setq yas/trigger-key [tab])
     (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
     (define-key yas/keymap [tab] 'yas/next-field)))

  (yas-global-mode 1))

;;; yaml
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
  (require 'yaml-path))

;;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;;; ------ end General ------

;;; ------ begin MyFunction ------

;; auto insert TODO: use yasnippet?
(defun lgfang-copy-left ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((beg (point)))
    (insert
     "\\file Name: "
     (file-name-nondirectory buffer-file-name) "\n"
     "Created:  " (user-full-name) " "
     (format-time-string "%m/%d/%Y") "\n"
     "Modified: >\n"
     "\n"
     "\\brief\n"
     "\n"
     "\\details\n"
     "\n"
     )
    (comment-region beg (point)))
    (let ((beg (point)))
      (insert
       "This program is free software; you can redistribute it
and/or modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Had you not received a copy of the GNU General Public License
yet, write to the Free Software Foundation, 675 Mass Ave,
Cambridge, MA 02139, USA.
")
      (let ((fill-column (- fill-column 6))) ;; make room for comment
        (fill-region beg (point)))
      (comment-region beg (point))
      )
    ))

(define-auto-insert
  (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "C / C++ header")
  'lgfang-copy-left t)

;; autoinsert C/C++ header
(define-auto-insert
  (cons "\\.\\([Cc]\\|cc\\|cpp\\)\\'" "C / C++ program")
  'lgfang-copy-left t)

(define-auto-insert
  (cons "\\.py\\'" "Python Script")
  'lgfang-copy-left t)

(define-auto-insert
  (cons "\\.sh\\'" "Shell Script")
  'lgfang-copy-left t)

(defadvice comment-dwim (before lgfang-comment-wim activate compile)
  "if neiter mark-active not at end of line, comment current
line (by making the whole line an active region). This gets you a
really cool behavior :)."
  (unless (or mark-active (looking-at "[ \t]*$"))
    (goto-char (line-beginning-position))
    (set-mark-command nil)
    (goto-char (line-end-position))))

(defun lgfang-goto-page (pageNumber)
  "RFCs in ascii format use traditional page
delimiter (Ctrl-L). While Emacs Provides functions like
forward-page,backward-page etc., it doesn't provide goto-page or
sth alike. To go to certain page, I used to either go to the
beginning of the buffer at first or calculate how many pages to
be moved from current page at first. For me, that is a little
boring. I think this function may help. P.S. You may want to give
rfcview.el a try."

  (interactive
   (if (and current-prefix-arg (not (consp current-prefix-arg)))
       (list (prefix-numeric-value current-prefix-arg))
     ;; Look for a default, a number in the buffer at point.
     (let* ((default
              (save-excursion
                (skip-chars-backward "0-9")
                (if (looking-at "[0-9]")
                    (buffer-substring-no-properties
                     (point)
                     (progn (skip-chars-forward "0-9") (point)))))))

       (list (read-from-minibuffer
              (format
               (if default "Goto Page (%s): " "Goto Page: ") default)
              nil nil t
              'minibuffer-history
              default)))))
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-page (1- pageNumber))))

(defun lgfang-insert-date ()
  "Insert current date at point.  From Tijs van Bakel at
newsgroup: gnu.emacs.help.  To customize format of date
string,refer to format-time-string."
  (interactive)
  (insert (format-time-string "%m/%d/%Y")))

(defun lgfang-mode-line-all ()              ; long/long/ago
  "Sometimes there are too many infomation in mode line to show
  it in one line. Using this function to show it in an message
  box (or pop-up tool tip)"
  (interactive)
  (message "%s" (format-mode-line mode-line-format t))
  ;; (tooltip-show (format-mode-line mode-line-format t))
  ;; (message-box "%s" (format-mode-line mode-line-format t))
  )
(fset 'mla 'lgfang-mode-line-all)

(require 'cl)                           ; list-length defined in "cl.el"
(defun lgfang-recentf-open ()
  "open recent files. In ido style if applicable"
  (interactive)
  (let* ((prompt "File Name: ")
         (path-table (mapcar
                      (lambda (x) (cons (file-name-nondirectory x) x))
                      recentf-list))
         (fname (if (require 'ido nil t)
                    (ido-completing-read
                     prompt
                     (mapcar (lambda(x) (file-name-nondirectory
                                         x)) recentf-list))
                  (completing-read prompt path-table)))
         candidates )
    (dolist (afile path-table)
      (if (string-equal (car afile) fname)
          (progn
            (add-to-list 'candidates (cdr afile)))))

    (if (> (list-length candidates) 1)
        (find-file (ido-completing-read "full path:" candidates))
      (find-file (cdr (assoc fname path-table))))))

;;; replace strings in parallel
(defun lgfang-paralle-repl (replacement-alist)
  "Replace pairs of strings to search/replace in parallel."
  (interactive (list (batch-replace-strings-prompt)))
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end))
            (case-fold-search nil))
        (save-excursion
          (dolist (pair replacement-alist)
            (goto-char (min beg end))
            (while (search-forward (car pair) (max beg end) t)
              (replace-match (cdr pair) t t)))))
    (message "No text selected")))

(defun batch-replace-strings-prompt ()  ; from Trey Jackson
  "prompt for string pairs and return as an association list"
  (let (from-string ret-alist)
    (while (not (string-equal "" (setq from-string
                                       (read-string
                                        "String to search (RET to stop): "))))
      (setq ret-alist
            (cons (cons from-string (read-string
                                     (format "Replace %s with: " from-string)))
                  ret-alist)))
    ret-alist))

(when (require 'ange-ftp nil t)
  (defvar lgfang-to-protocols (list "ssh" "ftp") "")
  (defvar lgfang-to-users (list "root" ange-ftp-default-user) "")
  (defvar lgfang-to-history nil "")
  (defun lgfang-to()
    "Inspired by 'remote-access' from tonyaw"
    (interactive)
    (let* ((protocol
            (read-from-minibuffer "Protocol (ftp): "
                                  "ftp" nil nil 'lgfang-to-protocols nil))
           (prompt "host name: ")
           (hosts "~/.hosts") host-names host-list)

      (when (file-readable-p hosts)
        (with-temp-buffer
          (insert-file-contents hosts)
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((line (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)))
                   (fields (split-string line)))
              (unless (or (string-match "^#.*$" line)
                          (string-match "^[ \t]*$" line))
                (add-to-list 'host-names (or (nth 1 fields) (car fields)))
                (add-to-list 'host-list
                             (list (or (nth 1 fields) (car fields)) fields)))
              (forward-line))))

        (let* ((dest (if (require 'ido nil t)
                         (ido-completing-read prompt host-names nil nil nil
                                              'lgfang-to-history)
                       (completing-read prompt host-list nil nil nil
                                        'lgfang-to-history)))
               (ip (nth 0 (car (cdr (assoc dest host-list)))))
               (user (or (nth 2 (car (cdr (assoc dest host-list))))
                         (read-from-minibuffer "username (root): " "root"
                                               nil nil 'lgfang-to-users)))
               (file-name (format "/%s:%s@%s:/" protocol user ip)))

          (ffap file-name))))))

;;; ------ end MyFunction ------
(load "tmp.el" t nil nil)
