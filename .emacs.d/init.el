;;; init.el --- Lungang's init.el

;; Created:  Lungang Fang 2004
;; Modified: Lungang Fang 2024-05-04T21:48:16+1000>

;;; Commentary:

;; Lungang's Emacs configuration file, initiated in 2004 and has been undergoing
;; an overhaul since 2024-05-01.

;;; Code:

;;; First things first

;;;; Paths
(defvar my-elisp-path
  (file-name-as-directory (expand-file-name "~/.emacs.d/my-elisp")))

(add-to-list 'load-path my-elisp-path t)

;;;; setup package

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Mini-buffer Interaction

(use-package vertico
  :ensure t
  :defer t
  :defines vertico-map
  :functions vertico-mode vertico-multiform-mode
  :bind (:map vertico-map ("C-o" . vertico-quick-exit))
  :custom
  (vertico-resize nil)
  (vertico-cycle nil)
  (vertico-buffer-display-action '(display-buffer-at-bottom
                                   (window-height . 16)))
  ;; Avoid `reverse' + `mouse', they are not compatible at the moment.
  (vertico-multiform-categories '(
                                  (buffer flat)
                                  ;; default, enable buffer & mouse
                                  (t buffer mouse)
                                  ))
  (vertico-multiform-commands '(("imenu" buffer mouse)
                                ("recentf-.*" buffer mouse)
                                ))

  :init
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  )

(use-package orderless
  :ensure t
  :defer t
  :custom
  (completion-styles '(orderless flex substring basic))
  )

(use-package marginalia
  :ensure t
  :defer t
  :functions marginalia-mode
  :init
  (marginalia-mode 1)
  )

;;; Looks

;;;; Frame & Window
(menu-bar-mode (if (display-graphic-p) 1 -1)) ; turn it on for GUI only
(tool-bar-mode -1)                            ; turn it off

(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;;;; Fonts
(when (display-graphic-p)
  (set-face-attribute 'default nil :font "monaco-18:weight=normal")

  ;; Select the font for Chinese characters using `set-fontset-font'. This
  ;; command sets the fallback font when the default font (set above) doesn't
  ;; support the current character. By default, Emacs iterates all the fonts
  ;; until it finds one that supports the character.
  (let ((zh-font "SimSong"))
    (if ;; Check the availability first to avoid error
        (member zh-font (font-family-list))
        ;; "fall back" to the designated zh font for `han' characters. Guard the
        ;; following expression with `fboundp' to avoid the warning: "function
        ;; ... is not known to be defined" .
        (and (fboundp 'set-fontset-font) (set-fontset-font t 'han zh-font))))

  ;; Scale Chinese fonts so that the width of 1 Chinese char equals that of two
  ;; English chars. This list is manually maintained as the scale factors for
  ;; different fonts are determined through trial and error. Note: a) To check
  ;; the font of the current character, run `C-u C-x ='. b) To get more accurate
  ;; data, compare longer lines of English/Chinese.
  (setq face-font-rescale-alist '(("SimSong" . 1.25)
                                  ("PingFang SC" . 1.25)
                                  ))
  )

;;;; ZWJ (Zero Width Joiner) emoji handling.
;; See a ZWJ example in ~/mynotes/emacs/emacs-unicode-test.org
(unless (display-graphic-p)
  ;; Disable auto-complete-mode if running when in a terminal as most terminal
  ;; emulators cannot handle Emoji ZWJ. NOTE: disabling it on the fly does not
  ;; work very well, must restart Emacs.
  (setq-default auto-composition-mode nil)
  )

;;;; Start up screen
(setq inhibit-startup-screen t)

;;; Spelling check
(use-package flyspell
  :config
  :hook ((prog-mode . flyspell-prog-mode)
         (yaml-mode . flyspell-prog-mode)
         (yaml-ts-mode . flyspell-prog-mode)
         (markdown-mode . flyspell-mode)
         (git-commit-setup . flyspell-mode))
  )

;;; Completion

;;;; Company
(use-package company
  :ensure t
  :functions global-company-mode
  :init (global-company-mode)
  )

;;;; Yasnippet
(use-package yasnippet
  ;; Put personal/customized snippets into the first dir of `yas-snippet-dirs',
  ;; which is `~/.emacs.d/snippets' by default. NOTE: it is `yas-snippet-dirs'
  ;; NOT `yasnippet-snippets-dir'. The later is where the package
  ;; `yasnippet-snippets' stores its snippets).
  ; TODO: cleanup duplicated/similar snippets in different directories.
  :ensure t
  :functions yas-global-mode
  :init (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after yasnippet-snippets)

;;; Syntax parser (Tree-sitter)
(use-package treesit
  ;; Run `treesit-install-language-grammar' to install the grammar
  ;; for each designated language.
  :when
  (and (fboundp 'treesit-available-p) (treesit-available-p))

  :custom
  (major-mode-remap-alist
   '(
     (bash-mode . bash-ts-mode)
     (c++-mode . c++-ts-mode)
     (c-mode . c-ts-mode)
     (cmake-mode . cmake-ts-mode)
     (conf-toml-mode . toml-ts-mode)
     (js-json-mode . json-ts-mode)
     (python-mode . python-ts-mode)
     (yaml-mode . yaml-ts-mode)
     ))
  )

(use-package treesit-fold
  ;; Place holder, this package is not ready yet.
  :after treesit
  ;; :load-path (lambda() (concat my-emacs-d "treesit-fold"))
  )

;;; LSP (eglot)
(use-package eglot
  ;; Note for Eglot + Pyright on MacOS: you may want to increase the "open
  ;; files" limit (`ulimit -n'), say to 65536. The default value is 256, which
  ;; Pyright easily hits when the python project is non-trivial. (You'll see the
  ;; error message if you set debug-on-error to t and then enable Eglot.).
  )

;;; Syntax check (flymake)
(use-package flymake
  ;; To list all the diagnostics, use `flymake-show-buffer-diagnostics' and
  ;; `flymake-show-project-diagnostics'. For checkers being used, see the buffer
  ;; local var `flymake-diagnostic-functions'.

  :bind (:map flymake-mode-map
              ("C-c p" . flymake-goto-prev-error)
              ("C-c n" . flymake-goto-next-error))

  :hook (prog-mode yaml-ts-mode)
)

(use-package flymake-ruff
  :ensure t
  :after flymake
  :custom
  (python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-"))
  )

(use-package flymake-yamllint
  :ensure t
  :after flymake
  :hook ((yaml-ts-mode . flymake-yamllint-setup))
  )

;;; DAP (dape)
(use-package dape
  ;; For Python, `pip3 install debugpy'. Run adapter `debugpy' to test a
  ;; program, adapter `debugpy-module' for testing a module.

  :ensure t
  :after eglot
  :config
  ;; Save files before sessions, useful for interpreted languages, such as
  ;; python; Cannot use `:hook' since this hook name doesn't end with "-hook"
  (add-hook 'dape-on-start-hooks 'save-some-buffers)
  )

;;; ANSI color code
(use-package ansi-color
  :hook (;; render color codes in the compilation buffer.
         (compilation-filter . ansi-color-compilation-filter))
  )

(use-package lgf-ansi-color-mode
  :after ansi-color
  )

;;; --- old configurations ---
;;; paths
(defvar my-emacs-base
  (file-name-as-directory (expand-file-name "~/.emacs.d")))
(defvar my-extension-path
  (file-name-as-directory (expand-file-name "~/.emacs.d/emacs-extensions")))
(defvar my-personal-path
  (file-name-as-directory (expand-file-name "~/mynotes/personal")))
(defvar my-backward-path
  (file-name-as-directory (concat my-extension-path "backward-compatibility")))

;; load path
(add-to-list 'load-path my-extension-path)
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

;;; Completion

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

;; Confidential info saved in this file.
(unless (load (concat my-personal-path "my-confidential") t nil nil)
  ;; If this file doesn't exist, set dumb values to allow emacs to start.
  (setq user-full-name "Full Name" user-mail-address "you@email.com"))

(setq
      ;; url-proxy-services '(("http" . "localhost:8888"))

      ;; world time 'M-x display-time-world', /usr/share/zoneinfo
      display-time-world-time-format "%Z\t%z\t%a %d %b %R"
      display-time-world-list '(("Australia/Sydney" "Sydney")
                                ("UTC" "UTC")
                                ("America/New_York" "New York")
                                ("America/Chicago" "Chicago")
                                ("America/Los_Angeles" "Palo Alto")
                                ("Asia/Shanghai" "Beijing")
                                ("Asia/Kolkata" "Delhi")
                                ("Asia/Tel_Aviv" "Tel Aviv")
                                ("Europe/London" "Dublin"))
      )

;;; Language Environment - these settings are normally no longer needed

;; (if (eq system-type 'windows-nt)
;;     (if (>= emacs-major-version 23)
;;         (set-language-environment 'Chinese-GB18030)
;;       (set-language-environment 'Chinese-GB)))


;; (set-language-environment 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)
;; ;; set terminal coding system to utf-8 explictly to display unicode chars
;; ;; (including emojis/Chinese chars) correctly. This is needed when utf-8 is
;; ;; supported but emacs does not detect and set set it correctly due to wrong
;; ;; shell locale etc.
;; (set-terminal-coding-system 'utf-8)

;;; global key bindings

(when (eq system-type 'darwin)          ; OSX
  ;; ;; Not all Emacs builds support this modifier remap. At the moment, it works
  ;; ;; for my GUI Emacs. For terminal emacs, please remap modifiers in
  ;; ;; corresponding terminal emulator instead.
  ;; (setq mac-option-modifier 'control mac-command-modifier 'meta)

  (unless (display-graphic-p)
    ;; In OSX terminal, trackpad gestures for up/down mapped to mouse-4/5
    (define-key global-map [mouse-4] '(lambda () (interactive) (scroll-down 1)))
    (define-key global-map [mouse-5] '(lambda () (interactive) (scroll-up 1)))))

;; F1-F12
(define-key global-map [f1] 'recentf-open)
(define-key global-map [f2] 'imenu)
;; f3/f4: built-in key bindings to define keyboard macros
(define-key global-map [f5] 'whitespace-cleanup)
(define-key global-map [f8]  (lambda() "bury in case only one window"
                               (interactive) (bury-buffer) (delete-window)))
;; f9 to clock in last, `C-u f9' to select from recent tasks.
(define-key global-map [f9] 'org-clock-in-last)
;; shift-f9 to clock out
(define-key global-map (kbd "S-<f9>") 'org-clock-out)
(define-key global-map [f10] 'org-capture)
;; f11 : reserved for twm/tmux etc.
;; f12 : reserved for twm/tmux etc.

;; C-, M-, C-M- ... :(
(define-key global-map (kbd "C-x C-b") 'ibuffer)
(define-key global-map (kbd "C-x c l") 'org-store-link)
(define-key global-map (kbd "C-x c a") 'org-agenda)
(define-key global-map (kbd "C-x c o") 'org-open-at-point-global)
(define-key global-map (kbd "M-/") 'hippie-expand)
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

;; Move cursor between buffers
(define-key global-map (kbd "C-x <up>") 'windmove-up)
(define-key global-map (kbd "C-x <down>") 'windmove-down)
(define-key global-map (kbd "C-x <right>") 'windmove-right)
(define-key global-map (kbd "C-x <left>") 'windmove-left)

;; Swap buffers
(define-key global-map (kbd "C-x S-<up>"   ) 'buf-move-up)
(define-key global-map (kbd "C-x S-<down>" ) 'buf-move-down)
(define-key global-map (kbd "C-x S-<right>") 'buf-move-right)
(define-key global-map (kbd "C-x S-<left>" ) 'buf-move-left)

;; Change Layout
(define-key global-map (kbd "C-\\") 'tiling-cycle)
(define-key global-map (kbd "C-x C-S-<up>") 'tiling-tile-up)
(define-key global-map (kbd "C-x C-S-<down>") 'tiling-tile-down)
(define-key global-map (kbd "C-x C-S-<right>") 'tiling-tile-right)
(define-key global-map (kbd "C-x C-S-<left>") 'tiling-tile-left)

;; ;; Another type of representation of same keys, in case your terminal
;; ;; doesn't recognize above key-binding. Tip: C-h k C-up etc. to see into
;; ;; what your terminal tranlated the key sequence.
;; (define-key global-map (kbd "M-[ a"     ) 'windmove-up)
;; (define-key global-map (kbd "M-[ b"     ) 'windmove-down)
;; (define-key global-map (kbd "M-[ c"     ) 'windmove-right)
;; (define-key global-map (kbd "M-[ d"     ) 'windmove-left)
;; (define-key global-map (kbd "ESC <up>"   ) 'buf-move-up)
;; (define-key global-map (kbd "ESC <down>" ) 'buf-move-down)
;; (define-key global-map (kbd "ESC <right>") 'buf-move-right)
;; (define-key global-map (kbd "ESC <left>" ) 'buf-move-left)
;; (define-key global-map (kbd "ESC M-[ a" ) 'tiling-tile-up)
;; (define-key global-map (kbd "ESC M-[ b" ) 'tiling-tile-down)
;; (define-key global-map (kbd "ESC M-[ c" ) 'tiling-tile-right)
;; (define-key global-map (kbd "ESC M-[ d" ) 'tiling-tile-left)




;;; ascii mode
(autoload 'ascii-display "ascii" "Toggle ASCII code display." t)

;;; asm mode
(setq-default asm-comment-char 35)      ; 35 -> ascii code for '#'

;;; auto-complete - use company mode instead

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

;;; bbdb & bbdb-vcard-export - removed, use google/apple contacts etc.


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
            ;; (eldoc-mode 1)
            ))
;; Can't hook imenu-add-menubar-index to c-mode-common-hook since awk mode don't
;; support it
(dolist (hook '(c-mode-hook c++-mode-hook java-mode-hook))
  (add-hook hook 'imenu-add-menubar-index))

;;; Calendar Chinese & Aussie NSW Holidays
(when (require 'cal-china-x nil t)
  (defun holiday-new-year-bank-holiday ()
    "This & next copied from https://emacs.stackexchange.com/a/45352/9670"
    (let ((m displayed-month) (y displayed-year))
      (calendar-increment-month m y 1)
      (when (<= m 3)
        (let ((d (calendar-day-of-week (list 1 1 y))))
          (cond ((= d 6)
                 (list (list (list 1 3 y)
                             "NSW: New Year's Day (day in lieu)")))
                ((= d 0)
                 (list (list (list 1 2 y)
                             "NSW: New Year's Day (day in lieu)"))))))))
  (defun holiday-christmas-bank-holidays ()
    (let ((m displayed-month) (y displayed-year))
      (calendar-increment-month m y -1)
      (when (>= m 10)
        (let ((d (calendar-day-of-week (list 12 25 y))))
          (cond ((= d 5)
                 (list (list (list 12 28 y)
                             "NSW: Boxing Day (day in lieu)")))
                ((= d 6)
                 (list (list (list 12 27 y)
                             "NSW: Boxing Day (day in lieu)")
                       (list (list 12 28 y)
                             "NSW: Christmas Day (day in lieu)")))
                ((= d 0)
                 (list (list (list 12 27 y)
                             "NSW: Christmas Day (day in lieu)"))))))))
  (setq mark-holidays-in-calendar t
        holiday-nsw-holidays '((holiday-fixed 1 1 "NSW: New Year's Day")
                               (holiday-new-year-bank-holiday)
                               (holiday-fixed 1 26 "NSW: Austrlia Day")
                               (holiday-easter-etc -2 "NSW: Good Friday")
                               (holiday-easter-etc -1 "NSW: Easter Saturday")
                               (holiday-easter-etc 0 "NSW: Easter Sunday")
                               (holiday-easter-etc 1 "NSW: Easter Monday")
                               (holiday-fixed 4 25 "NSW: Anzac Day")
                               (holiday-float 6 1 2 "NSW: Queen's Birthday")
                               (holiday-float 10 1 1 "NSW: Labour Day")
                               (holiday-fixed 12 25 "NSW: Christmas Day")
                               (holiday-fixed 12 26 "NSW: Boxing Day")
                               (holiday-christmas-bank-holidays))
        holiday-other-holidays '((holiday-lunar 1 15 "元宵节")
                                 (holiday-fixed 10 31 "Halloween"))
        calendar-holidays (append
                           cal-china-x-chinese-holidays
                           holiday-nsw-holidays
                           holiday-other-holidays
                           )))

;;; ccrypt: auto encrypt/decrypt files using ccrypt
(require 'ps-ccrypt nil t)

;;; Clipboard
;; from/to tmux buffer
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
  "Get current tmux buffer."
  (interactive)
  (call-process "tmux" nil t nil "show-buffer"))

;; aliases to type less characters
(fset 'to-tmux 'lgfang-send-to-tmux)
(fset 'from-tmux 'lgfang-get-from-tmux)

;; From/to system clipboard. To use it in tmux, upgrade to tmux 2.6+.
(when (eq system-type 'darwin)
  (defun copy-from-osx ()
    (let ((tramp-mode nil) (default-directory "~"))
      (shell-command-to-string "pbpaste")))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;;; color theme
(if (require 'solarized-theme nil t)
    (load-theme 'solarized-gruvbox-dark t)
  ;; else, fallback to this builtin theme
  (load-theme 'wombat))

(column-number-mode t)

(setq comment-style 'extra-line)

;;; company - auto completion


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

;;; Copy/cut current line
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

;;; Current path+filename
(defun current-file-path ()
  "Copy current path/to/file_name to the kill ring."
  (interactive)
  (let ((string (buffer-file-name)))
    (message (concat "current file: " string))
    (kill-new string)))

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
;; guess of '!' action
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


;;; emms configure in another file
(load "lgfang.emms" t nil nil)

;;; ERC - use RCIRC instead for cleaner code base

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

(when (require 'ffap nil t)
;; (ffap-bindings) ; Don't bind to `C-x C-f' etc., explicitly `M-x ffap' etc.
;; (setq ffap-c-path (append ffap-c-path sourcepair-header-path))

  ;; My extensions to ffap jira/sfsc tickets. Remember to define my-employer in
  ;; my-confidential.el
  (defun ffap-jira (name) ; ffap HELP-12345 etc. opens corresponding jira ticket
    (let ((company (if (boundp 'my-employer) my-employer "example")))
      (format "https://jira.%s.org/browse/%s" company name)))
  (add-to-list 'ffap-alist '("\\`\\(HELP\\|EVG\\|BF\\|TIG\\)-[0-9]+\\'" . ffap-jira))
  ;;                            ^^^ Or simply "\\`\\([A-Z]+\\)-[0-9]+" ?

  (defun ffap-sfsc (name)  ; ffap 123456 opens corresponding SFSC ticket
    (let ((company (if (boundp 'my-employer) my-employer "example")))
      (format "https://support.%s.com/case/%s%s"
              company (make-string (- 8 (length name)) ?0)  name)))
  (add-to-list 'ffap-alist '("\\`[0-9]\\{6,8\\}\\'" . ffap-sfsc))
)

;;; fill column
(setq-default fill-column 80 comment-fill-column nil)
(require 'fill-column-indicator nil t) ;; run "(fci-mode)"
;; (define-globalized-minor-mode
;;   global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode t)


;; gdb
;; (setq gdb-many-windows t)

;;; Generative AI (GAI) - copilot
;; Note: not enabling it by default as what I'm after is copilot chat not
;; completion, in which static type checkers excels.
(add-to-list 'load-path (concat my-extension-path "copilot.el"))
(when (require 'copilot)
  (setq copilot-log-max 50000)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "<backtab>") 'copilot-previous-completion)
  (define-key copilot-completion-map (kbd "M-f") 'copilot-accept-completion-by-word)
  (define-key copilot-completion-map (kbd "C-e") 'copilot-accept-completion)
  )

;;; git-gutter
;; Choose this over diff-hl because the later does not work in 'emacs -nw'.
(when (require 'git-gutter)

  ;; To diff with a revision other than the latest one, in the repo root
  ;; directory, add to the emacs directory local variable file (.dir-locals.el)
  ;; add content similar to the following:
  ;;
  ;; ((prog-mode . ((git-gutter:start-revision . "my_branch"))))

  (global-git-gutter-mode t)

  ;; Jump to next/previous hunk
  (global-set-key (kbd "C-x v [") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x v ]") 'git-gutter:next-hunk)

  (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

  ;; Stage current hunk
  (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
  ;; Revert current hunk
  (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

  (set-face-background 'git-gutter:modified "DarkOrange")
  (set-face-background 'git-gutter:added "green")
  (set-face-background 'git-gutter:deleted "red")
  (setq git-gutter:modified-sign " "
        git-gutter:added-sign " "
        git-gutter:deleted-sign " ")
  )

;;; git-link
(setq git-link-open-in-browser t)

;;; golang
(add-hook 'go-mode-hook
          (lambda() (add-hook 'before-save-hook 'gofmt-before-save)))

;;; hide-ifdef-mode settings
(require 'hideif)

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

;;; Fold ifdef blocks by default. This is safer than showing them by default
;;; since when you see a code snippet folded, you know it is folded. In
;;; contrast, if they are not folded, you may learn in a hard way that you are
;;; in an undefined block.
(setq hide-ifdef-initially t
      hide-ifdef-define-alist
      ;; Add/remove "define" alist per your own need
      '((default)  ; An empty alist, makes every ifdef block folded, but not
                   ; ifndef blocks. See below for an example of how to define a
                   ; list per your project/environment.
        (mongodb-mac __APPLE__
                     (__LIBCPP_STD_VER . 14)
                     )
        ))
(defvar my-define-alist "mongodb-mac")

(defun lgfang-hide-ifdef-use-define-alist (name)
  "A wrapper for `hide-ifdef-use-define-alist' to use NAME define alist."
  (interactive
   (list (let* ((prompt "Use MACRO define list: ")
                (symbol-names
                 (mapcar (lambda (a) (symbol-name (car a)))
                         hide-ifdef-define-alist)))
           (completing-read prompt symbol-names))))
  (setq my-define-alist name) ; also apply this to buffers not opened yet
  (hide-ifdefs)                         ; for current buffer
  (hide-ifdef-use-define-alist name))

(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook (lambda () (hide-ifdef-mode 1)
                   (hide-ifdef-use-define-alist my-define-alist))))

(eval-after-load "cc-mode"
  '(define-key c-mode-base-map (kbd "M-'") 'lgfang-hif-toggle-block))

;;; hide-show
(setq hs-allow-nesting t hs-isearch-open t)
(defun lgfang-toggle-level ()
  "hide/show the next level"
  (interactive) (hs-show-block) (hs-hide-level 1))

;;; highlight current line: use the built in `hl-line'
;; Note that this and other similar functions (such as beacon) only works with
;; *active* cursor. Therefore, none of them highlights the "current" line/point
;; of another buffer. For example, in a compilation/grep buffer, you press n/p
;; to move the cursor in another buffer. Because you don't jump to that buffer,
;; the highlight in that buffer does not change.

;;; highlight cursor of insertion: `highlight-tail'
;; (when (require 'highlight-tail)
;;   (setq highlight-tail-colors '(("black" . 0)
;;                                 ("#bc2525" . 25)
;;                                 ("black" . 66))
;;         highlight-tail-steps 14
;;         highlight-tail-timer 1
;;         highlight-tail-posterior-type 'const)
;;   (highlight-tail-mode 1))

;;; highlight indetation levels
(when (require 'highlight-indentation)
  ;; Explicitly turn off highlight-indentation-blank-lines, which makes C-a
  ;; unable to go to the beginning of blank lines. And make other issues.
  (setq highlight-indentation-blank-lines nil)

  ;; ;; manually set the face if desired ("gray20" suits dark themes)
  ;; (set-face-background 'highlight-indentation-face "gray20")
  (add-hook 'python-mode-hook 'highlight-indentation-mode)
  (add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)
  )

;;; highlight parenthesis: `highlight-parentheses'
;; It's better than `show-paren-mode', which matches parentheseses at point
;; only.
(when (require 'highlight-parentheses nil t)
  ;; M-x list-colors-display to see named colors
  (setq hl-paren-colors '("brown" "orange" "yellow" "forest green"
                          "cyan" "blue" "violet"))
  (global-highlight-parentheses-mode t))

;;; highlight region
(setq-default transient-mark-mode t)

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

;;; imenu
(setq imenu-sort-function 'imenu--sort-by-name
      imenu-auto-rescan t
      imenu-use-popup-menu 'on-mouse)

;;; ispell - aspell instead
(setq ispell-program-name "aspell"
      ;; regardless locale settings, always use english refer to
      ;; ispell-dictionary-alist for details
      ispell-dictionary "english")

;;; Javascript
(add-hook 'js-mode-hook
          (lambda()
            (define-key js-mode-map (kbd "M-'") 'lgfang-toggle-level)
            (define-key js-mode-map [mouse-3] 'lgfang-toggle-level)
            (hs-minor-mode 1)))

;;; Large files
(defun my-find-file-huge-file-hook ()
  "Turn off features that make Emacs super slow with large log
files and avoid accidental modifications."
  (when (> (buffer-size) (* 1024 1024 8)) ; 8 MB
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (which-function-mode -1)
    (if (fboundp 'highlight-parentheses-mode) (highlight-parentheses-mode -1))
    ))
(add-hook 'find-file-hook 'my-find-file-huge-file-hook)

;;; ldap mode for ldif files
(autoload 'ldap-mode "ldap-mode" "Edit ldif files" t)

;;; line number
;; (setq-default
;;  ;; Note that corresponding faces maybe undefined and hence the major/minor
;;  ;; ticks are not shown.
;;  display-line-numbers-major-tick 50 display-line-numbers-minor-tick 10)

;;; log mode
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

;;; mermaid mode: package-installed, just remember to install mermaid cli:
;; 'npm install -g @mermaid-js/mermaid-cli'

(setq messages-buffer-max-lines 500)    ; default value too small

(when (require 'midnight nil t)
  (midnight-delay-set 'midnight-delay "1:30am")
  (setq clean-buffer-list-delay-general 1))

;;; mode line
(defvar mode-line-format-original nil
  "Stores the mode line format before shorten-mode-line is ever run.")
(defun shorten-mode-line ()
  "Hide unnecessary information to make room for more important information."
  (interactive)
  (unless mode-line-format-original
    (setq mode-line-format-original (copy-tree mode-line-format)))
  (setq-default mode-line-format (delq 'mode-line-modes mode-line-format)))
(defun restore-mode-line ()
  "Show the original/default full mode line."
  (interactive)
  (when mode-line-format-original
    (setq-default mode-line-format (copy-tree mode-line-format-original))))
(shorten-mode-line)

;;; mongolog: MongodDB log file mode
(add-to-list 'load-path (concat my-elisp-path "mongolog"))
(require 'mongolog nil t)
(add-to-list 'auto-mode-alist '("mongod.*\\.log" . js-mode))

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

;;; occur
(define-key occur-mode-map "n" 'next-error-no-select)
(define-key occur-mode-map "p" 'previous-error-no-select)
(define-key occur-mode-map " " 'occur-mode-display-occurrence)
(define-key occur-mode-map "o" (lambda () (interactive)
                                 (occur-mode-goto-occurrence)
                                 (delete-other-windows)))

;;; org mode
(load "~/.org" t nil nil)

;;; perl: using cperl-mode instead
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook 'imenu-add-menubar-index)
(setq cperl-indent-level 4
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)

;;; PHP
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;; Installed using M-x package-install
(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)

;;; python related
(add-hook 'python-mode-hook
          (lambda()
            (setq tab-width 4)  ; "python-mode" sets it to 8, change it back.
            (hs-minor-mode 1)
            (outline-minor-mode 1)
            ;; (anaconda-mode)             ; for cross reference, also see eglot
            (blacken-mode 1)            ; relies on the command black
            (setq imenu-create-index-function 'python-imenu-create-flat-index)
            (imenu-add-menubar-index)))

(setq python-fill-docstring-style 'django) ; triple quotes on their own lines

(require 'pydoc-info nil t)

;;; RCIRC - removed, use IRC no more.

;;; recently opened file
(require 'recentf)
;; add at the front of list, don't conncect to remote hosts
(add-to-list 'recentf-keep 'file-remote-p)
(setq recentf-max-saved-items 666)
(recentf-mode 1)

;;; Always end a file with a newline
(setq require-final-newline t)

;;; rfc
(add-to-list 'auto-mode-alist
             '("/\\(rfc[0-9]+\\|draft-.+\\)\\.txt\\(\\.gz\\)?\\'"
               . rfcview-mode))
;; so far this one renders RFCs best. But no longer available online(?)
(autoload 'rfcview-mode "rfcview")
;; ffap tries to find RFCs in these directories before giving a URL
(setq ffap-rfc-directories '("~/projects/rfc"))
;; ffap no longer downloads RFCs, `rfc-mode-read` downloads the RFC at point to
;; this directory.
(setq rfc-mode-directory "~/projects/rfc")

;;; rnc mode - nxml mode uses rnc files
(add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode))
(autoload 'rnc-mode "rnc-mode")
(setq rnc-enable-imenu t
      rnc-jing-jar-file (expand-file-name
                         (concat my-extension-path "jing/bin/jing.jar")))
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
(setq-default save-place-mode t)
(require 'saveplace)

(setq scroll-margin 0 scroll-conservatively 100) ;  scroll-step ?

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

;;; split horizontally if screen wide enough
(setq split-width-threshold 300)

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

;;; Tiling
(add-to-list 'load-path (concat my-elisp-path "tiling"))
(require 'tiling nil t)

;;; time stamp
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-format "%U %Y-%02m-%02dT%02H:%02M:%02S%5z"
      time-stamp-start "\\(Modified\\|last-edit\\): *\\\\?"
      time-stamp-end "\\\\?>"
      ;; no Chinese chars in time stamps even in Chinese locale.
      system-time-locale "C")

;;; Terraform (package install terraform-mode)
(setq-default terraform-indent-level 4)

;;; toggle-window-dedicated.el
(load "toggle-window-dedicated" t nil nil)

;;; tramp
(require 'tramp)
(setq tramp-debug-buffer t)
(add-to-list 'tramp-default-method-alist '("localhost" nil "su"))

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

;;; version control: git, vc, clearcase etc.
(load "lgfang.vc" t nil nil)
(add-to-list 'load-path (concat my-extension-path "vc-clearcase"))
(load "vc-clearcase-auto" t nil nil)
(setq clearcase-use-external-diff t             ; the internal one sucks
      ;; vc-clearcase-diff-switches "-diff_format" ; if diff unavailable
      ;; vc-git-diff-switches "-w"         ; ignore diff of whitespace
      )
;; magit show CommitDate instead of AuthorDate in log mode
(setq magit-log-margin-show-committer-date t)

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
              tab-stop-list nil    ; stops every ‘tab-width’ columns
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

;;; xcscope,
;; NOTE: cscope is now just a backup. Normally eglot + clangd is more convient.
;; Just `M-x eglot` in a C/C++ buffer to activate eglot.
(when (require 'xcscope nil t)
  (cscope-setup)

  (setq
   ;; use gtags-cscope instead of the legacy cscope
   cscope-program "gtags-cscope"
   ;; set cscope-database-file accordingly. Otherwise xcscope looks for
   ;; "cscope.out" and fails and then build the database in the current
   ;; directory.
   cscope-database-file "GTAGS")

  ;; ;; Below are for huge code bases. No need of them at the moment
  ;; (require 'cscope-filter nil t)
  ;; (setq
  ;;  cscope-do-not-update-database t ; do not rebuild database for every search.
  ;;  cscope-database-regexps
  ;;  '(("\\(sandbox/trunk\\)"
  ;;     (t) ;; local cscope.out first
  ;;     ("/home/lgfang/projects/vsg/sandbox/lcp_lite/")
  ;;     ("/home/lgfang/projects/vsg/sandbox/libsoap-1.1.0/libcsoap/")
  ;;     ("/home/lgfang/projects/vsg/sandbox/libxml2/")
  ;;     t ; 't' doesn't work, comment out useless database-dir
  ;;     ;;("/remote/.../b2008.09_icc_us02/syn/icc_sh/cscope.out.bak")
  ;;     )))

  )

;;; xref
(setq xref-prompt-for-identifier t) ; always prompt for identifier to search

;;; yaml
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
  (require 'yaml-path))

;;; ------ end General ------

;;; ------ begin MyFunction ------

(defadvice comment-dwim (before lgfang-comment-wim activate compile)
  "if neiter mark-active nor at end of line, comment current
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
  (insert (format-time-string "%Y-%m-%d")))

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

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;; ------ end MyFunction ------

(load "tmp.el" t nil nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-colors '("#689d6a" "#d79921" "#458588" "#b16286" "#98971a"))
 '(package-selected-packages
   '(flymake-yamllint editorconfig company cue-mode git-gutter mermaid-mode protobuf-mode cmake-mode magit anaconda-mode eglot blacken git-link csv-mode emms json-reformat windata w3m solarized-theme showtip terraform-mode highlight-parentheses highlight-indentation org-contrib yasnippet-snippets hide-lines ox-gfm yasnippet pydoc-info pydoc markdown-mode jira-markup-mode ht go-mode flycheck f)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
