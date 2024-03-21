;;; Use built-in org mode, remember to M-x package-install org-contrib

(require 'org nil t)

(when (require 'ox-confluence nil t)

  ;; Confluence wiki/jira does not remove newlines, so need to unfill
  ;; paragraphs in org-confluence-export-as-confluence

  (defun org-confluence-paragraph (paragraph contents info)
    "An emulation of org-gfm-paragraph"
    (unless (plist-get info :preserve-breaks)
      (setq contents (concat (mapconcat 'identity (split-string contents) " ") "\n")))
    contents)
  )

;; Org mode -> reveal.js presentations
(require 'ox-reveal)
;; Ensure the following are done
;; - `brew install node'
;; - `git clone https://github.com/hakimel/reveal.js.git ~/source/reveal.js'
(setq org-reveal-title-slide 'auto
      org-reveal-root (file-name-as-directory (expand-file-name "~/source/reveal.js")))

(setq org-hide-leading-stars nil
      org-startup-folded t
      org-cycle-include-plain-lists t
      org-src-fontify-natively t
      org-reverse-note-order t
      org-archive-location (concat my-personal-path "archive.gtd::* From %s")

      ;; At the moment, I clock heavily. In such situation, the default value
      ;; "(closed clock)" becomes annoying in org-agenda-log-mode because is
      ;; CLOSED entries are kinda out of order due to sorting using the start
      ;; times.

      ;;  18:00-18:10 Clocked:   (0:10) DONE task1
      ;;  18:10-18:15 Clocked:   (0:05) DONE task2
      ;;  18:10...... Closed:     DONE task1 <--- I wish this entry move one line up
      ;;  18:15...... Closed:     DONE task2
      org-agenda-log-mode-items '(clock)

      org-clock-update-period 300
      org-duration-format '(("y")("m")("w")("d") (special . h:mm))
      org-duration-units `(("min" . 1)  ; IMPORTANT: must round all units to integers
                           ("h" . 60)
                           ("d" . ,(round (* 60 7.6))) ; 7.6 hours per day (Aussie law)
                           ("w" . ,(round (* 60 7.6 5))) ; 38 hours per week
                           ("m" . ,(round (* 60 7.6 22)))  ; ~22 days per month
                           ("y" . ,(round (* 60 7.6 250)))) ; ~250 working days per year

      ;;; GTD related
      ;; refer to [[file:./org-gtd.lgf]] for the usage
      org-agenda-files
      (mapcar (lambda (x) (concat my-personal-path x))
            (list "work.gtd" "personal.gtd" "part-time.gtd"))

      org-todo-keywords '(
                          ;; !@ means attach timestamps/notes when changing to certain state
                          (type "INCOME(i!)" "TODO(t!)" "WAIT(w@)" "HOLD(h!)" "MAYBE(m!)"
                                "|" "DONE(d!)" "CANCEL(c@)" "DELEGATE(@)")
                          )

      ;;; Todo dependencies
      ;; block changing to DONE until all tasks it depends on are done.
      org-enforce-todo-dependencies t
      ;; hide the task in org agenda until it is  unblocked.
      org-agenda-dim-blocked-tasks 'invisible

      ;; For easily refile todo items: mark possible targets with the tag
      ;; "headline". Do not use multiple levels of headers because that
      ;; generates too many targets.
      org-refile-targets '((org-agenda-files . (:tag . "headline")))

      org-log-done 'time
      org-agenda-span 'week
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-show-all-dates nil
      org-agenda-start-on-weekday nil
      ;; org-agenda-include-diary t
      org-deadline-warning-days 3
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-tags-exclude-from-inheritance (list "project")
      org-agenda-custom-commands
      '(("x" "TODO items @ALL@"
         ((todo "INCOME" ((org-agenda-todo-ignore-scheduled 'future)))
          (todo "TODO" ((org-agenda-todo-ignore-scheduled 'future)))
          (agenda "" ((org-agenda-span 2)
                      (org-deadline-warning-days 7)
                      ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("INCOME" "TODO")))
                                        ; Do not skip scheduled todo items. 1)
                                        ; As per my org-gtd, few items have both
                                        ; schedule and a todo tag. So I don't
                                        ; expect it would cause many troubles 2)
                                        ; I do want to see the clocked todo
                                        ; items in org-agenda-log-mode.
                      ))))
        ("y" "TODO items @WORK@"
         ((todo "INCOME"
                ((org-agenda-files '("~/mynotes/personal/work.gtd"))
                 (org-agenda-todo-ignore-scheduled 'future)))
          (todo "TODO"
                ((org-agenda-files '("~/mynotes/personal/work.gtd"))
                 (org-agenda-todo-ignore-scheduled 'future)))
          (agenda ""
                  ((org-agenda-span 2)
                   (org-deadline-warning-days 7)
                   ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("INCOME" "TODO")))
                                        ; see above for why no longer skip todo items.
                   (org-agenda-files '("~/mynotes/personal/work.gtd"))))))
        ("p" "PENDING items"
         ((todo "WAIT") (todo "HOLD")  (todo "MAYBE")))
        ("r" "Report work 1 week" tags "+CLOSED>\"<-7d>\""
         ((org-agenda-files '("~/mynotes/personal/work.gtd"))))
        ("b" "My bookmark" search nil
         ((org-agenda-files '("~/mynotes/personal/bookmarks.lgf"))))
        ("c" "Bookmark category" tags nil
         ((org-agenda-files '("~/mynotes/personal/bookmarks.lgf")))))

      org-default-notes-file "~/.income.org" ; overwritten by
                                             ; org-capture-templates
      org-capture-templates
      (list
       (list "w" "work related" 'entry
             (list 'file+olp (concat my-personal-path "work.gtd") "Misc.")
             "* INCOME %?" :prepend t)    ; number of * doesn't matter
       (list "p" "personal task" 'entry
             (list 'file+olp (concat my-personal-path "personal.gtd") "Misc.")
             "* INCOME %?" :prepend t)
       ;; (list "r" "things to be included in work report" 'entry
       ;;       ;; this one is a little bit cool :)
       ;;       (list 'file+datetree (concat my-personal-path "work-report.lgf"))
       ;;       "* %?" :prepend t)
       )        ; end of org-capture-templates

      ;;; publish related. Refer to my orgpub-levelN files for more
      ;; options

      org-export-headline-levels 3
      org-html-style-include-default nil
      org-html-htmlize-output-type 'css

      ;; for subscripts, use a_{b} instead of a_b
      org-export-with-sub-superscripts (quote {})

      ;; nil as default to avoid leaking sensitive info unintentionally,
      ;; overwritten in per-project settings bellow
      org-html-postamble nil
      org-publish-project-alist
      '(
        ("notes-attachments"
         :base-directory "~/mynotes"
         :base-extension "css\\|js\\|png\\|jpg\\|patch"
         :recursive t
         :publishing-directory "~/websites/blog/mynotes"
         :publishing-function org-publish-attachment)

        ("notes"
         :base-directory "~/mynotes"
         :base-extension "org"
         :recursive t
         :publishing-directory "~/websites/blog/mynotes"
         :publishing-function org-html-publish-to-html
         :exclude "\\(todo\\|confidential\\)"
         :html-postamble get-public-postamble
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "List of lgfang's notes"
         :sitemap-style tree
         :sitemap-sort-files anti-chronologically
         :sitemap-sort-folders last
         :sitemap-file-entry-format "%t")

        ("blog-attachments"
         :base-directory "~/myblog/images"
         :base-extension "png\\|jpg"
         :recursive t
         :publishing-directory "~/websites/blog/images"
         :publishing-function org-publish-attachment)

        ("blog"
         :base-directory "~/myblog"
         :base-extension "org"
         :recursive t
         :publishing-directory "~/websites/blog/_posts"
         :publishing-function org-html-publish-to-html
         :auto-sitemap nil
         :body-only t ;; Only export section between <body> </body>
         :html-link-use-abs-url t
         )

        ("projects"
         :base-directory "~/projects/"
         :base-extension "org"
         :recursive t
         :publishing-directory "~/websites/ihgp"
         :publishing-function org-html-publish-to-html
         :html-postamble get-intranet-postamble)

        ("lgf"
         ;; these are files want to keep secret but sometimes need to converted
         ;; to html temporarily
         :base-directory "~/mynotes"
         :base-extension "lgf"
         :recursive t
         :publishing-directory "~/tmp"
         :publishing-function org-html-publish-to-html))

      org-export-allow-bind-keywords t  ; I like "#+BIND:var value"
      )

;; tkt -> ticket
(add-to-list 'auto-mode-alist
             '("\\.\\(blog\\|org\\|lgf\\|tkt\\)$" . org-mode))

;; Enable structure templates, i.e. type `< s TAB' to insert #+begin_src etc.
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sb" . "src bash"))
(add-to-list 'org-structure-template-alist '("sj" . "src javascript"))
(add-to-list 'org-structure-template-alist '("st" . "src text"))

(add-hook 'remember-mode-hook 'org-remember-apply-template)

(add-hook 'org-mode-hook
          (lambda()
            (imenu-add-menubar-index)
            (define-key org-mode-map [mouse-3] 'org-mark-ring-goto)
            (highlight-lines-matching-regexp "lgf:" 'org-document-info)
            (when (and (stringp buffer-file-name)
                       (string-match "\\.tkt\\'" buffer-file-name))
              ;; turn on fly spell for tkt (ticket)
              (flyspell-mode 1)
              (setq-local org-export-with-toc nil))))

(defun get-intranet-postamble (plist)
  (let ((title (plist-get plist :title))
        (creator (plist-get plist :creator))
        (time (format-time-string org-html-metadata-timestamp-format)))
    (format
     "¯
     <p>
     <span class=\"date\">Created: %s</span>
     by <span class=\"creator\">%s</span>
     </p>
     <p>
     <a href=\"mailto:%s?Subject=Comments on %s\">Send a feedback</a>
     </p>
     "
     user-mail-address title time creator)))

(defun get-public-postamble (plist)
  (let ((title (plist-get plist :title))
        (creator (plist-get plist :creator))
        (time (format-time-string org-html-metadata-timestamp-format)))
    (format
     "
     <br/>
     <p>
     <span class=\"date\">Created: %s</span> by <span class=\"creator\">%s</span>
     </p>

     <!-- DISQUS  -->
     <div id=\"disqus_thread\"></div>
     <script type=\"text/javascript\">
         /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
         var disqus_shortname = '%s'; // required: replace example with your forum shortname

         /* * * DON'T EDIT BELOW THIS LINE * * */
         (function() {
             var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
             dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
             (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
         })();
     </script>
     <noscript>Please enable JavaScript to view the <a href=\"http://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>
     <a href=\"http://disqus.com\" class=\"dsq-brlink\">comments powered by <span class=\"logo-disqus\">Disqus</span></a>

     <!-- google analytic -->
     <script>
       (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
       (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
       m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
       })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

       ga('create', '%s', 'auto');
       ga('send', 'pageview');

     </script>
     "
     time creator
     ;; The following variables are defined in the my-confident
     my-disqus-shortname my-blog-google-analytic-track-id)))

(defun org-publish-find-date (file)
  "The official one takes toooo much time to run. For me, sort
according to modification time is good enough."
  (if (file-exists-p file) (nth 5 (file-attributes file))
    (error "No such file: \"%s\"" file)))

;; enable ditaa and plantuml etc.
(org-babel-do-load-languages 'org-babel-load-languages
                             '((ditaa . t)
                               (plantuml . t)
                               (dot . t)
                               ))
;; don't ask before evaluating
(defun my-org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "ditaa")
           (string= lang "plantuml")
           (string= lang "dot"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate
      org-ditaa-jar-path (concat my-extension-path "ditaa.jar")
      org-plantuml-jar-path (concat my-extension-path "plantuml.jar"))

(defun my-mark-comment ()
  "Mark the whole top level header as the active region.

 This is for my export-xxx functions. Each top level heading is
 one comment."
  (cond
   ((region-active-p) nil)
   ((org-before-first-heading-p)
    (push-mark (point-min))
    (goto-char (+ (or (outline-next-heading) (point-max)) -1))
    ;; ^^^ move 1 char back so that we are not on the header line of the next
    ;; subtree
    (activate-mark))
   (t
    (org-mark-subtree 100)        ; 100 levels up -> go to the top level heading
    (forward-line 1)              ; skip the header
    )))

(defun export-ticket-comment ()
  "Export the current page to gfm or confluence(jira) markdown according to the buffer name prefix."
  (interactive)
  (my-mark-comment)
  (let ((my-get-relative-level (lambda (headline info)
                                 (+ 2
                                    (org-element-property :level headline)
                                    (or (plist-get info :headline-offset) 0)))))
    ;; Start the headline level from 3 because h1, h2 are too big in jira/sfsc
    ;; for most ticket comments. If, we do want to start with h1, say we are
    ;; posting an article, instead of calling this function, (manually select
    ;; the region and) run org-confluence-export-as-confluence.
    (advice-add 'org-export-get-relative-level :override my-get-relative-level)
    (if (string-match "sf-" buffer-file-name) (org-gfm-export-as-markdown)
      (org-confluence-export-as-confluence))
    (advice-remove 'org-export-get-relative-level my-get-relative-level)))

(defun export-commit-msg ()
  "Export the current page to git commit friendly markdown."
  (interactive)
  (my-mark-comment)
  (let (;; Preserve line breaks, making the message terminal friendly.
        (org-export-preserve-breaks t)
        ;; Do not use the default (atx) style headlines because they start with
        ;; '#' and are ignored by 'git commit' as commments.
        (org-md-headline-style 'setext)
        )
    (org-gfm-export-as-markdown)))

(defun export-github-msg ()
  "Export the current page to a github web UI message.
It is the same as export-commit-msg except that it does not wrap
long lines"
  (interactive)
  (my-mark-comment)
  (let ((org-export-preserve-breaks nil)
        (org-md-headline-style 'setext)
        )
    (org-gfm-export-as-markdown)))

(defun comment-sanitize()
  "Remove the personal information in my screen outputs.

Run this function after a screen output is copied into an Emacs
buffer so that my personal info is not saved/shared."
  (interactive)
  (my-mark-comment)

  ;; remove the extra info in my PS1, which may be unnecessary & confusing.
  ;; An example PS1:
  ;; ╭ 1251 16:16:18 git:master kube:api.prod.corp.company.com/my-namespace
  ;; │ lungang.fang@laptop:~/source/org-contrib
  ;; ╰ $
  (replace-regexp-in-region "^\\( *\\)╭ .*\n *│ .*\n *╰ " "\\1"
                            (region-beginning) (region-end))
  ;; remove my username, don't want to be super smart with regex.
  (replace-regexp-in-region "lungang\\(.\\)fang" "<given_name>\\1<family_name>"
                            (region-beginning) (region-end))
  (replace-regexp-in-region "fang\\(.\\)lungang" "<family_name>\\1<given_name>"
                            (region-beginning) (region-end))
  )

(eval-after-load "org-agenda"
  '(define-key org-agenda-mode-map (kbd "M-p") 'add-to-project-list))

(provide 'lgfang.org)
;;; .org.el ends here
