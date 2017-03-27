(add-to-list 'load-path (concat my-extension-path "org-mode/lisp"))
(add-to-list 'load-path (concat my-extension-path "org-mode/contrib/lisp"))

(require 'org nil t)
(require 'ox-confluence nil t)

(setq org-hide-leading-stars nil
      org-startup-folded t
      org-cycle-include-plain-lists t
      org-src-fontify-natively t
      org-reverse-note-order t
      org-archive-location (concat my-personal-path "%s_archive.gtd::")

      org-time-clocksum-use-effort-durations t
      ;; refer to variable org-effort-durations

      ;;; GTD related
      ;; refer to [[file:./org-gtd.lgf]] for the usage
      org-agenda-files
      (mapcar (lambda (x) (concat my-personal-path x))
            (list "professional.gtd" "personal.gtd" "part-time.gtd"))
      org-todo-keywords
      '((type "INCOME(i!)" "TODO(t!)" "WAIT(w@)" "HOLD(h!)" "MAYBE(m!)"
              "|" "DONE(d!)" "CANCEL(c@)"))
      org-log-done 'time
      org-agenda-span 'week
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-show-all-dates nil
      org-agenda-start-on-weekday nil
      ;; org-agenda-include-diary t
      org-deadline-warning-days 3
      org-tags-exclude-from-inheritance (list "project")
      org-agenda-custom-commands
      '(("x" "TODO items @ALL@"
         ((todo "INCOME"
                ((org-agenda-overriding-header "In-basket:")
                 (org-agenda-skip-function
                  '(org-agenda-skip-subtree-if 'scheduled))))
          (todo "TODO"
                ;; In case foget to schedule a TODO item (to today if no
                ;; specific target date)
                ((org-agenda-overriding-header "TODOs:")
                 (org-agenda-skip-function
                  '(org-agenda-skip-subtree-if 'scheduled))))
          (agenda)))
        ("y" "TODO items @WORK@"
         ((todo "INCOME"
                ((org-agenda-overriding-header "In-basket:")
                 (org-agenda-skip-function
                  '(org-agenda-skip-subtree-if 'scheduled))))
          (todo "TODO"
                ;; In case foget to schedule a TODO item (to today if no
                ;; specific target date)
                ((org-agenda-overriding-header "TODOs:")
                 (org-agenda-skip-function
                  '(org-agenda-skip-subtree-if 'scheduled))
                 (org-agenda-files '("~/mynotes/personal/professional.gtd"))))
          (agenda "" ((org-agenda-files '("~/mynotes/personal/professional.gtd"))))))
        ("p" "Projects"
         ((tags "project"
                ((org-agenda-overriding-header "Active project list:")
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if
                    'todo
                    '("HOLD" "MAYBE" "DONE" "CANCEL")))))))
        ("w" "PENDING items" ((todo "WAIT")))
        ("h" "HOLD/MAYBE items"
         ((todo "HOLD") (todo "MAYBE")))
        ("b" "My bookmark" search nil
         ((org-agenda-files '("~/mynotes/personal/bookmarks.lgf"))))
        ("c" "Bookmark category" tags nil
         ((org-agenda-files '("~/mynotes/personal/bookmarks.lgf")))))

      org-default-notes-file "~/.income.org" ; overwritten by
                                             ; org-capture-templates
      org-capture-templates
      (list
       (list "w" "work related" 'entry
             (list 'file+olp (concat my-personal-path "professional.gtd") "Misc.")
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
         :base-directory "~/myblog"
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

(add-to-list 'org-structure-template-alist
             '("n" "#+BEGIN_COMMENT\n?\n#+END_COMMENT" ""))

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
              (setq-local org-export-with-toc nil)
              (visual-line-mode 1))))

(defun get-intranet-postamble (plist)
  (let ((title (plist-get plist :title))
        (creator (plist-get plist :creator))
        (time (format-time-string org-html-metadata-timestamp-format)))
    (format
     "
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
     ;; The following variables are defined in private-info.el
     disqus_shortname google_analytic_track_id_blog)))

(defun org-publish-find-date (file)
  "The official one takes toooo much time to run. For me, sort
according to modification time is good enough."
  (if (file-exists-p file) (nth 5 (file-attributes file))
    (error "No such file: \"%s\"" file)))

;; enable ditaa and plantuml etc.
(org-babel-do-load-languages 'org-babel-load-languages
                             '((ditaa . t)
                               (plantuml . t)
                               (dot . t)))
;; don't ask before evaluating
(defun my-org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "ditaa")
           (string= lang "plantuml")
           (string= lang "dot"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate
      org-ditaa-jar-path (concat my-extension-path "ditaa.jar")
      org-plantuml-jar-path (concat my-extension-path "plantuml.jar"))

(defun add-to-project-list ()
  (interactive)
  (let ((tags))
    (save-excursion
      (org-agenda-goto)
      (org-todo 'none)
      (setq tags (org-get-tags))
      (add-to-list 'tags "project")
      (org-set-tags-to tags)
      (message "Task added to the project list"))))

(eval-after-load "org-agenda"
  '(define-key org-agenda-mode-map (kbd "M-p") 'add-to-project-list))

(provide 'lgfang.org)
;;; lgfang.org.el ends here
