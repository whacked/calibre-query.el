
(defun test ()
  (interactive)
  (message (char-to-string (read-char "answer? ")))
 )

(defvar calibre-root-dir (expand-file-name "~/Calibre Library"))
(defvar calibre-db (concat calibre-root-dir "/metadata.db"))
(defvar calibre-text-cache-dir (expand-file-name "~/note/org/.calibre"))
;; CREATE TABLE pdftext ( filepath CHAR(255) PRIMARY KEY, content TEXT );
;; (defvar calibre-text-cache-db (expand-file-name "~/Documents/pdftextcache.db"))
;; (defun calibre-get-cached-pdf-text (pdf-filepath)
;;   (let ((found-text (shell-command-to-string
;;                      (format "%s -separator '\t' '%s' 'SELECT content FROM pdftext WHERE filepath = '%s'" sql-sqlite-program calibre-text-cache-db pdf-filepath))))
;;     (if (< 0 (length found-text))
;;         found-text
;;       (let ((text-extract (shell-command-to-string
;;                            (format "pdftotext '%s' -" pdf-filepath))))
;;         (message "supposed to insert this!")
;;         ))))


;; (shell-command-to-string
;;  (format "%s -separator '\t' '%s' '%s'" sql-sqlite-program calibre-db ".schema books"))


(defun calibre-query (sql-query)
  (interactive)
  (shell-command-to-string
   (format "%s -separator '\t' '%s' '%s'" sql-sqlite-program calibre-db sql-query)))

(defun calibre-build-default-query (whereclause &optional limit)
  (concat "SELECT "
          "b.id, b.author_sort, b.path, d.name, d.format, b.pubdate"
          " FROM data AS d "
          "LEFT OUTER JOIN books AS b ON d.book = b.id "
          whereclause
          (when limit
            (format "LIMIT %s" limit))
          ))

(defun calibre-query-by-field (wherefield argstring)
  (concat "WHERE lower(" wherefield ") LIKE '\\''%%"
          (format "%s" (downcase argstring))
          "%%'\\''"
          ))

(defun calibre-read-query-filter-command ()
  (interactive)
  (let* ((search-string (read-from-minibuffer "search string: "))
         (spl-arg (split-string search-string ":")))
    (if (< 1 (length spl-arg))
        (let* ((command (downcase (first spl-arg)))
               (argstring (second spl-arg))
               (wherefield
                (cond ((string= "a" (substring command 0 1))
                       "b.author_sort")
                      ((string= "t" (substring command 0 1))
                       "b.title")
                      )))
          (calibre-query-by-field wherefield argstring))
      (format "WHERE lower(b.author_sort) LIKE '\\''%%%s%%'\\'' OR lower(b.title) LIKE '\\''%%%s%%'\\''"
              (downcase search-string) (downcase search-string)))))

(defun quote-% (str)
  (replace-regexp-in-string "%" "%%" str))

(defun calibre-list ()
  (interactive)
  (message (quote-% (calibre-query
            (concat "SELECT b.path FROM books AS b "
                    (calibre-read-query-filter-command))))))

(defun calibre-get-cached-pdf-text (pdf-filepath)
  (let ((found-text (shell-command-to-string
                     (format "%s -separator '\t' '%s' 'SELECT content FROM pdftext WHERE filepath = '%s'" sql-sqlite-program calibre-text-cache-db pdf-filepath))))
    (if (< 0 (length found-text))
        found-text
      (let ((text-extract (shell-command-to-string
                           (format "pdftotext '%s' -" pdf-filepath))))
        (message "supposed to insert this!")
        ))))

(defun calibre-open-citekey ()
  (interactive)
  (if (word-at-point)
   (let ((spl-key (split-string (word-at-point) "id")))
     (when (< 1 (length spl-key))
       (let* ((calibre-id (first (last spl-key))))
         (if (< 0 (string-to-number calibre-id))
           (calibre-find (calibre-build-default-query (format "WHERE b.id = %s" calibre-id)))
           (message "no candidate id found here")))))
   (message "nothing at point!")))

(defun calibre-make-text-cache-path-from-citekey (citekey)
  (concat calibre-text-cache-dir "/" citekey "/text.org"))
(defun calibre-make-note-cache-path-from-citekey (citekey)
  (concat calibre-text-cache-dir "/" citekey "/note.org"))

;; TODO apple
;; write general query result destructurer and replace below
;; (let* ((spl-query-result (split-string query-result "\t"))...
;; with it

(defun calibre-find (&optional custom-query)
  (interactive)
  (let* ((sql-query (if custom-query
                        custom-query
                      (calibre-build-default-query (calibre-read-query-filter-command) 1)))
         (query-result (calibre-query sql-query))
         )
    (if (= 0 (length query-result))
        (message "nothing found.")
      ;; FIXME apple
      (let* ((spl-query-result (split-string query-result "\t"))
             (calibre-id   (nth 0 spl-query-result))
             (author-sort  (nth 1 spl-query-result))
             (book-dir     (nth 2 spl-query-result))
             (book-name    (nth 3 spl-query-result))
             (book-format  (downcase (nth 4 spl-query-result)))
             (book-pubdate (nth 5 spl-query-result))
             (found-file-path (concat calibre-root-dir "/" book-dir "/" book-name "." book-format))
             (xoj-file-path   (concat calibre-root-dir "/" book-dir "/" book-name ".xoj"))
             (citekey (concat (replace-regexp-in-string (first (split-string author-sort "[&,?]")) " " "") (substring book-pubdate 0 4) "id" calibre-id))
             (description-text ". [o]pen open[O]ther open[e]xt [c]itekey [i]nsertId [p]ath [t]ext [q]uit")
             )
        (if (file-exists-p found-file-path)
            (let ((opr (char-to-string (read-char
                                        (concat "found " book-name description-text)))))
              (cond ((string= "o" opr)
                     (find-file-other-window found-file-path))
                    ((string= "O" opr)
                     (find-file-other-frame found-file-path))
                    ((string= "e" opr)
                     (start-process "xournal-process" "*Messages*" "xournal" (if (file-exists-p xoj-file-path)
                                                                                 xoj-file-path
                                                                               found-file-path))
                     )
                    ((string= "c" opr)
                     (insert citekey))
                    ((string= "p" opr)
                     (insert found-file-path "\n"))
                    ;; query for identifiers
                    ;; FIXME apple
                    ((string= "i" opr)
                     (insert
                      (progn
                        ;; stupidly just insert the plain text result
                        (calibre-query (concat "SELECT "
                                               "idf.type, idf.val "
                                               "FROM identifiers AS idf "
                                               (format "WHERE book = %s"
                                                       calibre-id))))))
                    ((string= "t" opr)
                     (let ((cached-text-path (calibre-make-text-cache-path-from-citekey citekey))
                           (cached-note-path (calibre-make-note-cache-path-from-citekey citekey)))
                       (if (file-exists-p cached-text-path)
                           (progn
                             (find-file-other-window cached-text-path)
                             (when (file-exists-p cached-note-path)
                               (split-window-horizontally)
                               (find-file-other-window cached-note-path)
                               (org-open-link-from-string "[[note]]")
                               (forward-line 2)))
                         (let* ((pdftotext-out-buffer (get-buffer-create (format "pdftotext-extract-%s" calibre-id))))
                           (set-buffer pdftotext-out-buffer)
                           (insert (shell-command-to-string (concat "pdftotext '" found-file-path "' -")))
                           (switch-to-buffer-other-window pdftotext-out-buffer)
                           (beginning-of-buffer)
                           ))))
                    (t
                     (message "quit"))
                    )
              )
          (message "didn't find that file"))))
    ))

(global-set-key "\C-cK" 'calibre-open-citekey)

(provide 'calibre-mode)
