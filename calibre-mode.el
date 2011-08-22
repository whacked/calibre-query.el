
(defun test ()
  (interactive)
  (message (char-to-string (read-char "answer? ")))
 )

(defvar calibre-root-dir (expand-file-name "~/Calibre Library"))
(defvar calibre-db (concat calibre-root-dir "/metadata.db"))

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

(defun calibre-list ()
  (interactive)
  (message (calibre-query
            (concat "SELECT b.path FROM books AS b "
                    (calibre-read-query-filter-command)))))

(defun calibre-find ()
  (interactive)
  (let* ((sql-query (calibre-build-default-query (calibre-read-query-filter-command) 1))
         (query-result (calibre-query sql-query))
         (spl-query-result (split-string (sql-chomp query-result) "\t"))
         (calibre-id   (nth 0 spl-query-result))
         (author-sort  (nth 1 spl-query-result))
         (book-dir     (nth 2 spl-query-result))
         (book-name    (nth 3 spl-query-result))
         (book-format  (downcase (nth 4 spl-query-result)))
         (book-pubdate (nth 5 spl-query-result))
         (found-file-path (concat calibre-root-dir "/" book-dir "/" book-name "." book-format))
         (xoj-file-path   (concat calibre-root-dir "/" book-dir "/" book-name ".xoj"))
         )
    (if (file-exists-p found-file-path)
        (let ((opr (char-to-string (read-char
                                    (concat "found " book-name ". [o]pen open[O]ther open[e]xt [c]itekey [p]ath [q]uit")))))
          (cond ((string= "o" opr)
                 (find-file-other-window found-file-path))
                ((string= "O" opr)
                 (find-file-other-frame found-file-path))
                ((string= "e" opr)
                 (shell-command (format "xournal '%s' &" (if (file-exists-p xoj-file-path)
                                                           xoj-file-path
                                                           found-file-path))))
                ((string= "c" opr)
                 (insert (first (split-string author-sort ",")) (substring book-pubdate 0 4) "id" calibre-id))
                ((string= "p" opr)
                 (insert found-file-path "\n"))
                (t
                 (message "quit"))
                )
          )
      (message "didn't find that file"))
    ))
