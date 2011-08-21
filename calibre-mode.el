




(defvar calibre-root-dir (expand-file-name "~/Calibre Library"))
(defvar calibre-db (concat calibre-root-dir "/metadata.db"))
(defun calibre-find ()
  (interactive)
  (let* ((search-string (read-from-minibuffer "search string: "))
         (spl-arg (split-string search-string ":"))
         (whereclause
          (if (< 1 (length spl-arg))
              (let* ((command (downcase (first spl-arg)))
                     (argstring (second spl-arg))
                     (wherefield
                      (cond ((string= "a" (substring command 0 1))
                             "b.author_sort")

                            ((string= "t" (substring command 0 1))
                             "b.title")
                            )))
                (concat "WHERE lower(" wherefield ") LIKE '\\''%%"
                        (format "%s" (downcase argstring))
                        "%%'\\''"
                        )
                )
            (format "WHERE lower(b.author_sort) LIKE '\\''%%%s%%'\\'' OR lower(b.title) LIKE '\\''%%%s%%'\\''"
                    (downcase search-string) (downcase search-string))))
         (sql-query (concat "SELECT b.path, d.name, d.format FROM data AS d "
                            "LEFT OUTER JOIN books AS b ON d.book = b.id "
                            whereclause
                            "LIMIT 1"
                            )
                    )
         

         )
    (let* ((query-result (shell-command-to-string
                          (format "%s -separator '\t' '%s' '%s'" sql-sqlite-program calibre-db sql-query)))
           (spl-query-result (split-string query-result "\t"))
           )
      (insert (concat calibre-root-dir "/" (first spl-query-result) "/" (second spl-query-result) "." (downcase (nth 2 spl-query-result))))
      )
    ))


(let ((sqlite-openandclose-callback (defun sqlite-openandclose-callback (s)
                                      (when (member 'sqlite-openandclose-callback comint-preoutput-filter-functions)
                                        (remove-hook 'comint-preoutput-filter-functions 'sqlite-openandclose-callback))
                                      s))
      (ignore
       (add-hook 'comint-preoutput-filter-functions 'sqlite-openandclose-callback))
      (temp-sqlite-process-buffer (buffer-name (apply 'make-comint "my-sqlite-process"  sql-sqlite-program  nil `(,sql-sqlite-db ))))
      (temp-output-buffer (buffer-name (get-buffer-create "*my-sqlite-output*")))
      (my-query "select id from subject;") ;
      
      )
  )
