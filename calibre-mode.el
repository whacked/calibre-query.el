(defvar calibre-root-dir (expand-file-name "~/Calibre Library"))
(defvar calibre-db (concat calibre-root-dir "/metadata.db"))

(defun calibre-chomp (s)
  (replace-regexp-in-string "[\s\n]+$" "" s))

(defvar calibre-default-opener
  (cond ((eq system-type 'gnu/linux)
         ;; HACK!
         ;; "xdg-open"
         ;; ... but xdg-open doesn't seem work as expected! (process finishes but program doesn't launch)
         ;; appears to be related to http://lists.gnu.org/archive/html/emacs-devel/2009-07/msg00279.html
         ;; you're better off replacing it with your exact program...
         ;; here we run xdg-mime to figure it out for *pdf* only. So this is not general!
         (calibre-chomp
          (shell-command-to-string
           (concat
            "grep Exec "
            (first
             ;; attempt for more linux compat, ref
             ;; http://askubuntu.com/questions/159369/script-to-find-executable-based-on-extension-of-a-file
             ;; here we try to find the location of the mimetype opener that xdg-mime refers to.
             ;; it works for okular (Exec=okular %U %i -caption "%c"). NO IDEA if it works for others!
             (delq nil (let ((mime-appname (calibre-chomp (replace-regexp-in-string
                                                           "kde4-" "kde4/"
                                                           (shell-command-to-string "xdg-mime query default application/pdf")))))

                         (mapcar
                          '(lambda (dir) (let ((outdir (concat dir "/" mime-appname))) (if (file-exists-p outdir) outdir)))
                          '("~/.local/share/applications" "/usr/local/share/applications" "/usr/share/applications")))))
            "|awk '{print $1}'|cut -d '=' -f 2"))))
        ((eq system-type 'windows-nt)
         ;; based on
         ;; http://stackoverflow.com/questions/501290/windows-equivalent-of-the-mac-os-x-open-command
         ;; but no idea if it actuall works
         "start")
        ((eq system-type 'darwin)
         "open")
        (t (message "unknown system!?"))))

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

(defun calibre-query-to-alist (query-result)
  "builds alist out of a full calibre-query query record result"
  (if query-result
      (let ((spl-query-result (split-string (calibre-chomp query-result) "\t")))
        `((:id                     ,(nth 0 spl-query-result))
          (:author-sort            ,(nth 1 spl-query-result))
          (:book-dir               ,(nth 2 spl-query-result))
          (:book-name              ,(nth 3 spl-query-result))
          (:book-format  ,(downcase (nth 4 spl-query-result)))
          (:book-pubdate           ,(nth 5 spl-query-result))
          (:book-title             ,(nth 6 spl-query-result))
          (:file-path    ,(concat (file-name-as-directory calibre-root-dir)
                                  (file-name-as-directory (nth 2 spl-query-result))
                                  (nth 3 spl-query-result) "." (downcase (nth 4 spl-query-result))))))))

(defun calibre-build-default-query (whereclause &optional limit)
  (concat "SELECT "
          "b.id, b.author_sort, b.path, d.name, d.format, b.pubdate, b.title"
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

(defun getattr (my-alist key)
  (cadr (assoc key my-alist)))

(defun calibre-make-citekey (calibre-res-alist)
  "return some kind of a unique citation key for BibTeX use"
  (concat
   (replace-regexp-in-string (first (split-string (getattr calibre-res-alist :author-sort) "[&,?]")) " " "")
   (substring (getattr calibre-res-alist :book-pubdate) 0 4) "id" (getattr calibre-res-alist :id)))

;; define the result handlers here in the form of (hotkey description handler-function)
;; where handler-function takes 1 alist argument containing the result record
(setq calibre-handler-alist '(("o" "open"
                               (lambda (res) (find-file-other-window (getattr res :file-path))))
                              ("O" "open other frame"
                               (lambda (res) (find-file-other-frame (getattr res :file-path))))
                              ("v" "open with default viewer"
                               (lambda (res)
                                 (start-process "shell-process" "*Messages*" calibre-default-opener (getattr res :file-path))))
                              ("x" "open with xournal"
                               (lambda (res) (start-process "xournal-process" "*Messages*" "xournal"
                                                            (let ((xoj-file-path (concat calibre-root-dir "/" (getattr res :book-dir) "/" (getattr res :book-name) ".xoj")))
                                                              (if (file-exists-p xoj-file-path)
                                                                  xoj-file-path
                                                                (getattr res :file-path))))))
                              ("s" "insert calibre search string"
                               (lambda (res)
                                 (insert (concat "title:\"" (getattr res :book-title) "\""))))
                              ("c" "insert citekey"
                               (lambda (res)
                                 (insert (calibre-make-citekey res))))
                              ("i" "insert values in the book's `Ids` field (ISBN, DOI...)"
                               (lambda (res)
                                 ;; stupidly just insert the plain text result
                                 (insert
                                  (calibre-chomp
                                   (calibre-query (concat "SELECT "
                                                          "idf.type, idf.val "
                                                          "FROM identifiers AS idf "
                                                          (format "WHERE book = %s" (getattr res :id))))))))
                              ("p" "insert file path"
                               (lambda (res) (insert (getattr res :file-path))))
                              ("t" "open as plaintext in new buffer (via pdftotext)"
                               (lambda (res)
                                 (let* ((citekey (calibre-make-citekey res))
                                        (cached-text-path (calibre-make-text-cache-path-from-citekey citekey))
                                        (cached-note-path (calibre-make-note-cache-path-from-citekey citekey)))
                                   (if (file-exists-p cached-text-path)
                                       (progn
                                         (find-file-other-window cached-text-path)
                                         (when (file-exists-p cached-note-path)
                                           (split-window-horizontally)
                                           (find-file-other-window cached-note-path)
                                           (org-open-link-from-string "[[note]]")
                                           (forward-line 2)))
                                     (let* ((pdftotext-out-buffer (get-buffer-create (format "pdftotext-extract-%s" (getattr res :id)))))
                                       (set-buffer pdftotext-out-buffer)
                                       (insert (shell-command-to-string (concat "pdftotext '" (getattr res :file-path) "' -")))
                                       (switch-to-buffer-other-window pdftotext-out-buffer)
                                       (beginning-of-buffer))))))
                              ("T" "insert title"
                               (lambda (res) (insert (getattr res :book-title))))
                              ("q" "(or anything else) to cancel"
                               (lambda (res) (message "cancelled")))))

(defun calibre-find (&optional custom-query)
  (interactive)
  (let* ((sql-query (if custom-query
                        custom-query
                      (calibre-build-default-query (calibre-read-query-filter-command) 1)))
         (query-result (calibre-query sql-query)))
    (if (= 0 (length query-result))
        (message "nothing found.")
      (let ((res (calibre-query-to-alist query-result)))
        (if (file-exists-p (getattr res :file-path))
            (let ((opr (char-to-string (read-char
                                        ;; render menu text here
                                        (concat "[" (getattr res :book-name) "] found ... what do?\n"
                                                (mapconcat '(lambda (handler-list)
                                                              (let ((hotkey      (elt handler-list 0))
                                                                    (description (elt handler-list 1))
                                                                    (handler-fn  (elt handler-list 2)))
                                                                (format " %s :   %s" hotkey description))
                                                              ) calibre-handler-alist "\n"))))))
              (funcall
               (elt (if (null (assoc opr calibre-handler-alist)) (assoc "q" calibre-handler-alist)
                      (assoc opr calibre-handler-alist)) 2) res))
          (message "didn't find that file"))))))

(global-set-key "\C-cK" 'calibre-open-citekey)

(provide 'calibre-mode)
