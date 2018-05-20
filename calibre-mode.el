(require 'cl)

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
                          #'(lambda (dir) (let ((outdir (concat dir "/" mime-appname))) (if (file-exists-p outdir) outdir)))
                          '("~/.local/share/applications" "/usr/local/share/applications" "/usr/share/applications")))))
            "|head -1|awk '{print $1}'|cut -d '=' -f 2"))))
        ((eq system-type 'windows-nt)
         ;; based on
         ;; http://stackoverflow.com/questions/501290/windows-equivalent-of-the-mac-os-x-open-command
         ;; but no idea if it actuall works
         "start")
        ((eq system-type 'darwin)
         "open")
        (t (message "unknown system!?"))))

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
  (let* ((default-string (if mark-active (calibre-chomp (buffer-substring (mark) (point)))))
         ;; prompt &optional initial keymap read history default
         (search-string (read-string (format "Search Calibre for%s: "
                                             (if default-string
                                                 (concat " [" default-string "]")
                                               "")) nil nil default-string))
         (spl-arg (split-string search-string ":")))
    (if (and (< 1 (length spl-arg))
             (= 1 (length (first spl-arg))))
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
      (let ((where-string
             (replace-regexp-in-string
              ;; capture all up to optional "etal" into group \1
              ;; capture 4 digits of date          into group \2
              ;; capture first word in title       into group \3
              "\\b\\([^ :;,.]+?\\)\\(?:etal\\)?\\([[:digit:]]\\\{4\\\}\\)\\(.*?\\)\\b"
              "WHERE lower(b.author_sort) LIKE '\\\\''%\\1%'\\\\'' AND lower(b.title) LIKE '\\\\''\\3%'\\\\''AND b.pubdate >= '\\\\''\\2-01-01'\\\\'' AND b.pubdate <= '\\\\''\\2-12-31'\\\\'' LIMIT 1" (word-at-point))))
        (mark-word)
        (calibre-find (calibre-build-default-query where-string)))
    (message "nothing at point!")))

(defun getattr (my-alist key)
  (cadr (assoc key my-alist)))

(defun calibre-make-citekey (calibre-res-alist)
  "return some kind of a unique citation key for BibTeX use"
  (let* ((stopword-list '("the" "on" "a"))
         (spl (split-string (calibre-chomp (getattr calibre-res-alist :author-sort)) "&"))
         (first-author-lastname (first (split-string (first spl) ",")))
         (first-useful-word-in-title
          ;; ref fitlering in http://www.emacswiki.org/emacs/ElispCookbook#toc39
          (first (delq nil
                  (mapcar
                   (lambda (token) (if (member token stopword-list) nil token))
                   (split-string (downcase (getattr calibre-res-alist :book-title)) " "))))))
    (concat
     (downcase (replace-regexp-in-string  "\\W" "" first-author-lastname))
     (if (< 1 (length spl)) "etal" "")
     (substring (getattr calibre-res-alist :book-pubdate) 0 4)
     (downcase (replace-regexp-in-string  "\\W.*" "" first-useful-word-in-title)))))

(defun mark-aware-copy-insert (content)
  "copy to clipboard if mark active, else insert"
  (if mark-active
      (progn (kill-new content)
             (deactivate-mark))
    (insert content)))

;; Define the result handlers here in the form of (hotkey description
;; handler-function) where handler-function takes 1 alist argument
;; containing the result record.
(setq calibre-handler-alist
      '(("o" "open"
         (lambda (res) (find-file-other-window (getattr res :file-path))))
        ("O" "open other frame"
         (lambda (res) (find-file-other-frame (getattr res :file-path))))
        ("v" "open with default viewer"
         (lambda (res)
           (start-process "shell-process" "*Messages*" calibre-default-opener
                          (getattr res :file-path))))
        ("x" "open with xournal"
         (lambda (res)
           (start-process "xournal-process" "*Messages*" "xournal"
                          (let ((xoj-file-path (concat calibre-root-dir "/"
                                                       (getattr res :book-dir)
                                                       "/"
                                                       (getattr res :book-name)
                                                       ".xoj")))
                            (if (file-exists-p xoj-file-path)
                                xoj-file-path
                              (getattr res :file-path))))))
        ("s" "insert calibre search string"
         (lambda (res) (mark-aware-copy-insert
                        (concat "title:\"" (getattr res :book-title) "\""))))
        ("c" "insert citekey"
         (lambda (res) (mark-aware-copy-insert (calibre-make-citekey res))))
        ("i" "get book information (SELECT IN NEXT MENU) and insert"
         (lambda (res)
           (let ((opr
                  (char-to-string
                   (read-char
                    ;; render menu text here
                    (concat "What information do you want?\n"
                            "i : values in the book's `Ids` field (ISBN, DOI...)\n"
                            "d : pubdate\n"
                            "a : author list\n")))))
             (cond ((string= "i" opr)
                    ;; stupidly just insert the plain text result
                    (mark-aware-copy-insert
                     (calibre-chomp
                      (calibre-query
                       (concat "SELECT "
                               "idf.type, idf.val "
                               "FROM identifiers AS idf "
                               (format "WHERE book = %s" (getattr res :id)))))))
                   ((string= "d" opr)
                    (mark-aware-copy-insert
                     (substring (getattr res :book-pubdate) 0 10)))
                   ((string= "a" opr)
                    (mark-aware-copy-insert
                     (calibre-chomp (getattr res :author-sort))))
                   (t
                    (deactivate-mark)
                    (message "cancelled"))))))
        ("p" "insert file path"
         (lambda (res) (mark-aware-copy-insert (getattr res :file-path))))
        ("t" "insert title"
         (lambda (res) (mark-aware-copy-insert (getattr res :book-title))))
        ("g" "insert org link"
         (lambda (res)
           (insert (format "[[%s][%s]]"
                           (getattr res :file-path)
                           (concat (calibre-chomp (getattr res :author-sort))
                                   ", "
                                   (getattr res :book-title))))))
        ("j" "insert entry json"
         (lambda (res) (mark-aware-copy-insert (json-encode res))))
        ("X" "open as plaintext in new buffer (via pdftotext)"
         (lambda (res)
           (let* ((citekey (calibre-make-citekey res)))
             (let* ((pdftotext-out-buffer
                     (get-buffer-create
                      (format "pdftotext-extract-%s" (getattr res :id)))))
               (set-buffer pdftotext-out-buffer)
               (insert (shell-command-to-string (concat "pdftotext '"
                                                        (getattr res :file-path)
                                                        "' -")))
               (switch-to-buffer-other-window pdftotext-out-buffer)
               (beginning-of-buffer)))))
        ("q" "(or anything else) to cancel"
         (lambda (res)
           (deactivate-mark)
           (message "cancelled")))))

(defun calibre-file-interaction-menu (calibre-item)
  (if (file-exists-p (getattr calibre-item :file-path))
      (let ((opr (char-to-string (read-char
                                  ;; render menu text here
                                  (concat (format "(%s) [%s] found, what do?\n"
                                                  (getattr calibre-item :book-format)
                                                  (getattr calibre-item :book-name))
                                          (mapconcat #'(lambda (handler-list)
                                                         (let ((hotkey      (elt handler-list 0))
                                                               (description (elt handler-list 1))
                                                               (handler-fn  (elt handler-list 2)))
                                                           ;; ULGY BANDAID HACK
                                                           ;; replace "insert" with "copy to clipboard" if mark-active
                                                           (format " %s :   %s"
                                                                   hotkey
                                                                   (if mark-active
                                                                       (replace-regexp-in-string "insert \\(.*\\)" "copy \\1 to clipboard" description)
                                                                     description)))
                                                         ) calibre-handler-alist "\n"))))))
        (funcall
         (elt (if (null (assoc opr calibre-handler-alist)) (assoc "q" calibre-handler-alist)
                (assoc opr calibre-handler-alist)) 2) calibre-item))
    (message "didn't find that file")))

(defun calibre-format-selector-menu (calibre-item-list)
  (let* ((chosen-number
          (char-to-string
           (read-char
            ;; render menu text here
            (let ((num-result (length calibre-item-list)))
              (concat (format "%d matches. Pick target:\n" num-result)
                      (mapconcat #'(lambda (idx)
                                     (let ((item (nth idx calibre-item-list)))
                                       (format "   (%s) %s %s %s "
                                               (1+ idx)
                                               (getattr item :author-sort)
                                               (getattr item :book-title)
                                               (getattr item :book-format))))
                                 (number-sequence 0 (1- num-result))
                                 "\n"))))))
         (chosen-item (nth (1- (string-to-int chosen-number)) calibre-item-list)))
    (calibre-file-interaction-menu chosen-item)))

(defun calibre-find (&optional custom-query)
  (interactive)
  (let* ((sql-query (if custom-query
                        custom-query
                      (calibre-build-default-query (calibre-read-query-filter-command))))
         (query-result (calibre-query sql-query))
         (line-list (split-string (calibre-chomp query-result) "\n"))
         (num-result (length line-list)))
    (if (= 0 num-result)
        (progn
          (message "nothing found.")
          (deactivate-mark))
      (let ((res-list (mapcar '(lambda (line) (calibre-query-to-alist line)) line-list)))
        (if (= 1 (length res-list))
            (calibre-file-interaction-menu (car res-list))
          (calibre-format-selector-menu res-list))))))

(global-set-key "\C-cK" 'calibre-open-citekey)

(provide 'calibre-mode)
