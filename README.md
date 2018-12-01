# Install

Put the following lines in .emacs.el (or equivalent). `sql-sqlite-program` should be discovered by `sql.el`, but you can override it

    (require 'calibre-mode)
    (setq sql-sqlite-program "/usr/bin/sqlite3")
    
If your Calibre library is not in the default location, `calibre--find-library-filepath` will try to find it; alternatively, set `calibre-root-dir` explicitly:

    (setq calibre-root-dir (expand-file-name "~/Calibre Library"))

if somehow your calibre library is not `metadata.db`, override the full db path directly:

    (setq calibre-db (concat calibre-root-dir "/metadata.db"))

# Usage

## M-x calibre-list

Prompts for a search string and displays all records which match on title or author.

## M-x calibre-find

Prompts for a search string.  Matches the first record which would have been matched by calibre-list.  Offers several options for opening the work.
