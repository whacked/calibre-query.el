# Install

Put the following lines in .emacs.el (or equivalent).  Change the paths as necessary.  There is no default for sql-sqlite-program.

    (require 'calibre-mode)
    (setq sql-sqlite-program "/usr/bin/sqlite3")
    
If your Calibre library is not in the default location, set calibre-root-dir, also:

    (setq calibre-root-dir (expand-file-name "~/Calibre Library"))
    (setq calibre-db (concat calibre-root-dir "/metadata.db"))

# Getting Started

## M-x calibre-list

Prompts for a search string and displays all records which match on title or author.

## M-x calibre-find

Prompts for a search string.  Matches the first record which would have been matched by calibre-list.  Offers several options for opening the work.
