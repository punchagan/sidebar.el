;;; sidebar-elfeed.el --- Sidebar for elfeed -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Puneeth Chaganti
;;
;; Author: Puneeth Chaganti <http://github.com/punchagan>
;; Created: May 29, 2020
;; Modified: May 29, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/punchagan/sidebar-elfeed
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'dash)
(require 'elfeed)
(require 'sidebar-utils)
(require 'url-parse)
(require 'subr-x)

(declare-function elfeed-search-set-filter "ext:elfeed.el")
(declare-function sidebar-init-mode "ext:sidebar.el")
(declare-function sidebar-find-file-from-line "ext:sidebar.el")
(declare-function sidebar-open "ext:sidebar.el")
(declare-function icons-in-terminal "ext:sidebar.el")
(declare-function url-host "ext:url-parse.el")
(declare-function hash-table-values "ext:subr-x.el")
(defvar elfeed-feeds)

(defgroup sidebar-elfeed nil
  "Sidebar mode to view its tags and feeds."
  :group 'tools
  :group 'convenience
  :group 'sidebar
  :link '(custom-manual "(sidebar-elfeed) Top")
  :link '(info-link "(sidebar-elfeed) Customizing"))

(defcustom sidebar-elfeed-width 30
  "Width of the sidebar with elfeed."
  :type 'integer
  :group 'sidebar-elfeed)

(defcustom sidebar-elfeed-autostart t
  "If non-nil, sidebar-elfeed is started with elfeed.
More precisely, it is started when `elfeed' is called."
  :type 'boolean
  :group 'sidebar-elfeed)

(defcustom sidebar-elfeed-feed-icon 'oct_rss
  "Icon to use with feeds that have no unreads.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar-elfeed)

(defcustom sidebar-elfeed-unread-feed-icon 'fa_rss_square
  "Icon to use with feeds with unreads.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar-elfeed)

(defcustom sidebar-elfeed-filter-extra "#10 +unread"
  "Extra search filter params to set along with feed url."
  :type 'symbol
  :group 'sidebar-elfeed)

(defun sidebar-elfeed? ()
  "Return non-nil if we have to use `sidebar-elfeed-mode' on the sidebar creation."
  (prog1
      (with-selected-window (sidebar-get window-origin)
        (or (sidebar-get elfeed-force)
            (derived-mode-p 'elfeed-show-mode
                            'elfeed-search-mode)))
    (sidebar-set elfeed-force nil)))

(sidebar-content-provider elfeed (&rest _)
  ;; List of items processed by sidebar-elfeed-item-builder
  (sidebar-set elfeed-feeds-count (length elfeed-feeds))
  (sidebar-elfeed-compute-unreads)
  )

(defun sidebar-elfeed-compute-unreads ()
  "Compute all the unreads per feed."
  (with-current-buffer (elfeed-search-buffer)
    (elfeed-search-set-filter "+unread")
    (elfeed-search--update-list)
    (cl-loop with feeds = (make-hash-table :test 'equal)
           for entry in elfeed-search-entries
           for feed = (elfeed-entry-feed entry)
           for url = (elfeed-feed-url feed)
           for feed-unread-count = (gethash url feeds 0)
           count entry into entry-count
           count (elfeed-tagged-p 'unread entry) into unread-count
           do (puthash url (1+ feed-unread-count) feeds)
           finally
           (cl-return
            (cl-mapcar (lambda (f)
                         (list (cons 'feed f)
                                         (cons 'unread (gethash (elfeed-feed-url f) feeds))))
                       (hash-table-values elfeed-db-feeds))))))

(defun sidebar-elfeed-item-builder (item)
  "Return an association list from ITEM.
Function similar to `sidebar-file-struct' adapted for elfeed data."
  (-let* (((&alist 'feed feed 'unread unread) item))
    (list (cons 'title (or (elfeed-feed-title feed) (elfeed-meta feed :title)))
          (cons 'unread unread)
          (cons 'url (elfeed-feed-url feed))
          (cons 'type 'feed)
          (cons 'line 0))))

(sidebar-print-function elfeed (item)
  "ITEM."
  (-let* (((&alist 'title title 'type type 'unread unread) item))
    (if (eq type 'separator)
        (ignore (overlay-put (make-overlay (point) (point)) 'after-string "\n"))
      (concat
       " "
       (pcase type
         ('feed (let ((icon (if unread sidebar-elfeed-unread-feed-icon
                              sidebar-elfeed-feed-icon)))
                  (icons-in-terminal icon :height 1.0))))
       " "
       (pcase type
         ('feed (if unread (format "%s (%s)" title unread)
                  title)))
       "\n"))))

(defun sidebar-elfeed-make-header ()
  "Return the string to insert in the sidebar header."
  (let* ((context "elfeed"))
    (concat
     " "
     (icons-in-terminal 'oct_book
                        :face 'sidebar-icon-header-project
                        :background (face-background 'sidebar-header-line nil t)
                        :raise -0.07
                        :height 1.3)
     " "
     (propertize
      (concat (upcase (substring context 0 1)) (substring context 1))
      'display '(raise 0.12)))))

(defun sidebar-elfeed-make-modeline-left ()
  "Return the string to insert in the modeline (left side)."
  " elfeed")

(defun sidebar-elfeed-make-modeline-right ()
  "Return the string to insert in the modeline (right side)."
  (concat
   (number-to-string (sidebar-get elfeed-feeds-count))
   " "
   (icons-in-terminal sidebar-elfeed-feed-icon)
   "  "))

(defun sidebar-elfeed-open-line ()
  "Open the feed on the current line."
  (interactive)
  (-let* (((&alist 'url url 'type type) (sidebar-find-file-from-line)))
    (select-window (sidebar-get window-origin))
    (pcase type
      ('feed (sidebar-elfeed-open-feed url)))))

(defun sidebar-elfeed-open-feed (url)
  "Filter to the selected feed based on URL."
  (elfeed-search-set-filter (format "=%s %s" url sidebar-elfeed-filter-extra))
  (elfeed-search-first-entry))

(defun sidebar-elfeed-update-feed ()
  "Update the feed on the current line."
  (interactive)
  (-let* (((&alist 'url url 'type type) (sidebar-find-file-from-line)))
    (pcase type
      ('feed (elfeed-update-feed url)))))

(defun sidebar-elfeed-quit (&rest _)
  "Function called when elfeed quits.
It removes the sidebar."
  (interactive)
  (advice-remove 'elfeed-search-quit-window 'sidebar-elfeed-quit)
  (ignore-errors (kill-buffer (sidebar-cons-buffer-name)))
  (select-window (sidebar-get window-origin)))

(defun sidebar-elfeed-autostart (&rest _)
  "Auto start sidebar-elfeed.

If variable `sidebar-elfeed-autostart' is non-nil, sidebar-elfeed
is open automatically with elfeed."
  (when sidebar-elfeed-autostart
    (sidebar-set elfeed-force t)
    (run-with-timer 0.1 nil #'sidebar-open)))

(defvar sidebar-elfeed-mode-map nil
  "Keymap used with ‘sidebar-elfeed-mode’.")

(unless sidebar-elfeed-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "q") 'sidebar-elfeed-quit)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "g") 'sidebar-refresh)
    (define-key map (kbd "G") 'elfeed-search-fetch)
    (define-key map (kbd "RET") 'sidebar-elfeed-open-line)
    (define-key map (kbd "U") 'sidebar-elfeed-update-feed)
    (define-key map (kbd "<right>") 'sidebar-adjust-window-width)
    (define-key map (kbd "<left>") 'sidebar-reset-window-width)
    (define-key map (kbd "?") 'sidebar-help)
    (setq sidebar-elfeed-mode-map map)))

(advice-add 'elfeed :after 'sidebar-elfeed-autostart)

(define-derived-mode sidebar-elfeed-mode special-mode "Sidebar-elfeed"
  "Major mode for Sidebar-elfeed.

\\{sidebar-elfeed-mode-map}"
  ::group sidebar-elfeed

  (make-local-variable 'post-command-hook)
  (make-local-variable 'pre-command-hook)

  (sidebar-set default-width sidebar-elfeed-width)

  (advice-add 'elfeed-search-quit-window :before 'sidebar-elfeed-quit)

  (sidebar-init-mode)

  (add-hook 'post-command-hook 'sidebar-post-command t)
  (add-hook 'delete-frame-functions 'sidebar-delete-buffer-on-kill)
  (add-hook 'before-make-frame-hook 'sidebar-before-make-frame-hook)

  (sidebar-set-window))

(provide 'sidebar-elfeed)

;;; sidebar-elfeed.el ends here
