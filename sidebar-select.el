;;; sidebar-select.el --- sidebar-select

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/sidebar.el
;; Keywords: project, sidebar, projectile, file explorer
;; Version: 0.0.1
;; Package-Requires: ((dash "2.13.0") (projectile "0.11.0"))

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;

;;; Code:

(require 's)
(require 'dash)
(require 'icons-in-terminal nil t)
(require 'sidebar-utils)

(defface sidebar-select-line-gui-face
  '((t :foreground "white"
       :box nil
       :background "#1A237E"))
  "Face used for the current line."
  :group 'sidebar-gui-faces)

(defface sidebar-select-header-gui-face
  '((t :foreground "white"
       :background "#1A237E"
       :overline "#1A237E"))
  "Face used for the headers."
  :group 'sidebar-gui-faces)

(defface sidebar-select-line-terminal-face
  '((t :foreground "white"
       :box nil
       :background "#005fff"))
  "Face used for the current line."
  :group 'sidebar-terminal-faces)

(defface sidebar-select-header-terminal-face
  '((t :foreground "white"
       :background "#005fff"
       :overline "#005fff"))
  "Face used for the headers."
  :group 'sidebar-terminal-faces)

(defcustom sidebar-select-icon-left-header 'myicons_0006
  "Icon to use on the left of the header.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-select-icon-right-header 'myicons_0008
  "Icon to use on the right of the header.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-select-icon-before-window 'oct_device_desktop
  "Icon to use before the window name.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-select-icon-before-directory 'oct_file_directory
  "Icon to use before the directory name.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defvar sidebar-select-buffer-name " SIDEBAR-SELECT")

(defvar sidebar-select-line-other
  nil)

(defvar sidebar-select-windows-count
  0)

(defvar sidebar-select-header
  "")

(defvar sidebar-select-callback
  nil)

(defvar sidebar-select-args
  nil)

(defvar sidebar-select-mapping
  nil)

(defvar sidebar-select-window
  nil)

(defvar sidebar-select-window-width
  "")

(defvar-local sidebar-select-last-line 0)

(defun sidebar-select-set-header (string spaces raise height)
  "."
  (let* ((width (window-width))
	 (space-to-add (- (/ width 2) spaces))
	 (space-at-the-end (- (/ width 2) spaces)))
    (concat
     (propertize (s-repeat space-to-add " ") 'face `(:overline ,(face-background 'sidebar-select-header-face)))
     (icons-in-terminal sidebar-select-icon-left-header :foreground (face-background 'sidebar-select-header-face)
			:overline (face-background 'sidebar-select-header-face) :raise raise :height height)
     (propertize string 'display '(raise 0.25)
		 'face '(:inherit sidebar-select-header-face :height 1.0))
     (icons-in-terminal sidebar-select-icon-right-header :foreground (face-background 'sidebar-select-header-face)
			:overline (face-background 'sidebar-select-header-face) :raise raise :height height)
     (propertize (s-repeat space-at-the-end " ") 'face `(:overline ,(face-background 'sidebar-select-header-face))))
    ))

(defun sidebar-select-insert-buffername (name icon)
  "NAME ICON."
  (setq sidebar-select-last-line (line-number-at-pos))
  (insert
   (s-truncate (- sidebar-select-window-width 1)
	       (concat
		" "
		(icons-in-terminal icon)
		" "
		name)))
  (newline))

(defun sidebar-select-insert-list (list func-on-string icon)
  "LIST FUNC-ON-STRING ICON."
  (newline)
  (--each list
    (let ((string (funcall func-on-string it)))
      (setq sidebar-select-mapping
	    (append sidebar-select-mapping
		    (list `(:line ,(line-number-at-pos) :window ,it))))
      (sidebar-select-insert-buffername string icon))))

(defun sidebar-select-make-buffer (list header1 header2 func-on-string icon callback &rest args)
  "LIST HEADER1 HEADER2 FUNC-ON-STRING ICON CALLBACK ARGS."
  (select-window
   (display-buffer (get-buffer-create sidebar-select-buffer-name)))
  (with-current-buffer sidebar-select-buffer-name
    (sidebar-select-mode)
    (set (make-local-variable 'scroll-margin) 1)
    (set (make-local-variable 'sidebar-select-window) (get-buffer-window))
    (set (make-local-variable 'sidebar-select-mapping) nil)
    (set (make-local-variable 'sidebar-select-header) header1)
    (set (make-local-variable 'sidebar-select-window-width) (window-width))
    (set (make-local-variable 'sidebar-select-callback) callback)
    (set (make-local-variable 'sidebar-select-args) args)
    (setq header-line-format (list '(:eval (sidebar-select-set-header sidebar-select-header 10 0.0 2.0))))
    (set (make-local-variable 'sidebar-select-line-other) nil)
    (sidebar-select-insert-list (car list) func-on-string icon)
    (when (car (cdr list))
      (newline)
      (insert (sidebar-select-set-header header2 9 0.12 1.7))
      (newline)
      (set (make-local-variable 'sidebar-select-line-other) (line-number-at-pos))
      (sidebar-select-insert-list (car (cdr list)) func-on-string icon))
    (set (make-local-variable 'sidebar-select-windows-count) (+ (length (car list)) (length (car (cdr list)))))
    (shrink-window-if-larger-than-buffer)
    (window-resize nil 1 nil)
    (setq buffer-read-only t)
    (sidebar-goto-line 2)))

(defun sidebar-select-killed-hook ()
  "."
  (when (s-equals? sidebar-select-buffer-name (buffer-name))
    (sidebar-set select-active nil)))

(defun sidebar-select-previous ()
  "."
  (interactive)
  (let* ((current-line (line-number-at-pos))
	 (previous (- current-line 1)))
    (cond ((= previous 1)
	   (sidebar-goto-line sidebar-select-last-line t))
	  ((and sidebar-select-line-other
		(= previous sidebar-select-line-other))
	   (sidebar-goto-line (- previous 3) t))
	  (t (sidebar-goto-line previous t))))
  (set-window-start nil 0))

(defun sidebar-select-next ()
  "."
  (interactive)
  (let* ((current-line (line-number-at-pos))
	 (next (+ current-line 1)))
    (cond ((= next (+ sidebar-select-windows-count (if sidebar-select-line-other 5 2)))
	   (sidebar-goto-line 2 t))
	  ((and sidebar-select-line-other
		(= next (- sidebar-select-line-other 2)))
	   (sidebar-goto-line (+ next 3) t))
	  (t (sidebar-goto-line next t)))))

(defun sidebar-select-select ()
  "."
  (interactive)
  (let ((window (--first (equal (line-number-at-pos) (plist-get it :line)) sidebar-select-mapping)))
    (when window
      (if (car sidebar-select-args)
	  (apply sidebar-select-callback (plist-get window :window) sidebar-select-args)
	(funcall sidebar-select-callback (plist-get window :window)))
      (sidebar-select-cancel))))

(defun sidebar-select-cancel ()
  "."
  (interactive)
  (ignore-errors (kill-buffer sidebar-select-buffer-name))
  (remove-hook 'kill-buffer-hook 'sidebar-select-killed-hook)
  (remove-hook 'buffer-list-update-hook 'sidebar-select-on-change)
  (remove-hook 'pre-command-hook 'sidebar-select-pre-command)
  (remove-hook 'focus-out-hook 'sidebar-select-focus-out)
  (select-window (sidebar-get-window)))

(defvar sidebar-select-mode-map nil
  "Keymap use with sidebar-select-mode.")
(unless sidebar-select-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
;;;    (define-key map [t] 'ignore)

    (define-key map (kbd "<up>") 'sidebar-select-previous)
    (define-key map (kbd "C-p") 'sidebar-select-previous)
    (define-key map (kbd "<down>") 'sidebar-select-next)
    (define-key map (kbd "C-n") 'sidebar-select-next)
    (define-key map (kbd "RET") 'sidebar-select-select)
    (define-key map (kbd "<return>") 'sidebar-select-select)
    (define-key map "<return>" 'sidebar-select-select)
    (define-key map (kbd "q") 'sidebar-select-cancel)
    (define-key map (kbd "C-g") 'sidebar-select-cancel)

    (setq sidebar-select-mode-map map)))

(defun sidebar-select-on-change ()
  "."
  (when (and (not (equal 'sidebar-open-in-window this-command))
	     (not sidebar-select-window)
	     this-command)
    (sidebar-select-cancel)))

(defun sidebar-select-focus-out ()
  "."
  (sidebar-select-cancel))

(defun sidebar-select-pre-command ()
  "."
  (when (equal this-command 'handle-switch-frame)
    (sidebar-select-cancel)))

(define-derived-mode sidebar-select-mode nil "Sidebar"
  "Major mode for Sidebar select.
\\{sidebar-select-mode-map}"
  ::group sidebar-select
  (setq cursor-type nil
	mode-line-format "Type 'q' or C-g to cancel"
	cursor-type nil
	buffer-read-only nil)
  (sidebar-set select-active t)
  (internal-show-cursor nil nil)

  (if (sidebar-gui?)
      (progn
	(copy-face 'sidebar-select-header-gui-face 'sidebar-select-header-face)
	(copy-face 'sidebar-select-line-gui-face 'sidebar-select-line-face))
    (copy-face 'sidebar-select-header-terminal-face 'sidebar-select-header-face)
    (copy-face 'sidebar-select-line-terminal-face 'sidebar-select-line-face))

  (face-remap-add-relative 'hl-line 'sidebar-select-line-face)
  (add-hook 'kill-buffer-hook 'sidebar-select-killed-hook)
  (add-hook 'buffer-list-update-hook 'sidebar-select-on-change)
  (add-hook 'pre-command-hook 'sidebar-select-pre-command)
  (add-hook 'focus-out-hook 'sidebar-select-focus-out)
  (face-remap-add-relative 'mode-line '((:inherit sidebar-header-face :foreground "" :background "" :box nil)))
  (face-remap-add-relative 'mode-line-inactive '((:inherit sidebar-header-face :foreground "" :background "" :box nil)))
  (face-remap-add-relative 'header-line '((:inherit sidebar-header-face :background "" :box nil)))
  (hl-line-mode))

(provide 'sidebar-select)

;;; sidebar-select.el ends here
