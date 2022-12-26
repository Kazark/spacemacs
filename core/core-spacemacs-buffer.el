;;; core-spacemacs-buffer.el --- Spacemacs Core File -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'core-dotspacemacs)
(eval-when-compile
  (defvar dotspacemacs-distribution)
  (defvar dotspacemacs-filepath)
  (defvar dotspacemacs-show-startup-list-numbers)
  (defvar dotspacemacs-startup-buffer-show-icons)
  (defvar spacemacs-version)
  (defvar configuration-layer-error-count))


(defconst spacemacs-buffer-name "*spacemacs*"
  "The name of the spacemacs buffer.")

(defconst spacemacs-buffer--window-width 80
  "Current width of the home buffer if responsive, 80 otherwise.
See `dotspacemacs-startup-buffer-responsive'.")

(defconst spacemacs-buffer--cache-file
  (expand-file-name (concat spacemacs-cache-directory "spacemacs-buffer.el"))
  "Cache file for various persistent data for the spacemacs startup buffer.")

(defvar spacemacs-buffer-startup-lists-length 20
  "Length used for startup lists with otherwise unspecified bounds.
Set to nil for unbounded.")

(defvar spacemacs-buffer-list-separator "\n\n")

(defvar spacemacs-buffer--current-note-type nil
  "Type of note currently displayed.")

(defvar spacemacs-buffer--errors nil
  "List of errors during startup.")

(defvar spacemacs-buffer--idle-numbers-timer nil
  "This stores the idle numbers timer.")

(defvar spacemacs-buffer--startup-list-number nil
  "This accumulates the numbers that are typed in the home buffer.
It's cleared when the idle timer runs.")

(defvar spacemacs-buffer--last-width nil
  "Previous width of spacemacs-buffer.")

(defvar spacemacs-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (when dotspacemacs-show-startup-list-numbers
      (define-key map (kbd "0") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "1") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "2") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "3") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "4") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "5") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "6") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "7") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "8") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "9") 'spacemacs-buffer/jump-to-number-startup-list-line))

    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map (kbd "RET") 'spacemacs-buffer/return)

    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "J") 'widget-forward)
    (define-key map (kbd "C-i") 'widget-forward)

    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "K") 'widget-backward)

    (define-key map (kbd "C-r") 'spacemacs-buffer/refresh)
    (define-key map "q" 'quit-window)
    map)
  "Keymap for spacemacs-buffer mode.")

(define-derived-mode spacemacs-buffer-mode special-mode "Spacemacs buffer"
  "Spacemacs major mode for startup screen.

\\{spacemacs-buffer-mode-map}"
  :group 'spacemacs
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (page-break-lines-mode +1)
  (with-eval-after-load 'evil
    (progn
      (evil-set-initial-state 'spacemacs-buffer-mode 'motion)
      (evil-make-overriding-map spacemacs-buffer-mode-map 'motion)))
  (suppress-keymap spacemacs-buffer-mode-map t)
  (set-keymap-parent spacemacs-buffer-mode-map nil)
  (setq-local buffer-read-only t
              truncate-lines t))

(defun spacemacs-buffer/set-mode-line (format &optional redisplay)
  "Set mode-line format for spacemacs buffer.
FORMAT: the `mode-line-format' variable Emacs will use to build the mode-line.
If REDISPLAY is non-nil then force a redisplay as well"
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
    (setq mode-line-format format))
  (when redisplay (spacemacs//redisplay)))

(defun spacemacs-buffer/message (msg &rest args)
  "Display MSG in *Messages* prepended with '(Spacemacs)'.
The message is displayed only if `init-file-debug' is non nil.
ARGS: format string arguments."
  (when init-file-debug
    (message "(Spacemacs) %s" (apply 'format msg args))))

(defun spacemacs-buffer/error (msg &rest args)
  "Display MSG as an Error message in `*Messages*' buffer.
ARGS: format string arguments."
  (let ((msg (apply 'format msg args)))
    (message "(Spacemacs) Error: %s" msg)
    (when message-log-max
      (add-to-list 'spacemacs-buffer--errors msg 'append))))

(defvar spacemacs-buffer--warnings nil
  "List of warnings during startup.")

(defun spacemacs-buffer/warning (msg &rest args)
  "Display MSG as a warning message but in buffer `*Messages*'.
ARGS: format string arguments."
  (let ((msg (apply 'format msg args)))
    (message "(Spacemacs) Warning: %s" msg)
    (when message-log-max
      (add-to-list 'spacemacs-buffer--warnings msg 'append))))

(defun spacemacs-buffer/insert-page-break ()
  "Insert a page break line in spacemacs buffer."
  (spacemacs-buffer/append "\n\n"))

(defun spacemacs-buffer/append (msg &optional messagebuf)
  "Append MSG to spacemacs buffer.
If MESSAGEBUF is not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (when messagebuf
        (message "(Spacemacs) %s" msg)))))

(defun spacemacs-buffer/replace-last-line (msg &optional messagebuf)
  "Replace the last line of the spacemacs buffer with MSG.
If MESSAGEBUF is not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (delete-region (line-beginning-position) (point-max))
      (insert msg)
      (when messagebuf
        (message "(Spacemacs) %s" msg)))))

(eval-and-compile
  (defun spacemacs-buffer//startup-list-jump-func-name (str)
    "Given a string, return a spacemacs-buffer function name.

Given:           Return:
\"[?]\"            \"spacemacs-buffer/jump-to-[?]\"
\"Recent Files:\"  \"spacemacs-buffer/jump-to-recent-files\""
    (let ((s (downcase str)))
      ;; remove last char if it's a colon
      (when (string-match ":$" s)
        (setq s (substring s nil (1- (length s)))))
      ;; replace any spaces with a dash
      (setq s (replace-regexp-in-string " " "-" s))
      (concat "spacemacs-buffer/jump-to-" s))))

(defmacro spacemacs-buffer||add-shortcut
    (shortcut-char search-label &optional no-next-line)
  "Add a single-key keybinding for quick navigation in the home buffer.
Navigation is done by searching for a specific word in the buffer.
SHORTCUT-CHAR: the key that the user will have to press.
SEARCH-LABEL: the word the cursor will be brought under (or on).
NO-NEXT-LINE: if nil the cursor is brought under the searched word.

Define a named function: spacemacs-buffer/jump-to-...
for the shortcut. So that a descriptive name is shown,
in for example the `view-lossage' (C-h l) buffer:
 r                      ;; spacemacs-buffer/jump-to-recent-files
 p                      ;; spacemacs-buffer/jump-to-projects
instead of:
 r                      ;; anonymous-command
 p                      ;; anonymous-command"
  (let ((func-name-symbol
         (intern (spacemacs-buffer//startup-list-jump-func-name search-label))))
    `(progn (defun ,func-name-symbol ()
              (interactive)
              (unless (search-forward ,search-label (point-max) t)
                (search-backward ,search-label (point-min) t))
              ,@(unless no-next-line
                  '((forward-line 1)))
              (back-to-indentation))
            (define-key spacemacs-buffer-mode-map ,shortcut-char ',func-name-symbol))))

(defun spacemacs-buffer//center-line (&optional real-width)
  "When point is at the end of a line, center it.
REAL-WIDTH: the real width of the line.  If the line contains an image, the size
            of that image will be considered to be 1 by the calculation method
            used in this function.  As a consequence, the caller must calculate
            himself the correct length of the line taking into account the
            images he inserted in it."
  (let* ((width (or real-width (current-column)))
         (margin (max 0 (floor (/ (- spacemacs-buffer--window-width
                                     width)
                                  2)))))
    (beginning-of-line)
    (insert (make-string margin ?\s))
    (end-of-line)))

(defun spacemacs-buffer//insert-string-list (list-display-name list)
  "Insert a non-interactive startup list in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
LIST: a list of strings displayed as entries."
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert
             "\n"
             (with-temp-buffer
               (insert el)
               (fill-paragraph)
               (goto-char (point-min))
               (insert "    - ")
               (while (= 0 (forward-line))
                 (insert "      "))
               (buffer-string))))
          list)))

(defun spacemacs-buffer//insert-file-list (list-display-name list)
  "Insert an interactive list of files in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
LIST: a list of string pathnames made interactive in this function.

If LIST-DISPLAY-NAME is \"Recent Files:\":
prepend each list item with a number starting at: 1
The numbers indicate that the file can be opened,
by pressing its number key."
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (let ((button-prefix
                   (concat
                    "\n    "
                    (when dotspacemacs-show-startup-list-numbers
                      (format "%2s " (number-to-string spacemacs-buffer--startup-list-nr)))
                    " "
                    (when dotspacemacs-startup-buffer-show-icons
                      (cond
                       ((file-remote-p el)
                        (all-the-icons-octicon "radio-tower" :height 0.8 :v-adjust -0.05))
                       ((file-directory-p el)
                        (all-the-icons-icon-for-dir el))
                       (t
                        (all-the-icons-icon-for-file (file-name-nondirectory el) :height 0.8 :v-adjust -0.05))))
                    " "))
                  (button-text (abbreviate-file-name el)))
              (insert button-prefix)
              (widget-create 'push-button
                             :action `(lambda (&rest ignore)
                                        (find-file-existing ,el))
                             :mouse-face 'highlight
                             :follow-link "\C-m"
                             :button-prefix ""
                             :button-suffix ""
                             :button-face nil
                             :format "%[%t%]" button-text))
            (setq spacemacs-buffer--startup-list-nr
                  (1+ spacemacs-buffer--startup-list-nr)))
          list)))

(defun spacemacs-buffer//insert-files-by-dir-list
    (list-display-name grouped-list)
  "Insert an interactive grouped list of files in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
GROUPED-LIST: a list of string pathnames made interactive in this function."
  (when (car-safe grouped-list)
    (insert list-display-name)
    (mapc (lambda (group)
            (let* ((group-remote-p (file-remote-p (car group)))
                   (button-prefix
                    (concat
                     "\n    "
                     (when dotspacemacs-show-startup-list-numbers
                       (format "%2s " (number-to-string spacemacs-buffer--startup-list-nr)))
                     " "
                     (when dotspacemacs-startup-buffer-show-icons
                       (if group-remote-p
                           (all-the-icons-octicon "radio-tower" :height 0.8 :v-adjust -0.05)
                         (all-the-icons-icon-for-dir (car group))))
                     " "))
                   (button-text-project (abbreviate-file-name (car group))))
              (insert button-prefix)
              (widget-create 'push-button
                             :action `(lambda (&rest ignore)
                                        (find-file-existing ,(car group)))
                             :mouse-face 'highlight
                             :follow-link "\C-m"
                             :button-prefix ""
                             :button-suffix ""
                             :format "%[%t%]" button-text-project)
              (setq spacemacs-buffer--startup-list-nr
                    (1+ spacemacs-buffer--startup-list-nr))
              (mapc (lambda (el)
                      (let* ((button-prefix
                              (concat
                               "\n        "
                               (when dotspacemacs-show-startup-list-numbers
                                 (format "%2s " (number-to-string spacemacs-buffer--startup-list-nr)))
                               " "
                               (when dotspacemacs-startup-buffer-show-icons
                                 (if (or group-remote-p
                                         (file-remote-p (concat (car group) el)))
                                     (all-the-icons-octicon "radio-tower" :height 0.8 :v-adjust -0.05)
                                   (all-the-icons-icon-for-file (file-name-nondirectory el) :height 0.8 :v-adjust -0.05)))
                               " "))
                             (button-text-filename (abbreviate-file-name el)))
                        (insert button-prefix)
                        (widget-create 'push-button
                                       :action `(lambda (&rest ignore)
                                                  (find-file-existing
                                                   (concat ,(car group) ,el)))
                                       :mouse-face 'highlight
                                       :follow-link "\C-m"
                                       :button-prefix ""
                                       :button-suffix ""
                                       :format "%[%t%]" button-text-filename))
                      (setq spacemacs-buffer--startup-list-nr
                            (1+ spacemacs-buffer--startup-list-nr)))
                    (cdr group))))
          grouped-list)))

(defun spacemacs-buffer//insert-bookmark-list (list-display-name list)
  "Insert an interactive list of bookmarks entries (if any) in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
LIST: a list of string bookmark names made interactive in this function."
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (let* ((filename (bookmark-get-filename el))
                   (button-prefix
                    (concat
                     "\n    "
                     (when dotspacemacs-show-startup-list-numbers
                       (format "%2s " (number-to-string spacemacs-buffer--startup-list-nr)))
                     " "
                     (when dotspacemacs-startup-buffer-show-icons
                       (cond
                        ((file-remote-p filename)
                         (all-the-icons-octicon "radio-tower" :height 0.8 :v-adjust -0.05))
                        ((file-directory-p filename)
                         (all-the-icons-icon-for-dir filename))
                        (t
                         (all-the-icons-icon-for-file (file-name-nondirectory filename) :height 0.8 :v-adjust -0.05))))
                     " "))
                   (button-text
                    (if filename
                        (format "%s - %s"
                                el (abbreviate-file-name filename))
                      (format "%s" el))))
              (insert button-prefix)
              (widget-create 'push-button
                             :action `(lambda (&rest ignore) (bookmark-jump ,el))
                             :mouse-face 'highlight
                             :follow-link "\C-m"
                             :button-prefix ""
                             :button-suffix ""
                             :format "%[%t%]" button-text))
            (setq spacemacs-buffer--startup-list-nr
                  (1+ spacemacs-buffer--startup-list-nr)))
          list)))

(defun spacemacs-buffer//get-org-items (types)
  "Make a list of agenda file items for today of kind types.
TYPES: list of `org-mode' types to fetch."
  (require 'org-agenda)
  (let ((date (calendar-gregorian-from-absolute (org-today))))
    (cl-loop for file in (org-agenda-files nil 'ifmode)
             append (spacemacs-buffer//make-org-items
                     file
                     (apply 'org-agenda-get-day-entries file date
                            types)))))

(defun spacemacs-buffer//agenda-list ()
  "Return today's agenda."
  (require 'org-agenda)
  (spacemacs-buffer//get-org-items org-agenda-entry-types))

(defun spacemacs-buffer//todo-list ()
  "Return current todos."
  (require 'org-agenda)
  (spacemacs-buffer//get-org-items '(:todo)))

(defun spacemacs-buffer//make-org-items (file items)
  "Make a spacemacs-buffer org item list.
FILE: file name.
ITEMS:"
  (cl-loop for item in items
           collect (spacemacs-buffer//make-org-item file item)))

(defun spacemacs-buffer//make-org-item (file item)
  "Make a spacemacs-buffer version of an org item.
FILE: file name.
ITEM:"
  `(("text" . ,(get-text-property 0 'txt item))
    ("file" . ,file)
    ("pos"  . ,(marker-position (get-text-property 0 'org-marker item)))
    ("time" . ,(get-text-property 0 'time item))))

(defun spacemacs-buffer//org-jump (el)
  "Action executed when using an item in the home buffer's todo list.
EL: `org-agenda' element to jump to."
  (require 'org-agenda)
  (find-file-other-window (cdr (assoc "file" el)))
  (widen)
  (goto-char (cdr (assoc "pos" el)))
  (when (derived-mode-p 'org-mode)
    (org-show-context 'agenda)
    (save-excursion
      (and (outline-next-heading)
           (org-flag-heading nil)))    ; show the next heading
    (when (outline-invisible-p)
      (outline-show-entry))            ; display invisible text
    (recenter (/ (window-height) 2))
    (org-back-to-heading t)
    (if (re-search-forward org-complex-heading-regexp nil t)
        (goto-char (match-beginning 4))))
  (run-hooks 'org-agenda-after-show-hook))

(defun spacemacs-buffer//insert-todo-list (list-display-name list)
  "Insert an interactive todo list of `org-agenda' entries in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
LIST: list of `org-agenda' entries in the todo list."
  (when (car list)
    (insert list-display-name)
    (setq list (sort list
                     (lambda (a b)
                       (cond
                        ((eq "" (cdr (assoc "time" b)))
                         t)
                        ((eq "" (cdr (assoc "time" a)))
                         nil)
                        (t
                         (string< (cdr (assoc "time" a))
                                  (cdr (assoc "time" b))))))))
    (mapc (lambda (el)
            (let* ((button-prefix
                    (concat
                     "\n    "
                     (when dotspacemacs-show-startup-list-numbers
                       (format "%2s " (number-to-string spacemacs-buffer--startup-list-nr)))
                     " "
                     (when dotspacemacs-startup-buffer-show-icons
                       (all-the-icons-octicon "primitive-dot" :height 1.0 :v-adjust 0.01))
                     " "))
                   (button-text
                    (format "%s %s %s"
                            (let ((filename (cdr (assoc "file" el))))
                              (if dotspacemacs-home-shorten-agenda-source
                                  (file-name-nondirectory filename)
                                (abbreviate-file-name filename)))
                            (if (not (eq "" (cdr (assoc "time" el))))
                                (format "- %s -"
                                        (cdr (assoc "time" el)))
                              "-")
                            (cdr (assoc "text" el)))))
              (insert button-prefix)
              (widget-create 'push-button
                             :action `(lambda (&rest ignore)
                                        (spacemacs-buffer//org-jump ',el))
                             :mouse-face 'highlight
                             :follow-link "\C-m"
                             :button-prefix ""
                             :button-suffix ""
                             :format "%[%t%]" button-text))
            (setq spacemacs-buffer--startup-list-nr
                  (1+ spacemacs-buffer--startup-list-nr)))
          list)))

(defun spacemacs-buffer//associate-to-project (recent-file by-project)
  (dolist (x by-project)
    (when (string-prefix-p (car x) recent-file)
      (setcdr x (cons (string-remove-prefix (car x) recent-file) (cdr x))))))

(defun spacemacs-buffer//recent-files-by-project ()
  (let ((by-project (mapcar (lambda (p) (cons (expand-file-name p) nil))
                            (projectile-relevant-known-projects))))
    (dolist (recent-file recentf-list by-project)
      (spacemacs-buffer//associate-to-project recent-file by-project))))

(defun spacemacs//subseq (seq start end)
  "Adapted version of `cl-subseq'.
Use `cl-subseq', but accounting for end points greater than the size of the
list.  Return entire list if end is omitted.
SEQ, START and END are the same arguments as for `cl-subseq'"
  (let ((len (length seq)))
    (cl-subseq seq start (and (number-or-marker-p end)
                              (min len end)))))

(defmacro spacemacs-buffer||propertize-heading (icon text shortcut-char)
  `(concat (when dotspacemacs-startup-buffer-show-icons
             (concat ,icon " "))
           (propertize ,text 'face 'font-lock-keyword-face)
           (propertize (concat " (" ,shortcut-char ")")
                       'face 'font-lock-comment-face)))

(defun spacemacs-buffer//insert-errors ()
  (when (spacemacs-buffer//insert-string-list
         (spacemacs-buffer||propertize-heading
          (when dotspacemacs-startup-buffer-show-icons
            (all-the-icons-material "error" :face 'font-lock-keyword-face))
          "Errors:" "e")
         spacemacs-buffer--errors)
    (spacemacs-buffer||add-shortcut "e" "Errors:")
    (insert spacemacs-buffer-list-separator)))

(defun spacemacs-buffer//insert-warnings ()
  (when (spacemacs-buffer//insert-string-list
         (spacemacs-buffer||propertize-heading
          (when dotspacemacs-startup-buffer-show-icons
            (all-the-icons-material "warning" :face 'font-lock-keyword-face))
          "Warnings:" "w")
         spacemacs-buffer--warnings)
    (spacemacs-buffer||add-shortcut "w" "Warnings:")
    (insert spacemacs-buffer-list-separator)))

(defun spacemacs-buffer//insert-recent-files (list-size)
  (unless recentf-mode (recentf-mode))
  (setq spacemacs-buffer//recent-files-list
        (let ((agenda-files (if (fboundp 'org-agenda-files)
                                (mapcar #'expand-file-name (org-agenda-files))
                              nil)))
          (cl-delete-if (lambda (x)
                          (or (when (and (bound-and-true-p org-directory) (file-exists-p org-directory))
                                (member x (directory-files org-directory t)))
                              (member x agenda-files)))
                        recentf-list)))
  (setq spacemacs-buffer//recent-files-list
        (spacemacs//subseq spacemacs-buffer//recent-files-list 0 list-size))
  (when (spacemacs-buffer//insert-file-list
         (spacemacs-buffer||propertize-heading
          (when dotspacemacs-startup-buffer-show-icons
            (all-the-icons-octicon "history" :face 'font-lock-keyword-face :v-adjust -0.05))
          "Recent Files:" "r")
         spacemacs-buffer//recent-files-list)
    (spacemacs-buffer||add-shortcut "r" "Recent Files:"))
  (insert spacemacs-buffer-list-separator))

(defun spacemacs-buffer//insert-recent-files-by-project (list-size)
  (unless recentf-mode (recentf-mode))
  (unless projectile-mode (projectile-mode))
  (when (spacemacs-buffer//insert-files-by-dir-list
         (spacemacs-buffer||propertize-heading
          (when dotspacemacs-startup-buffer-show-icons
            (all-the-icons-octicon "rocket" :face 'font-lock-keyword-face :v-adjust -0.05))
          "Recent Files by Project:" "R")
         (mapcar (lambda (group)
                   (cons (car group)
                         (spacemacs//subseq (reverse (cdr group))
                                            0
                                            (cdr list-size))))
                 (spacemacs//subseq (spacemacs-buffer//recent-files-by-project)
                                    0
                                    (car list-size))))
    (spacemacs-buffer||add-shortcut "R" "Recent Files by Project:")
    (insert spacemacs-buffer-list-separator)))

(defun spacemacs-buffer//insert-todos (list-size)
  (when (spacemacs-buffer//insert-todo-list
         (spacemacs-buffer||propertize-heading
          (when dotspacemacs-startup-buffer-show-icons
            (all-the-icons-octicon "check" :face 'font-lock-keyword-face :v-adjust -0.05))
          "To-Do:" "d")
         (spacemacs//subseq (spacemacs-buffer//todo-list)
                            0 list-size))
    (spacemacs-buffer||add-shortcut "d" "To-Do:")
    (insert spacemacs-buffer-list-separator)))

(defun spacemacs-buffer//insert-agenda (list-size)
  (when (spacemacs-buffer//insert-todo-list
         (spacemacs-buffer||propertize-heading
          (when dotspacemacs-startup-buffer-show-icons
            (all-the-icons-octicon "calendar" :face 'font-lock-keyword-face :v-adjust -0.05))
          "Agenda:" "c")
         (spacemacs//subseq (spacemacs-buffer//agenda-list)
                            0 list-size))
    (spacemacs-buffer||add-shortcut "c" "Agenda:")
    (insert spacemacs-buffer-list-separator)))

(defun spacemacs-buffer//insert-bookmarks (list-size)
  (when (configuration-layer/layer-used-p 'spacemacs-helm)
    (helm-mode))
  (require 'bookmark)
  (when (spacemacs-buffer//insert-bookmark-list
         (spacemacs-buffer||propertize-heading
          (when dotspacemacs-startup-buffer-show-icons
            (all-the-icons-octicon "bookmark" :face 'font-lock-keyword-face :v-adjust -0.05))
          "Bookmarks:" "b")
         (spacemacs//subseq (bookmark-all-names)
                            0 list-size))
    (spacemacs-buffer||add-shortcut "b" "Bookmarks:")
    (insert spacemacs-buffer-list-separator)))

(defun spacemacs-buffer//insert-projects (list-size)
  (unless projectile-mode (projectile-mode))
  (when (spacemacs-buffer//insert-file-list
         (spacemacs-buffer||propertize-heading
          (when dotspacemacs-startup-buffer-show-icons
            (all-the-icons-octicon "rocket" :face 'font-lock-keyword-face :v-adjust -0.05))
          "Projects:" "p")
         (spacemacs//subseq (projectile-relevant-known-projects)
                            0 list-size))
    (spacemacs-buffer||add-shortcut "p" "Projects:")
    (insert spacemacs-buffer-list-separator)))

(defvar spacemacs-buffer--startup-list-nr 1)

(defun spacemacs-buffer//do-insert-startupify-lists ()
  "Insert the startup lists in the current buffer."
  (setq spacemacs-buffer--startup-list-nr 1)
  (let ((dotspacemacs-startup-buffer-show-icons dotspacemacs-startup-buffer-show-icons))
    (if (display-graphic-p)
        (unless (configuration-layer/package-used-p 'all-the-icons)
          (message "Package `all-the-icons' isn't installed")
          (setq dotspacemacs-startup-buffer-show-icons nil))
      (setq dotspacemacs-startup-buffer-show-icons nil))
    (when dotspacemacs-startup-buffer-show-icons
      (require 'all-the-icons))
    (dolist (els (append '(warnings) dotspacemacs-startup-lists))
      (let ((el (or (car-safe els) els))
            (list-size (or (cdr-safe els)
                           spacemacs-buffer-startup-lists-length)))
        (cond
         ((eq el 'warnings)
          (spacemacs-buffer//insert-errors)
          (spacemacs-buffer//insert-warnings))
         ((eq el 'recents) (spacemacs-buffer//insert-recent-files list-size))
         ((and (eq el 'recents-by-project)
	       (fboundp 'projectile-mode))
          (spacemacs-buffer//insert-recent-files-by-project list-size))
         ((eq el 'todos) (spacemacs-buffer//insert-todos list-size))
         ((eq el 'agenda) (spacemacs-buffer//insert-agenda list-size))
         ((eq el 'bookmarks) (spacemacs-buffer//insert-bookmarks list-size))
         ((and (eq el 'projects)
               (fboundp 'projectile-mode))
          (spacemacs-buffer//insert-projects list-size)))))))

(defun spacemacs-buffer//get-buffer-width ()
  "Return the length of longest line in the current buffer."
  (save-excursion
    (goto-char 0)
    (let ((current-max 0))
      (while (not (eobp))
        (let ((line-length (- (line-end-position) (line-beginning-position))))
          (setq current-max (max current-max line-length)))
        (forward-line 1))
      current-max)))

(defun spacemacs-buffer//center-startup-lists ()
  "Center startup lists after they were inserted."
  (let* ((lists-width (spacemacs-buffer//get-buffer-width))
         (width-diff (- spacemacs-buffer--window-width lists-width))
         (final-padding
          (cond
           ((< width-diff 0) 0)
           (t              (floor (/ width-diff 2))))))
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (insert (make-string final-padding ?\s))
      (forward-line))))

(defun spacemacs-buffer/insert-startup-lists ()
  "Insert startup lists in home buffer."
  (interactive)
  (with-current-buffer (get-buffer spacemacs-buffer-name)
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (save-restriction
        (narrow-to-region (point) (point))
        (spacemacs-buffer//do-insert-startupify-lists)
        (spacemacs-buffer//center-startup-lists)))))

(defun spacemacs-buffer/jump-to-number-startup-list-line ()
  "Jump to the startup list line with the typed number.

The minimum delay in seconds between number key presses,
can be adjusted with the variable:
`dotspacemacs-startup-buffer-multi-digit-delay'."
  (interactive)
  (when spacemacs-buffer--idle-numbers-timer
    (cancel-timer spacemacs-buffer--idle-numbers-timer))
  (let* ((key-pressed-string (string-trim-left (if (characterp last-input-event)
                                                   (string last-input-event)
                                                 (format "%s" last-input-event))
                                               "kp-")))
    (setq spacemacs-buffer--startup-list-number
          (concat spacemacs-buffer--startup-list-number key-pressed-string))
    (let (message-log-max) ; only show in minibuffer
      (message "Jump to startup list: %s" spacemacs-buffer--startup-list-number))
    (setq spacemacs-buffer--idle-numbers-timer
          (run-with-idle-timer
           dotspacemacs-startup-buffer-multi-digit-delay nil
           'spacemacs-buffer/stop-waiting-for-additional-numbers))))

(defun spacemacs-buffer/jump-to-line-starting-with-nr-space (nr-string)
  "Jump to the line begins with NR-STRING, skipping non-digit prefix."
  (let ((prev-point (point)))
    (goto-char (window-start))
    (if (not (re-search-forward
              (concat "^ +" nr-string "[0-9]* +. ")
              ;; don't search past two lines above the window-end,
              ;; because they bottom two lines are hidden by the mode line
              (save-excursion (goto-char (window-end))
                              (forward-line -1)
                              (point))
              'noerror))
        (progn
          (goto-char prev-point)
          (let (message-log-max) ; only show in minibuffer
            (message "Couldn't find startup list number: %s"
                     spacemacs-buffer--startup-list-number)))
      (message "Opening file/dir: %s"
               (widget-value (widget-at (point))))
      (widget-button-press (point)))))

(defun spacemacs-buffer/stop-waiting-for-additional-numbers ()
  (spacemacs-buffer/jump-to-line-starting-with-nr-space
   spacemacs-buffer--startup-list-number)
  (setq spacemacs-buffer--startup-list-number nil))

(defun spacemacs-buffer//startup-hook ()
  "Code executed when Emacs has finished loading."
  (with-current-buffer (get-buffer spacemacs-buffer-name)
    (when dotspacemacs-startup-lists
      (spacemacs-buffer/insert-startup-lists))
    (if configuration-layer-error-count
        (progn
          (spacemacs-buffer-mode)
          (face-remap-add-relative 'mode-line
                                   '((:background "red") mode-line))
          (spacemacs-buffer/set-mode-line
           (format
            (concat "%s error(s) at startup! "
                    "Spacemacs may not be able to operate properly.")
            configuration-layer-error-count) t))
      (spacemacs-buffer/set-mode-line spacemacs--default-mode-line)
      (spacemacs-buffer-mode))
    (force-mode-line-update)))

(defun spacemacs-buffer/goto-buffer (&optional refresh)
  "Create the special buffer for `spacemacs-buffer-mode' and switch to it.
REFRESH if the buffer should be redrawn.

If a prefix argument is given, switch to it in an other, possibly new window."
  (interactive)
  (let ((buffer-exists (buffer-live-p (get-buffer spacemacs-buffer-name)))
        (save-line nil))
    (when (or (not (eq spacemacs-buffer--last-width (window-width)))
              (not buffer-exists)
              refresh)
      (setq spacemacs-buffer--window-width (if dotspacemacs-startup-buffer-responsive
                                               (window-width)
                                             80)
            spacemacs-buffer--last-width spacemacs-buffer--window-width)
      (with-current-buffer (get-buffer-create spacemacs-buffer-name)
        (page-break-lines-mode)
        (save-excursion
          (when (> (buffer-size) 0)
            (setq save-line (line-number-at-pos))
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (spacemacs-buffer/set-mode-line "")
          (when (bound-and-true-p spacemacs-initialized)
            (when dotspacemacs-startup-lists
              (spacemacs-buffer/insert-startup-lists))
            (configuration-layer/display-summary emacs-start-time)
            (spacemacs-buffer/set-mode-line spacemacs--default-mode-line)
            (force-mode-line-update)
            (spacemacs-buffer-mode)))
        (if save-line
            (progn (goto-char (point-min))
                   (forward-line (1- save-line))
                   (forward-to-indentation 0))))
      (if current-prefix-arg
          (switch-to-buffer-other-window spacemacs-buffer-name)
        (switch-to-buffer spacemacs-buffer-name))
      (spacemacs//redisplay))))

(add-hook 'window-setup-hook
          (lambda ()
            (add-hook 'window-configuration-change-hook
                      'spacemacs-buffer//resize-on-hook)
            (spacemacs-buffer//resize-on-hook)))

(defun spacemacs-buffer//resize-on-hook ()
  "Hook run on window resize events to redisplay the home buffer."
  ;; prevent spacemacs buffer redisplay in the filetree window
  (unless (memq this-command '(neotree-find-project-root
                               neotree-show
                               neotree-toggle
                               spacemacs/treemacs-project-toggle
                               treemacs
                               treemacs-bookmark
                               treemacs-find-file
                               treemacs-select-window))
    (let ((home-buffer (get-buffer-window spacemacs-buffer-name))
          (frame-win (frame-selected-window)))
      (when (and dotspacemacs-startup-buffer-responsive
                 home-buffer
                 (not (window-minibuffer-p frame-win)))
        (with-selected-window home-buffer
          (spacemacs-buffer/goto-buffer))))))

(defun spacemacs-buffer/refresh ()
  "Force recreation of the spacemacs buffer."
  (interactive)
  (setq spacemacs-buffer--last-width nil)
  (spacemacs-buffer/goto-buffer t))

(defalias 'spacemacs/home 'spacemacs-buffer/refresh
  "Go to Spacemacs home buffer.")

(defun spacemacs-buffer/return ()
  "Open the button or go to next line.

This function is intended to be used in `spacemacs-buffer-mode' only."
  (interactive)
  (if (get-char-property (point) 'button)
      ;; point on a button, press it
      (widget-button-press (point))
    ;; point on an entry, press it
    (if-let ((button (save-excursion
                       (beginning-of-line-text)
                       (re-search-forward "[0-9]* +. " (point-at-eol) 'noerror))))
        (widget-button-press button)
      ;; go to next line
      (forward-line)
      (beginning-of-line-text))))

(defun spacemacs/home-delete-other-windows ()
  "Open home Spacemacs buffer and delete other windows.
Useful for making the home buffer the only visible buffer in the frame."
  (interactive)
  (spacemacs/home)
  (delete-other-windows))

(provide 'core-spacemacs-buffer)

;;; core-spacemacs-buffer ends here
