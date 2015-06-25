;;; storytime-mode.el --- Emacs Major mode for Storytime files  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Samuel Laurén

;; Author: Samuel Laurén <samuel.lauren@iki.fi>
;; Keywords: Storytime
;; Homepage: https://bitbucket.org/Soft/storytime

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defgroup storytime nil
  "Major mode for Storytime files."
  :prefix "storytime-"
  :group 'wp
  :link '(url-link "https://bitbucket.org/Soft/storytime"))

(defface storytime-header-face
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for headers."
  :group 'storytime)

(defface storytime-prefix-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for header prefixes."
  :group 'storytime)

(defface storytime-meta-key-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for metadata keys."
  :group 'storytime)

(defface storytime-meta-value-face
  '((t (:inherit font-lock-string-face)))
  "Face for metadata values."
  :group 'storytime)

(defface storytime-bracket-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for brackets."
  :group 'storytime)

(defface storytime-link-target-face
  '((t (:inherit font-lock-constant-face :underline t)))
  "Face for link targets."
  :group 'storytime)

(defface storytime-link-title-face
  '((t (:inherit font-lock-string-face)))
  "Face for link titles."
  :group 'storytime)

(defface storytime-missing-target-face
  '((t (:foreground "dark red" :underline (:color "dark red" :style wave))))
  "Face for links with missing targets"
  :group 'storytime)

(defface storytime-operator-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for operators"
  :group 'storytime)

(defcustom storytime-command "storytime"
  "Storytime command."
  :group 'storytim
  :type 'string)

(require 'font-lock)

(defconst storytime-regex-tag
  "[a-zA-Z][a-zA-Z0-9-/]+")

(defconst storytime-regex-header
  (concat "^\\(\\*\\)[ \t]*\\(" storytime-regex-tag "\\)")
  "Regular expression for headers")

(defconst storytime-regex-meta
  "^%[ \t]*\\([^:]+\\):[ \t]*\\(.+\\)"
  "Regular expression for metadata")

(defconst storytime-regex-link
  (concat "^\\(\\[\\)\\(" storytime-regex-tag "\\)\\(\\]\\):[ \t]*\\(.+\\)")
  "Regular expression for links")

(defconst storytime-regex-operator
  "&&\\|||\\|~\\|>\\|<\\|=="
  "Regular expression for operators")

(defconst storytime-mode-font-lock-keywords
  `((,storytime-regex-header . ((1 'storytime-prefix-face)
                                (2 'storytime-header-face)))
    (,storytime-regex-meta . ((1 'storytime-meta-key-face)
                              (2 'storytime-meta-value-face)))
    (,storytime-regex-link . ((1 'storytime-bracket-face)
                              (2 'storytime-link-target-face)
                              (3 'storytime-bracket-face)
                              (4 'storytime-link-title-face)))
    (,storytime-regex-operator . 'storytime-operator-face)))

(defvar-local storytime-process nil
  "Storytime process")

(defun storytime-link-at-point ()
  (save-match-data
    (let* ((line (thing-at-point 'line t))
           (matches (string-match storytime-regex-link line)))
      (when matches
        (match-string 2 line)))))

(defun storytime-follow-link-at-point ()
  (interactive)
  (storytime-jump-to-header (storytime-link-at-point)))

(defun storytime-follow-link-at-point-dwim ()
  (interactive)
  (let ((target (storytime-link-at-point)))
    (if (storytime-header-exists-p target)
        (storytime-jump-to-header target)
      (progn
        (goto-char (point-max))
        (newline)
        (storytime-insert-header target)))))

(defun storytime-make-header-regex (header)
  (concat "^\\*[ \t]*" header))

(defun storytime-jump-to-header (header)
  (interactive "sHeader: ")
  (let ((point
         (save-excursion
           (goto-char (point-min))
           (re-search-forward (storytime-make-header-regex header) nil t))))
    (when point
      (goto-char point))))

(defun storytime-launch ()
  (interactive)
  (when storytime-process
    (interrupt-process storytime-process))
  (if buffer-file-name
      (setq storytime-process
            (start-process "storytime" "*Storytime*" storytime-command buffer-file-name))
    (error "File must be saved before Storytime can be launched")))

(defun storytime-imenu-create-index ()
  (goto-char (point-min))
  (save-match-data
    (let ((matches '()))
      (while (re-search-forward storytime-regex-header nil t)
        (push (cons (match-string 2) (point)) matches))
      (nreverse matches))))

(defun storytime-header-exists-p (header)
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward (storytime-make-header-regex header) nil t) t)))

(defvar storytime-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") #'storytime-launch)
    (define-key map (kbd "C-c C-o") #'storytime-follow-link-at-point-dwim)
    (define-key map (kbd "C-c C-j") #'storytime-jump-to-header)
    (define-key map (kbd "C-c C-s h") #'storytime-insert-header)
    (define-key map (kbd "C-c C-s l") #'storytime-insert-link)
    map)
  "Keymap for Storytime mode.")

(easy-menu-define storytime-mode-menu storytime-mode-map
  "Menu for Storytime mode."
  '("Storytime"
    ["Launch" storytime-launch]
    ["Follow link" storytime-follow-link-at-point-dwim]
    ["Jump to header" storytime-jump-to-header]
    ["Insert header" storytime-insert-header]
    ["Insert link" storytime-insert-link]))

(defun storytime-update-overlays ()
  (remove-overlays)
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward storytime-regex-link nil t)
        (unless (storytime-header-exists-p (match-string 2))
          (let ((overlay (make-overlay (match-beginning 2) (match-end 2))))
            (overlay-put overlay 'face 'storytime-missing-target-face)))))))

(defvar-local storytime-update-timer nil)

(require 'autoinsert)

(define-skeleton storytime-insert-link
  "Insert a link"
  "Target: "
  "[" str "]: " _)

(define-skeleton storytime-insert-header
  "Insert a link"
  "Tag: "
  "* " str "\n\n" _)

;;;###autoload
(define-derived-mode storytime-mode text-mode "Storytime"
  "Major mode for editing Storytime files."
  (setq font-lock-defaults '(storytime-mode-font-lock-keywords)
        font-lock-multiline t)
  (font-lock-mode 1)
  (setq-local outline-regexp storytime-regex-header)
  (easy-menu-add storytime-mode-menu storytime-mode-map)
  (setq imenu-create-index-function #'storytime-imenu-create-index)
  (storytime-update-overlays)
  (setq storytime-update-timer
        (run-with-idle-timer 5 t
                             (lambda (buffer)
                               (with-current-buffer buffer
                                 (storytime-update-overlays)))
                             (current-buffer)))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when storytime-update-timer
                (cancel-timer storytime-update-timer)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.story\\'" . storytime-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.str\\'" . storytime-mode))

(provide 'storytime-mode)

;; End:
;;; storytime-mode.el ends here
