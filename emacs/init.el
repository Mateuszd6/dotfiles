;; A _lot_ of this file is heavily inspired by Harry R. Schwartz great emacs
;; config file, see: https://github.com/hrs/dotfiles

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;; Use sensible defaults by Harry R. Schwartz, but don't use all settings.
(load "~/.emacs.d/lisp/sensible-defaults")
(sensible-defaults/open-files-from-home-directory)
(sensible-defaults/increase-gc-threshold)
(sensible-defaults/delete-trailing-whitespace)
(sensible-defaults/automatically-follow-symlinks)
(sensible-defaults/make-scripts-executable)
(sensible-defaults/single-space-after-periods)
(sensible-defaults/offer-to-create-parent-directories-on-save)
(sensible-defaults/apply-changes-to-highlighted-region)
(sensible-defaults/overwrite-selected-text)
(sensible-defaults/ensure-that-files-end-with-newline)
(sensible-defaults/quiet-startup)
(sensible-defaults/make-dired-file-sizes-human-readable)
(sensible-defaults/shorten-yes-or-no)
(sensible-defaults/always-highlight-code)
(sensible-defaults/refresh-buffers-when-files-change)
(sensible-defaults/show-matching-parens)
(sensible-defaults/flash-screen-instead-of-ringing-bell)
(sensible-defaults/set-default-line-length-to 80)
(sensible-defaults/open-clicked-files-in-same-frame-on-mac)
(sensible-defaults/yank-to-point-on-mouse-click)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

(setq initial-major-mode 'org-mode) ;; Org mode in scratch buffer.
(setq user-full-name "Mateusz Dudziński"
      user-mail-address "dudzinskimat@outlook.com")

(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)
(delete-selection-mode t)
(transient-mark-mode 0)

;; Disable *Messages* buffer
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

(use-package visible-mark)
(global-visible-mark-mode t)

; Hide menu bar, tool bar and scorll bars.
(menu-bar-mode -1)
(tool-bar-mode 0)
(scroll-bar-mode -1)

(defun nil-bell ())
(setq ring-bell-function 'nil-bell)
; I usually set this instead of the above, when I want to unlearn a hotkey that
; I'm used to so that emacs flashes the screen every time I make a mistate.
;; (setq visible-bell 1)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-create-new-buffer 'always) ; don't confirm when creating new buffer

(defun hrs/rename-file (new-name)
  (interactive "FNew name: ")
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          (when (buffer-modified-p)
            (save-buffer))
          (rename-file filename new-name t)
          (kill-buffer (current-buffer))
          (find-file new-name)
          (message "Renamed '%s' -> '%s'" filename new-name))
      (message "Buffer '%s' isn't backed by a file!" (buffer-name)))))

(defun hrs/generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
       name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(defun hrs/add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;; Old great trick from Stack Exchange to always keep scratch buffer alive.
(add-hook 'kill-buffer-query-functions #'my/dont-kill-scratch)
(defun my/dont-kill-scratch ()
  (if (not (equal (buffer-name) "*scratch*"))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

(defun open-as-sudo ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

(save-place-mode t)
(setq-default indent-tabs-mode nil)

(use-package which-key :config (which-key-mode))

;; TODO: Read these:
(show-paren-mode)
(electric-pair-mode)

;;
;; AUTOCOMPLETE
;;
(use-package auto-complete)
(ac-config-default)
(require 'auto-complete-config)
(ac-ropemacs-initialize)
(setq ac-delay 0.0)
(global-auto-complete-mode t)
(define-key ac-complete-mode-map "<right>" nil)
(define-key ac-complete-mode-map "<left>" nil)
(define-key ac-complete-mode-map "<up>" nil)
(define-key ac-complete-mode-map "<down>" nil)
(define-key ac-complete-mode-map "\t" 'ac-expand)
(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)
(setq ac-auto-start 1)
(setq ac-dwim t)
;; (set-default 'ac-sources '(ac-source-abbrev ac-source-words-in-buffer))
(setq ac-modes (append ac-modes '(eshell-mode)))
;; (use-package company)
;; (add-hook 'after-init-hook 'global-company-mode)
;; (global-set-key (kbd "M-/") 'company-complete-common)

;;
;; LOOK AND FEEL
;;
(setq hrs/default-font "DejaVu Sans Mono")
(setq hrs/default-font-size 9.5)
(setq hrs/current-font-size hrs/default-font-size)

(setq hrs/font-change-increment 0.5)

(defun hrs/font-code ()
  "Return a string representing the current font (like \"Inconsolata-14\")."
  (concat hrs/default-font "-" (number-to-string hrs/current-font-size)))

(defun hrs/set-font-size ()
  "Set the font to `hrs/default-font' at `hrs/current-font-size'.
Set that for the current frame, and also make it the default for
other, future frames."
  (let ((font-code (hrs/font-code)))
    (if (assoc 'font default-frame-alist)
        (setcdr (assoc 'font default-frame-alist) font-code)
      (add-to-list 'default-frame-alist (cons 'font font-code)))
    (set-frame-font font-code)))

(defun hrs/reset-font-size ()
  "Change font size back to `hrs/default-font-size'."
  (interactive)
  (setq hrs/current-font-size hrs/default-font-size)
  (hrs/set-font-size))

(defun hrs/increase-font-size ()
  "Increase current font size by a factor of `hrs/font-change-increment'."
  (interactive)
  (setq hrs/current-font-size
        (ceiling (* hrs/current-font-size hrs/font-change-increment)))
  (hrs/set-font-size))

(defun hrs/decrease-font-size ()
  "Decrease current font size by a factor of `hrs/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq hrs/current-font-size
        (max 1
             (floor (/ hrs/current-font-size hrs/font-change-increment))))
  (hrs/set-font-size))

(hrs/reset-font-size)

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

;; (add-hook 'prog-mode-hook 'linum-mode) ; TODO: Try to live without it.
(setq linum-format "%d ")
(set-fringe-mode '(4 . 4)) ; Remove fringe.

(set-face-attribute 'visible-mark-active nil :background "#00FF00")
(set-cursor-color "#f6f6f0")

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

;; Bright-red TODOs:
(setq fixme-modes '(c-mode c++-mode objc-mode emacs-lisp-mode shell-mode python-mode
                           shell-script-mode csharp-mode java-mode org-mode
                           prog-mode haskell-mode org-mode latex-mode))

(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\|IMPORTANT\\|BUG\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(NOTE\\|HACK\\)" 1 'font-lock-note-face t))))
      fixme-modes)

(use-package monokai-theme)
(load-theme 'monokai t)
(modify-face 'font-lock-fixme-face "Red3" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

;;
;; KEYBINDINGS
;;
(keyboard-translate ?\C-t ?\C-c)
(keyboard-translate ?\C-c ?\C-t)
(keyboard-translate ?\C-x ?\C-y)
(keyboard-translate ?\C-y ?\C-x)

(global-set-key (kbd "C-y") 'kill-region)
(global-set-key (kbd "C-t") 'copy-region-as-kill)
(global-unset-key (kbd "C-w"))

(defface highlight-yanked-code
  '((((class color) (min-colors 88) (background light))
     :background "yellow")
    (((class color) (min-colors 88) (background dark))
     :background "#b2ae80")
    (((class color) (min-colors 16) (background light))
     :background "yellow")
    (((class color) (min-colors 16) (background dark))
     :background "#b2ae80")
    (((class color) (min-colors 8))
     :background "#b2ae80" :foreground "black")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'basic-faces)

(setq-default tab-width 4)

(defun md/yank ()
  "Yank the text and blink it so that its clearly visible what
have been pasted."
  (interactive)
  (yank)
  (pulse-momentary-highlight-region (mark) (point) 'highlight-yanked-code))

(defun md/yank-pop ()
  "Similar functionality to 'md/yank', but it does yank-pop
instead of yank command."
  (interactive)
  (yank-pop)
  (pulse-momentary-highlight-region (mark) (point) 'highlight-yanked-code))

(global-set-key (kbd "C-v") 'md/yank)
(global-set-key (kbd "M-v") 'md/yank-pop)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-x") 'kill-whole-line)

(defun md/save-buffer ()
  "Save the buffer after untabifying it, and deleting trailing whitespaces."
  (interactive)
  (save-excursion
    (save-restriction
      ;; Dont untabify when in makefile.
      (unless (member major-mode '(makefile-gmake-mode))
        (untabify (1- (point)) (point-max))
        (widen))))
  (delete-trailing-whitespace)
  (save-buffer))

(global-set-key (kbd "C-s") 'md/save-buffer)
(global-set-key (kbd "C-r") 'hrs/rename-file)

(global-set-key (kbd "C-SPC") 'set-mark-command)
(global-set-key (kbd "C-S-SPC") 'pop-to-mark-command)
(global-set-key (kbd "C-2") 'exchange-point-and-mark)

;; Insert new line below current line
;; and move cursor to new line
;; it will also indent newline
(global-set-key (kbd "<C-return>") (lambda ()
                                     (interactive)
                                     (end-of-line)
                                     (newline-and-indent)))

;; Insert new line above current line
;; and move cursor to previous line (newly inserted line)
;; it will also indent newline
(global-set-key (kbd "<C-S-return>") (lambda ()
                                       (interactive)
                                       (beginning-of-line)
                                       (newline-and-indent)
                                       (previous-line)))

(global-set-key (kbd "M-w") 'other-window)
(global-set-key (kbd "<C-prior>") 'switch-to-prev-buffer)
(global-set-key (kbd "<C-next>") 'switch-to-next-buffer)

(use-package magit)
(global-set-key (kbd "M-g") 'magit-status)

(with-eval-after-load 'magit-mode
  (define-key magit-mode-map (kbd "M-w") nil))

(global-set-key (kbd "M-s") 'isearch-forward)
(global-set-key (kbd "M-S") 'isearch-backward)
(add-hook 'isearch-mode-hook
          (function
           (lambda ()
             (define-key isearch-mode-map (kbd "M-h") 'isearch-mode-help)
             (define-key isearch-mode-map (kbd "M-r") 'isearch-toggle-regexp)
             (define-key isearch-mode-map (kbd "M-c") 'isearch-toggle-case-fold)
             (define-key isearch-mode-map (kbd "M-w") 'isearch-toggle-word)
             (define-key isearch-mode-map (kbd "M-s") 'isearch-repeat-forward)
             (define-key isearch-mode-map (kbd "M-S") 'isearch-repeat-backward))))

(global-set-key (kbd "M-r") 'query-replace)
(global-set-key (kbd "M-R") 'query-replace-regexp)

;; Goto line:
;; Files and buffers:
(global-set-key (kbd "M-l") 'locate)
(global-set-key (kbd "M-f") 'find-file)
(global-set-key (kbd "M-F") 'find-file-other-window)
(global-set-key (kbd "M-C-R") 'revert-buffer)
(global-set-key (kbd "M-b") 'switch-to-buffer)
(global-set-key (kbd "M-B") 'switch-to-buffer-other-window)
(global-set-key (kbd "<C-tab>") 'indent-region)
;; (global-set-key (kbd "C-a") 'mark-whole-buffer) ;; TODO: find a keystroke for this.
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-S-w") 'kill-word)

(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "M-k") 'kill-this-buffer)

;; This hack is used to kill client instances. In main window server-edit closes
;; the file and it is fine, becasue frame can't be deleted. In the client frame
;; file is either edited, when the edited file is opened and if for any reason
;; there is not file (e.g.: emacsclient was invoked with -f but no file), client
;; window gets killed.
(global-set-key (kbd "M-K") (lambda ()
                              (interactive)
                              (server-edit)
                              (delete-frame)))

;; I never ever split windows, so there are always two on my screen.
(use-package buffer-move)
(defun md/swap-buffers ()
  (interactive)
  (if (condition-case nil (buf-move-left) (error nil))
      ()
    (buf-move-right)))
(global-set-key (kbd "M-W") 'md/swap-buffers)

;; Shortcuts for moving stuff around.
(use-package drag-stuff)
(global-set-key (kbd "C-M-<up>")   #'drag-stuff-up)
(global-set-key (kbd "C-M-<down>") #'drag-stuff-down)
(global-set-key (kbd "C-M-<left>") #'drag-stuff-left)
(global-set-key (kbd "C-M-<right>") #'drag-stuff-right)

(global-set-key (kbd "<f8>") 'next-error)
(global-set-key (kbd "S-<f8>") 'previous-error)

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-N") 'previous-error)

(global-set-key (kbd "<f7>") 'flycheck-next-error)
(global-set-key (kbd "S-<f7>") 'flycheck-previous-error)

;; Visit emacs config
(global-set-key (kbd "M-C-<home>") (lambda ()
                                     (interactive)
                                     (find-file "~/.emacs.d/init.el")))

;; Visit notes
(global-set-key (kbd "M-C-<end>") (lambda ()
                                     (interactive)
                                     (switch-to-buffer "*scratch*")))

(setq fill-column 120)
(global-set-key (kbd "M-q") 'fill-paragraph)

;; Commenting and lines blocks:
(global-set-key (kbd "C-/") 'comment-line)

(global-set-key (kbd "C-.") (lambda ()
                              (interactive)
                              (execute-kbd-macro(kbd "->"))))

(global-set-key (kbd "C-,") (lambda ()
                              (interactive)
                              (execute-kbd-macro(kbd "<-"))))

(global-set-key (kbd "C-SPC") (lambda ()
                                (interactive)
                                (execute-kbd-macro(kbd "_"))))

;; Expand region:
(use-package expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)

;; TODO: It is usless, find another binding.
(global-set-key (kbd "M-c") 'quick-calc)

;; Multiple cursor support:
;; TODO: Find another bindings for them
;; (global-set-key (kbd "C-d") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-S-d") 'mc/skip-to-next-like-this)

(global-set-key (kbd "M-m") 'md/compile-file)

;;
;; Semantic mode
;;
(semantic-mode)
(defun find-definition (arg)
  (interactive "P")
  (let* ((tag (or (semantic-idle-summary-current-symbol-info-context)
                  (semantic-idle-summary-current-symbol-info-brutish)
                  (error "No known tag at point")))
         (pos (or (semantic-tag-start tag)
                  (error "Tag definition not found")))
         (file (semantic-tag-file-name tag)))
    (if file
        (if arg (find-file-other-window file) (find-file file))
      (if arg (switch-to-buffer-other-window (current-buffer))))
    (push-mark)
    (goto-char pos)
    (end-of-line)))

(global-set-key (kbd "M-d") 'find-definition)
(global-set-key (kbd "M-D") 'semantic-ia-show-doc)

;; New hotkeys:
(global-set-key (kbd "<C-left>") '(lambda () (interactive) (ding)))
(global-set-key (kbd "<C-right>") '(lambda () (interactive) (ding)))
(global-set-key (kbd "<C-down>") '(lambda () (interactive) (ding)))
(global-set-key (kbd "<C-up>") '(lambda () (interactive) (ding)))

(global-set-key (kbd "C-a") '(lambda () (interactive) (ding)))
(global-set-key (kbd "C-A") '(lambda () (interactive) (ding)))
(global-set-key (kbd "C-e") '(lambda () (interactive) (ding)))
(global-set-key (kbd "C-E") '(lambda () (interactive) (ding)))

(global-set-key (kbd "<M-left>") 'backward-word)
(global-set-key (kbd "<M-right>") 'forward-word)
(global-set-key (kbd "<M-down>") 'forward-paragraph)
(global-set-key (kbd "<M-up>") 'backward-paragraph)

(global-set-key (kbd "M-a") 'move-beginning-of-line)
(global-set-key (kbd "M-A") 'beginning-of-buffer)
(global-set-key (kbd "M-e") 'move-end-of-line)
(global-set-key (kbd "M-E") 'end-of-buffer)

(global-set-key (kbd "M-l") 'goto-line)

(global-set-key (kbd "C-SPC") 'set-mark-command)
(global-set-key (kbd "M-SPC") 'set-mark-command)

(global-set-key (kbd "C-0") '(lambda () (interactive) (execute-kbd-macro(kbd "C-)"))))
(global-set-key (kbd "C-)") '(lambda ()
                               (interactive)
                               (insert ")")
                               (exchange-point-and-mark)
                               (insert "(")
                               (exchange-point-and-mark)))

(global-set-key (kbd "C-9") '(lambda () (interactive) (execute-kbd-macro(kbd "C-("))))
(global-set-key (kbd "C-(") '(lambda ()
                               (interactive)
                               (insert "(")
                               (exchange-point-and-mark)
                               (insert ")")
                               (exchange-point-and-mark)))

(global-set-key (kbd "C-]") '(lambda ()
                               (interactive)
                               (insert "]")
                               (exchange-point-and-mark)
                               (insert "[")
                               (exchange-point-and-mark)))

(global-set-key (kbd "C-}") '(lambda ()
                               (interactive)
                               (insert "}")
                               (exchange-point-and-mark)
                               (insert "{")
                               (exchange-point-and-mark)))

;;
;; C++
;;
(c-add-style "md" '("linux" (c-offsets-alist
                             (innamespace . 0)
                             (inline-open . 0))))

(setq c-default-style "md"
      c-basic-offset 4)

;; Grey out #if 0's
(defun my-c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

(defun md/c-mode-common-hook ()
  (font-lock-add-keywords
   nil
   '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

(defun get-include-guard ()
  (let* ((fname (buffer-file-name (current-buffer)))
         (fbasename (replace-regexp-in-string ".*/" "" fname))
         (inc-guard-base (replace-regexp-in-string "[.-]"
                                                   "_"
                                                   fbasename)))
    (concat (upcase inc-guard-base) "_")))

(defun md/get-makefile-dir (&optional startdir)
  "Move up directories until we find a makefile."
  (interactive)
  (let ((dirname (expand-file-name
                  (if startdir startdir ".")))
        (found nil)
        (top nil))
    (while (not (or found top))
      (if (string= (expand-file-name dirname) "/")
          (setq top t))
      (if (file-exists-p (expand-file-name "Makefile" dirname))
          (setq found t)
        (setq dirname (expand-file-name ".." dirname))))
    (if found dirname nil)))

(defun md/compile-file ()
  (interactive)
  (setq make-dir (md/get-makefile-dir))
  (if make-dir
    (setq compile-command (concat (concat "cd " make-dir) " && make")))
  (call-interactively 'compile))

(add-hook 'find-file-not-found-hooks
          '(lambda ()
             (let ((file-name (buffer-file-name (current-buffer))))
               (when (or (string= ".hpp" (substring file-name -4))
                         (string= ".h" (substring file-name -2)))
                 (let ((include-guard (get-include-guard)))
                   (insert "#ifndef " include-guard)
                   (newline)
                   (insert "#define " include-guard)
                   (newline 4)
                   (insert "#endif // " include-guard)
                   (newline)
                   (previous-line 3)
                   (set-buffer-modified-p nil))))))

(add-hook 'c-mode-common-hook 'md/c-mode-common-hook)

;;
;; Makefile:
;;

;; Disable M-n
(add-hook 'makefile-gmake-mode
          (lambda() (local-unset-key (kbd "M-n"))))

;;
;; Dired:
;;
(load "~/.emacs.d/lisp/dired+")
;; (setq dired-omit-files (concat dired-omit-files "^\\."))

(use-package dired-open
  :config
  (setq dired-open-extensions
        ;; Yeah, I don't open that many files with external programs...
        '(("jpeg" . "feh")
          ("jpg" . "feh")
          ("pdf" . "zathura")
          ("png" . "feh")
          ("tiff" . "feh"))))

(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(setq dired-dwim-target t)
(setq dired-clean-up-buffers-too t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top)

(defun md/dired-up-dir ()
  "Go up a directory."
  (interactive)
  (let ((current-dir (dired-current-directory)))
    (find-alternate-file "..")
    (dired-goto-file current-dir)))

(define-key dired-mode-map (kbd "<left>") 'md/dired-up-dir)
(define-key dired-mode-map (kbd "<right>") 'diredp-find-file-reuse-dir-buffer)
(define-key dired-mode-map (kbd "<return>") 'dired-open-file)

(add-hook 'dired-mode-hook
          (lambda()
            (local-unset-key (kbd "M-s f C-s"))
            (local-unset-key (kbd "M-s f C-M-s"))
            (local-unset-key (kbd "M-s f"))

            (local-unset-key (kbd "M-s a C-s"))
            (local-unset-key (kbd "M-s a C-M-s"))
            (local-unset-key (kbd "M-s a"))
            (local-unset-key (kbd "M-s"))

            (local-unset-key (kbd "M-g"))
            (local-unset-key (kbd "M-b"))
            ))

(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

;;
;; Diff
;;
(add-hook 'diff-mode-hook
          (lambda()
            (local-unset-key (kbd "M-k"))
            (local-unset-key (kbd "M-SPC"))))

;;
;; General text files:
;;
(defalias 'change-dict 'ispell-change-dictionary)
(global-set-key (kbd "M-/") 'ispell-word)

(add-to-list 'auto-mode-alist '("\\`/tmp/neomutt-" . mail-mode))

;; Enable polish dict in mails.
(add-hook 'mail-mode-hook
          (lambda ()
            (flyspell-mode)
            (ispell-change-dictionary "polish")))

;;
;; Org:
;;
(use-package org-bullets)

(defun md/org-no-increasing-height ()
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))

(setq org-agenda-start-on-weekday 1) ; Start week on monday.
(setq org-bullets-bullet-list '(" "))
(setq org-ellipsis " ⤵")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(add-hook 'org-mode-hook
          (lambda ()
            (md/org-no-increasing-height)
            (org-bullets-mode 1)
            (flyspell-mode)
            (ispell-change-dictionary "english")
            (local-set-key (kbd "M-m") 'org-export-dispatch)
            (local-unset-key (kbd "C-y"))
            (local-unset-key (kbd "M-SPC"))
            (local-unset-key (kbd "M-<left>"))
            (local-unset-key (kbd "M-<right>"))
            (local-unset-key (kbd "M-<up>"))
            (local-unset-key (kbd "M-<down>"))
            (local-set-key (kbd "C-M-<left>") 'org-shiftmetaleft)
            (local-set-key (kbd "C-M-<right>") 'org-shiftmetaright)
            (local-set-key (kbd "C-M-<up>") 'org-metaup)
            (local-set-key (kbd "C-M-<down>") 'org-metadown)))

;; ORG Capture: I don't use org capture from within emacs.
;; Instead I spwan a new instance of emacs client, that lives only for me to
;; type in the capture. By default it opens a scratch buffer and I don't want
;; it, so this makes sure that after selecting the template, all windows are
;; dead.
(add-hook 'org-capture-mode-hook
          (lambda ()
            (if org-capture-mode
                (delete-other-windows))))

(defun md/org-capture-finish-kill-client ()
  (let ((key (plist-get org-capture-plist :key))
        (desc (plist-get org-capture-plist :description)))
    (delete-frame)))
(add-hook 'org-capture-after-finalize-hook 'md/org-capture-finish-kill-client)

;; Emacs as the Command Center of the Universe
(server-start)
(split-window-horizontally)

;; CUSTOM:
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diredp-compressed-file-suffix ((t (:foreground "cyan"))))
 '(diredp-dir-name ((t (:foreground "#7474FFFFFFFF"))))
 '(diredp-omit-file-name ((t (:inherit diredp-ignored-file-name))))
 '(linum ((t (:inherit default :background "#272822" :foreground "#5b5c57" :underline nil)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" default)))
 '(org-export-backends (quote (ascii beamer html icalendar latex man md odt)))
 '(package-selected-packages
   (quote
    (dired-subtree dired-open dired-hide-dotfiles org-bullets fill-column-indicator org-superstar markdown-mode+ markdown-mode haskell-mode drag-stuff highlight-operators cypher-mode magit web-mode csharp-mode monokai-theme expand-region buffer-move auto-complete)))
 '(safe-local-variable-values (quote ((TeX-master . t))))
 '(send-mail-function (quote mailclient-send-it)))


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
