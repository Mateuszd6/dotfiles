;; My emacs config with lot of features taken from more 'modern' editors. Currently
;; supports only C/C++, Python, and what emacs supports by default.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            GLOBAL            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start package.el with emacs and MELPA to repository list.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; Some use package stuff that seems to be important...
(eval-when-compile
  (require 'use-package))

;; Add a path to manually installed packages.
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Disable the defun
(setq-default ring-bell-function 'ignore)

;;; Make undo buffer as large as possible.
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Change the ~files directory and make more of them.
(setq backup-directory-alist `(("." . "~/.backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Very important for me, I'm very used to this behaviour.
(delete-selection-mode t)

(transient-mark-mode 0)

(global-visible-mark-mode t)
(set-face-attribute 'visible-mark-active nil :background "#00FF00")

;; Hide menu bar, tool bar and scorll bars.
(menu-bar-mode -1)
(tool-bar-mode 0)
(scroll-bar-mode -1)

;; Remove fringe.
(set-fringe-mode '(0 . 0))

;; Hard to call it smooth scroll, but as smooth as possible.
(setq scroll-step 8)

;; Show line numbers.
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format "%4d  ") ;; \u2502

;;;; Use ivy.
;; (ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
;; (setq enable-recursive-minibuffers nil)

;;;; EXPERIMENTAL: Use ido instead.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(flx-isearch-mode)
(flx-ido-mode 1) ; better/faster matching
(setq ido-create-new-buffer 'always) ; don't confirm to create new buffers
;; (ido-vertical-mode 1)
;; (setq ido-vertical-define-keys 'C-n-and-C-p-only)

(flx-isearch-mode)                                                         ;;

;; Use this comment tool replacement for emacs, because defauls are pretty bad.
(load "newcomment")
(require 'newcomment)

;; Highlight numbers in most programming modes.
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; HACK: Make highlight-parentheses work with autopair
;; TODO: No it does not work for now... :D
(add-hook 'highlight-parentheses-mode-hook
          (lambda ()
            (setq autopair-handle-action-fns
                  (append
                   (if autopair-handle-action-fns
                       autopair-handle-action-fns
                       '(autopair-default-handle-action))
                       '((lambda (action pair pos-before)
                        (hl-paren-color-update)))))))

;; Enable autopair in all buffers
(autopair-global-mode)

;; Use company-mode.
(add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load "company-autoloads"
  (global-company-mode 1)
  (setq company-tooltip-limit 20
        company-minimum-prefix-length 1
        company-echo-delay 0
        company-begin-commands '(self-insert-command
                                 c-electric-lt-gt c-electric-colon
                                 completion-separator-self-insert-command)
        company-idle-delay 0
        company-show-numbers t
        company-tooltip-align-annotations t)
  (setq company-selection-wrap-around t))

(use-package company-insert-selected
  :bind (:map company-active-map
              ("TAB" . company-simple-complete-next)
              ("<tab>" . company-simple-complete-next)
              ("<S-tab>" . company-simple-complete-previous)
              ("<backtab>" . company-simple-complete-previous)
              ("<up>" . nil)
              ("<down>" . nil)
              ( "RET". nil)))

;; Add more space for filling a paragraph.
(setq-default fill-column 80)

;; Bright-red TODO:
(setq fixme-modes '(c-mode c++-mode objc-mode emacs-lisp-mode shell-mode python-mode java-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\|IMPORTANT\\|BUG\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(NOTE\\|HACK\\)" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          KEYBINDINGS         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(keyboard-translate ?\C-t ?\C-c)
(keyboard-translate ?\C-c ?\C-t)
(keyboard-translate ?\C-x ?\C-y)
(keyboard-translate ?\C-y ?\C-x)

;; (set-keyboard-coding-system nil)
;; (set-terminal-coding-system nil)

;; (define-key key-translation-map [?\C-t] [?\C-c])
;; (define-key key-translation-map [?\C-c] [?\C-t])
;; (define-key key-translation-map [?\C-x] [?\C-y])
;; (define-key key-translation-map [?\C-y] [?\C-x])

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

(defun mat-yank ()
  "Yank the text and blink it so that its clearly visible what
have been pasted."
  (interactive)
  (yank)
  (pulse-momentary-highlight-region (mark) (point) 'highlight-yanked-code))

(defun mat-yank-pop ()
  "Similar functionality to 'mat-yank', but it does yank-pop
instead of yank command."
  (interactive)
  (yank-pop)
  (pulse-momentary-highlight-region (mark) (point) 'highlight-yanked-code))

(global-set-key (kbd "C-v") 'mat-yank)
(global-set-key (kbd "M-v") 'mat-yank-pop)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-x") 'kill-whole-line)


(require 'misc)
(defun mat-jump-right ()
  "TODO: Some description"
  (interactive)
  (while (and (looking-at-p "[\s]") (not (looking-at-p "$")))
    (forward-char)
    )
  (if (looking-at-p "$")
      (forward-char)
    (if (looking-at-p "[_a-zA-Z0-9]")
        (while (and (looking-at-p "[_a-zA-Z0-9]") (not (looking-at-p "$")))
          (forward-char)
          )
      ;; TODO, IDEA: Remove '_' from the next statement so that it is treated as
      ;; no alpha character and as alpha whatever is needed.
      (while (and (looking-at-p "[^_a-zA-Z0-9\s]") (not (looking-at-p "$")))
        (forward-char)
        )
      )
    )
  )

(defun mat-jump-left ()
  "TODO: Some description"
  (interactive)
  (while (and (looking-back "[[:space:]]" 1) (not (looking-back "^" 1)))
    (backward-char)
    )
  (if (looking-back "^" 1)
        (backward-char)
    (if (looking-back "[_a-zA-Z0-9]" 1)
        (while (and (looking-back "[_a-zA-Z0-9]" 1) (not (looking-back "^" 1)))
          (backward-char)
          )
      ;; TODO, IDEA: Remove '_' from the next statement so that it is treated as
      ;; no alpha character and as alpha whatever is needed.
      (while (and (looking-back "[^_a-zA-Z0-9\s]" 1) (not (looking-back "^" 1)))
        (backward-char)
        )
      )
    )
  )

(defun mat-jump-right-select ()
  (interactive)
  (unless (use-region-p)
    (progn
      (set-mark-command nil)
      (setq transient-mark-mode '(only . OLDVAL))
      (setq activate-mark t)))
  (mat-jump-right))

(defun mat-jump-right-unselect ()
  (interactive)
  (when (use-region-p)
      (setq deactivate-mark t))
  (mat-jump-right))

(defun mat-jump-left-select ()
  (interactive)
  (unless (use-region-p)
    (progn
      (set-mark-command nil)
      (setq transient-mark-mode '(only . OLDVAL))
      (setq activate-mark t)))
  (mat-jump-left))

(defun mat-jump-left-unselect ()
  (interactive)
  (when (use-region-p)
      (setq deactivate-mark t))
  (mat-jump-left))

(defun mat-ctrl-backspace ()
  (interactive)
  (set-mark (point))
  (mat-jump-left)
  (kill-region (mark) (point)))

;; This makes deleting and word jumping much more normal (Its very similar to VScode)
;; (global-set-key (kbd "<C-right>") 'mat-jump-right-unselect)
;; (global-set-key (kbd "<C-S-right>") 'mat-jump-right-select)
;; (global-set-key (kbd "<C-left>") 'mat-jump-left-unselect)
;; (global-set-key (kbd "<C-S-left>") 'mat-jump-left-select)

(global-set-key (kbd "<C-backspace>") 'mat-ctrl-backspace)

(defun mat-save-buffer ()
  "Save the buffer after untabifying it, and deleting trailing whitespaces."
  (interactive)
  (save-excursion
    (save-restriction
      ;; Dont untabify when in makefile.
      (unless (member major-mode '(makefile-gmake-mode))
        (widen)
        (untabify (point-min) (point-max)))))
  (delete-trailing-whitespace)
  (save-buffer))

(global-set-key (kbd "C-s") 'mat-save-buffer)

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

(global-set-key (kbd "M-E") 'save-buffers-kill-emacs)

(global-set-key (kbd "M-w") 'other-window)
(global-set-key (kbd "<C-prior>") 'switch-to-prev-buffer)
(global-set-key (kbd "<C-next>") 'switch-to-next-buffer)

(global-set-key (kbd "<f4>") 'magit-status)

(with-eval-after-load 'magit-status
  (define-key magit-status-mode-map (kbd "M-w") nil))

;; NOTE: this works in terminal, but not with gui, the second once oposite way!
;; I-search and query-replace (global-set-key "" 'isearch-forward)
(global-set-key (kbd "C-/") 'isearch-forward)
(global-set-key (kbd "C-f") 'isearch-forward)

(defun my/query-replace (from-string to-string &optional delimited start end)
  "Replace some occurrences of FROM-STRING with TO-STRING.  As
each match is found, the user must type a character saying what
to do with it. This is a modified version of the standard
`query-replace' function in `replace.el', This modified version
defaults to operating on the entire buffer instead of working
only from POINT to the end of the buffer. For more information,
see the documentation of `query-replace'"
  (interactive
   (let ((common
          (query-replace-read-args
           (concat "Query replace"
                   (if current-prefix-arg " word" "")
                   (if (and transient-mark-mode mark-active) " in region" ""))
           nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
           (if (and transient-mark-mode mark-active)
               (region-beginning)
             (buffer-end -1))
           (if (and transient-mark-mode mark-active)
               (region-end)
             (buffer-end 1)))))
  (perform-replace from-string to-string t nil delimited nil nil start end))

(defun my/query-replace-regexp (regexp to-string &optional delimited start end)
  "Replace some things after point matching REGEXP with TO-STRING.  As each
match is found, the user must type a character saying what to do with
it. This is a modified version of the standard `query-replace-regexp'
function in `replace.el', This modified version defaults to operating on the
entire buffer instead of working only from POINT to the end of the
buffer. For more information, see the documentation of `query-replace-regexp'"
  (interactive
   (let ((common
          (query-replace-read-args
           (concat "Query replace"
                   (if current-prefix-arg " word" "")
                   " regexp"
                   (if (and transient-mark-mode mark-active) " in region" ""))
           t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
           (if (and transient-mark-mode mark-active)
               (region-beginning)
             (buffer-end -1))
           (if (and transient-mark-mode mark-active)
               (region-end)
             (buffer-end 1)))))
  (perform-replace regexp to-string t t delimited nil nil start end))

(global-set-key (kbd "C-r") 'query-replace)

(add-hook 'isearch-mode-hook
          (function
           (lambda ()
             (define-key isearch-mode-map "\C-h" 'isearch-mode-help)
             (define-key isearch-mode-map "\C-r" 'isearch-toggle-regexp)
             (define-key isearch-mode-map "\C-c" 'isearch-toggle-case-fold)
             (define-key isearch-mode-map "\C-w" 'isearch-toggle-word)
             (define-key isearch-mode-map (kbd "C-/") 'isearch-edit-string)
             (define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
             (define-key isearch-mode-map (kbd "C-S-n") 'isearch-repeat-backward))))

;; Goto line:
(global-set-key (kbd "M-g") 'goto-line)
;; Files and buffers:
(global-set-key (kbd "M-l") 'counsel-locate)
(global-set-key (kbd "M-f") 'find-file)
(global-set-key (kbd "M-F") 'find-file-other-window)
(global-set-key (kbd "M-R") 'revert-buffer)
(global-set-key (kbd "M-b") 'switch-to-buffer)
(global-set-key (kbd "M-B") 'switch-to-buffer-other-window)
(global-set-key (kbd "M-s") 'counsel-imenu)
(global-set-key (kbd "<C-tab>") 'indent-region)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-S-w") 'kill-word)

(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "M-K") 'kill-buffer-and-window)

;; NOTE: It works because I never ever split windows, so there are always two on my screen.
(require 'buffer-move)
(defun mat-swap-buffers ()
  (interactive)
  (if (condition-case nil (buf-move-left) (error nil))
      ()
    (buf-move-right)))
(global-set-key (kbd "M-W") 'mat-swap-buffers)

;; Shortcuts for moving stuff around.
(require 'drag-stuff)
(global-set-key (kbd "C-M-<up>")   #'drag-stuff-up)
(global-set-key (kbd "C-M-<down>") #'drag-stuff-down)
(global-set-key (kbd "C-M-<left>") #'drag-stuff-left)
(global-set-key (kbd "C-M-<right>") #'drag-stuff-right)

(global-set-key (kbd "<M-left>") 'beginning-of-line)
(global-set-key (kbd "<M-right>") 'end-of-line)
(global-set-key (kbd "<M-down>") 'scroll-up-command)
(global-set-key (kbd "<M-up>") 'scroll-down-command)

(global-set-key (kbd "<f8>") 'next-error)
(global-set-key (kbd "S-<f8>") 'previous-error)

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-N") 'previous-error)

(global-set-key (kbd "<f7>") 'flycheck-next-error)
(global-set-key (kbd "S-<f7>") 'flycheck-previous-error)

(global-set-key (kbd "M-q") 'fill-paragraph)

;; Commenting and lines blocks:
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key (kbd "C-?") 'comment-box)

(global-set-key (kbd "M-.") (lambda ()
                              (interactive)
                              (execute-kbd-macro(kbd "->"))))

(global-set-key (kbd "M-SPC") (lambda ()
                              (interactive)
                              (execute-kbd-macro(kbd "_"))))

;; Expand region:
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)

;; Quick calc:
(global-set-key (kbd "M-c") 'quick-calc)

;; Multiple cursor support:
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-d") 'mc/skip-to-next-like-this)

;; Dumb jump is pretty dumb, but it does it's job pretty well in most
;; cases anyway.
(global-set-key (kbd "M-d") 'dumb-jump-go-current-window)
(global-set-key (kbd "M-D") 'dumb-jump-go-other-window)

;; This makes isearch search text if one is selected.
(defadvice isearch-mode (around isearch-mode-default-string
                                (forward &optional regexp op-fun recursive-edit word-p)
                                activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

;; The custom search URLs
(defvar *internet-search-urls*
  (quote ("http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
          "http://en.wikipedia.org/wiki/Special:Search?search=")))
;;; Search a query on the Internet using the selected URL.
(defun search-in-internet (arg)
  "Searches the internet using the ARGth custom URL for the marked text.
  If a region is not selected, prompts for the string to search
  on. The prefix number ARG indicates the Search URL to use. By
  default the search URL at position 1 will be used."
  (interactive "p")
  ;; Some sanity check.
  (if (> arg (length *internet-search-urls*))
      (error "There is no search URL defined at position %s" arg))
  (let ((query                          ; Set the search query first.
         (if (region-active-p)
             (buffer-substring (region-beginning) (region-end))
           (read-from-minibuffer "Search for: ")))
        ;; Now get the Base URL to use for the search
        (baseurl (nth (1- arg) *internet-search-urls*)))
    ;; Add the query parameter
    (let ((url
           (if (string-match "%s" baseurl)
               ;; If the base URL has a %s embedded, then replace it
               (replace-match query t t baseurl)
             ;; Else just append the query string at end of the URL
             (concat baseurl query))))
      ;; Now browse the URL
      (browse-url url))))
;; My custom google search.
(setq-default browse-url-browser-function 'browse-url-firefox)
(global-set-key (kbd "M-?") 'search-in-internet)

;; NOTE: supported languages so far:
;;       * C
;;       * C++
(defun get-default-command-based-file-language (compiled-file-name)
  "Given a file try to guess the best command to compile it. Note that
it doesn't have to work great and in most cases it's best just to
create a makefile and run it if the project is more than one file."
  (setq language (file-name-extension compiled-file-name))
  (cond
   ;; C
   ((string= language "c")
    (concat (concat (concat
                     "gcc -Wall -Wextra -Wshadow --std=c11 -o "
                     (concat (file-name-directory compiled-file-name) (file-name-base compiled-file-name)))
                    " ")
            compiled-file-name))
   ;; C++
   ((string= language "cpp")
    (concat (concat (concat
                     "g++ -Wall -Wextra -Wshadow --std=c++11 -o "
                     (concat (file-name-directory compiled-file-name) (file-name-base compiled-file-name)))
                    " ")
            compiled-file-name))
   ;; This is a default case:
   (1 "make -e")))

(defun guess-command-compile-file ()
  (interactive)
  (setq make-dir (get-makefile-dir))
  (if make-dir (setq compile-command (concat (concat "cd " make-dir) " && make"))
    (setq compile-command (get-default-command-based-file-language buffer-file-name)))
  (setq compiled-buffer
        (if make-dir (concat make-dir "/bin/program")
          (concat "./" (file-name-base buffer-file-name))))
  (call-interactively 'compile))

(global-set-key (kbd "C-b") 'guess-command-compile-file)

(defun get-makefile-dir (&optional startdir)
  "Move up directories until we find a makefile. If we manage to find
  it, return the containing directory. Else if we get to the toplevel
  directory and still can't find it, return nil. Start at startdir or
  . if startdir not given"
  (interactive)
  (let ((dirname (expand-file-name
                  (if startdir startdir ".")))
        (found nil) ; found is set as a flag to leave loop if we find it
        (top nil))  ; top is set when we get
    (while (not (or found top)) ; If we're at / set top flag.
      (if (string= (expand-file-name dirname) "/")
          (setq top t))
      (if (file-exists-p (expand-file-name "makefile" dirname)) ; Check for the file
          (setq found t)
        (setq dirname (expand-file-name ".." dirname)))) ; If not, move up a directory
    (if found dirname nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            C/C++             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set default style.
(c-add-style "microsoft"
             '("stroustrup"
               (c-offsets-alist
                (innamespace . +)
                (inline-open . 0)
                (inher-cont . c-lineup-multi-inher)
                (arglist-cont-nonempty . +)
                (template-args-cont . +)
                (c-set-offset 'case-label '+))))

(setq c-default-style "linux"
      c-basic-offset 4)

(c-set-offset 'case-label '+)

;; This is a fix because C-d shortcut seems to be overriten in c-mode.
;; (define-key c-mode-map "\C-d/" 'mc/mark-next-like-this))
(defun my-c-fix-hook ()
  (define-key c-mode-base-map "\C-d" 'mc/mark-next-like-this))

(add-hook 'c-mode-hook 'my-c-fix-hook)
(add-hook 'c++-mode-hook
          (lambda ()
            (highlight-operators-mode)
            (local-unset-key (kbd "\C-d"))
            (local-set-key (kbd "\C-d") 'mc/mark-next-like-this)))

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

(defun my-c-mode-common-hook ()
  (font-lock-add-keywords
   nil
   '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; (defun my-c++-fix-hook ()
;;   (define-key c++-mode-map "\C-d" 'mc/mark-next-like-this))
;; (add-hook 'c++-mode-hook 'my-c++-fix-hook)

;; Highlight operatoras for C/C++ modes.
(add-hook 'c-mode-hook 'highlight-operators-mode)

(add-hook 'objc-mode-hook 'highlight-operator-mode)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map
    [remap completion-at-point] 'counsel-irony)
  (define-key irony-mode-map
    [remap complete-symbol] 'counsel-irony))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(require 'company-irony-c-headers)
;; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

;; (add-hook 'c++-mode-hook 'flycheck-mode)
;; (add-hook 'c++-mode-hook (lambda ()
;;                            (setq flycheck-clang-language-standard "c++11")))
;; (add-hook 'c-mode-hook 'flycheck-mode)
;; (add-hook 'c-mode-hook (lambda ()
;;                          (setq flycheck-clang-language-standard "c11")))
;; (add-hook 'objc-mode-hook 'flycheck-mode)

;; Use flycheck for C/C++
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; C/C++ compile command:

;; The name of currently compiled buffer and a shell program will
;; execute if compilation has succeded.
(setq compiled-buffer "No-buffer-selected!")

(defun run-compiled-program(buffer msg)
  "If compilation has finished run program in a separated
terminal. Note that it starts separated terminal, because emacs
integrated terminal is at least not the best..."
  (if (string-match "^finished" msg)
      (with-temp-buffer
        (call-process-shell-command (concat
                                     "gnome-terminal -- ~/.config/run-console-program.sh "
                                     compiled-buffer))
        t)))

(add-hook 'c-mode-hook (lambda ()
                         (add-to-list 'compilation-finish-functions
                                      'run-compiled-program)))

(add-hook 'c++-mode-hook (lambda ()
                           (add-to-list 'compilation-finish-functions
                                        'run-compiled-program)))

(defun casey-find-corresponding-file ()
  "Find the file that corresponds to this one."
  (interactive)
  (setq CorrespondingFileName nil)
  (setq BaseFileName (file-name-sans-extension buffer-file-name))
  (if (string-match "\\.c" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if (string-match "\\.h" buffer-file-name)
      (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
        (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
  (if (string-match "\\.hin" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".cin")))
  (if (string-match "\\.cin" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".hin")))
  (if (string-match "\\.cpp" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".hpp")))
  (if CorrespondingFileName (find-file CorrespondingFileName)
    (error "Unable to find a corresponding file")))
(defun casey-find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (find-file-other-window buffer-file-name)
  (casey-find-corresponding-file)
  (other-window -1))

(defun c-find-corresponding-file-hook ()
  (define-key c-mode-map "\ec" 'casey-find-corresponding-file)
  (define-key c-mode-map "\eC" 'casey-find-corresponding-file-other-window))

(add-hook 'c-mode-common-hook 'c-find-corresponding-file-hook)

(defun c++-find-corresponding-file-hook ()
  (define-key c++-mode-map "\ec" 'casey-find-corresponding-file)
  (define-key c++-mode-map "\eC" 'casey-find-corresponding-file-other-window))

(add-hook 'c++-mode-hook 'c++-find-corresponding-file-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            PYTHON            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (elpy-enable)

;; (define-key elpy-mode-map (kbd "<C-up>") nil)
;; (define-key elpy-mode-map (kbd "<C-down>") nil)

;; (define-key elpy-mode-map (kbd "<f12>") 'xref-find-definitions)

;; (define-key elpy-mode-map (kbd "<f8>") 'elpy-flymake-next-error)
;; (define-key elpy-mode-map (kbd "S-<f8>") 'elpy-flymake-previous-error)
;; (define-key elpy-mode-map (kbd "<f7>") 'elpy-flymake-next-error)
;; (define-key elpy-mode-map (kbd "S-<f7>") 'elpy-flymake-previous-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            ORG MODE          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          POST INIT           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Visual stuff:
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-10.5"))
(load-theme 'monokai t)

(global-hl-line-mode t)
(set-face-background 'hl-line "#1e1f1c")

(when (display-graphic-p)
  (split-window-horizontally))

;; Some custom stuff, that it's best not to touch.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("53f97243218e8be82ba035ae34c024fd2d2e4de29dc6923e026d5580c77ff702" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(jdee-server-dir "/home/mateusz/.emacs.d/jdee-server/target")
 '(package-selected-packages
   (quote
    (org-bullets flx-isearch flx-ido cmake-mode realgud tuareg csharp-mode flycheck-irony elpy visible-mark magit multiple-cursors expand-region highlight-parentheses autopair highlight-operators highlight-numbers dumb-jump flycheck company-irony-c-headers company-irony company auto-complete irony drag-stuff monokai-theme ivy))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
