;; My emacs config with lot of features taken from more 'modern'
;; editors. Currently supports only C/C++ languages.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            GLOBAL            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start package.el with emacs and MELPA to repository list.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

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

;; Hide menu bar, tool bar and scorll bars.
(menu-bar-mode -1)
(tool-bar-mode 0)
(scroll-bar-mode -1)

;;; Use ivy.
(ivy-mode 1)

;; Remove fringe.
(set-fringe-mode '(0 . 0))

;; Show line numbers.
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format "%4d  ") ;; \u2502

;;; Use ivy.
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; Use cua-mode:
;; (cua-mode t)
;; (setq cua-keep-region-after-copy t) ; Standard Windows behaviour

;; HACK: This crazy thing makes me able to use C-x binding more
;; fluently because emacs desn't wait for the rest of the command, as
;; there is only one command starting by C-t. However emacs
;; suggestions about the keyboard shortucuts will be probobly wrong.
;; (keyboard-translate ?\C-t ?\C-x)
;; (keyboard-translate ?\C-x ?\C-t)
;; (keyboard-translate ?\C-\\ ?\C-c)
;; (keyboard-translate ?\C-c ?\C-\\

(global-set-key (kbd "C-x") 'kill-region)
(global-set-key (kbd "C-c") 'copy-region-as-kill)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "M-v") 'yank-pop)
;; (global-set-key (kbd "C-S-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-z") 'undo)

;; Replace cursor with this fancy mode:
(load "cursor-chg")
(require 'cursor-chg)
;; Turn on cursor change when Emacs is idle
(toggle-cursor-type-when-idle 1) 
;; Turn on change for overwrite, read-only, and input mode
(change-cursor-mode 1)

;; Use this comment tool replacement for emacs, because defauls are
;; pretty bad.
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
(add-hook 'highlight-parentheses-mode-hook
          '(lambda ()
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
(global-set-key (kbd "C-SPC") 'company-complete) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          KEYBINDINGS         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "M-w") 'other-window)

;; I-search and query-replace
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

(global-set-key (kbd "C-h") 'my/query-replace)
(global-set-key (kbd "C-S-H") 'my/query-replace-regexp)

(add-hook 'isearch-mode-hook
	  (function
	   (lambda ()
	     (define-key isearch-mode-map "\C-h" 'isearch-mode-help)
	     (define-key isearch-mode-map "\C-r" 'isearch-toggle-regexp)
	     (define-key isearch-mode-map "\C-c" 'isearch-toggle-case-fold)
	     (define-key isearch-mode-map "\C-f" 'isearch-edit-string)
	     (define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
	     (define-key isearch-mode-map (kbd "C-S-n") 'isearch-repeat-backward))))

;; Goto line:
(global-set-key (kbd "M-g") 'goto-line)

;; Files and buffers:
(global-set-key (kbd "M-l") 'counsel-locate)
(global-set-key (kbd "M-f") 'find-file)
(global-set-key (kbd "M-F") 'find-file-other-window)
(global-set-key (kbd "<C-tab>") 'switch-to-buffer)
(global-set-key (kbd "<C-iso-lefttab>") 'switch-to-buffer-other-window)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-k") 'kill-this-buffer)
(global-set-key (kbd "M-k") 'kill-buffer-and-window)

;; Shortcuts for moving stuff around.
(require 'drag-stuff)
(global-set-key (kbd "M-<up>")   #'drag-stuff-up)
(global-set-key (kbd "M-<down>") #'drag-stuff-down)
(global-set-key (kbd "M-<left>") #'drag-stuff-left)
(global-set-key (kbd "M-<right>") #'drag-stuff-right)

(global-set-key (kbd "<f8>") 'next-error)
(global-set-key (kbd "S-<f8>") 'previous-error)
(global-set-key (kbd "<f7>") 'flycheck-next-error)
(global-set-key (kbd "S-<f7>") 'flycheck-previous-error)

(global-set-key (kbd "C-e") 'move-end-of-line)
(global-set-key (kbd "C-q") 'move-beginning-of-line)

;; Commenting lines and blocks:
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key (kbd "C-?") 'comment-box)

;; Expand region:
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Quick calc:
(global-set-key (kbd "M-S-c") 'quick-calc)

;; Multiple cursor support:
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-d") 'mc/skip-to-next-like-this)

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
(global-set-key (kbd "M-p") 'search-in-internet)

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
  (if make-dir (setq compile-command (concat (concat "pushd " make-dir) " && make ; popd"))
    (setq compile-command (get-default-command-based-file-language buffer-file-name)))
  (setq compiled-buffer (file-name-base buffer-file-name))
  (call-interactively 'compile))

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

;; Very fancy and completly usless eshel prompt.
(defmacro with-face (str &rest properties)
    `(propertize ,str 'face (list ,@properties)))

(defun shk-eshell-prompt ()
  (let ((header-bg "#fff"))
    (concat
     (with-face (concat (eshell/pwd) " ") :background header-bg)
     (with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time))
		:background header-bg :foreground "#888")
     (with-face
      (or (ignore-errors (format "(%s)"
				 (vc-responsible-backend default-directory))) "")
      :background header-bg)
     (with-face "\n" :background header-bg)
     (with-face user-login-name :foreground "blue")
     "@"
     (with-face "localhost" :foreground "green")
     (if (= (user-uid) 0)
	 (with-face " #" :foreground "red")
       " $")
     " ")))
(setq eshell-prompt-function 'shk-eshell-prompt)
(setq eshell-highlight-prompt nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            C/C++             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set default style.
(c-add-style "microsoft"
	     '("stroustrup"
	       (c-offsets-alist
		(innamespace . -)
		(inline-open . 0)
		(inher-cont . c-lineup-multi-inher)
		(arglist-cont-nonempty . +)
		(template-args-cont . +))))

(setq c-default-style "microsoft"
      c-basic-offset 4)

;; This is a fix because C-d shortcut seems to be overriten in c-mode.
;; (define-key c-mode-map "\C-d/" 'mc/mark-next-like-this))
(defun my-c-fix-hook ()
  (define-key c-mode-map "\C-d" 'mc/mark-next-like-this))
(add-hook 'c-mode-hook 'my-c-fix-hook)

(defun my-c++-fix-hook ()
  (define-key c++-mode-map "\C-d" 'mc/mark-next-like-this))
(add-hook 'c++-mode-hook 'my-c++-fix-hook)

;; Highlight operatoras for C/C++ modes. Not sure if i like this.
(add-hook 'c-mode-hook 'highlight-operators-mode)
(add-hook 'c++-mode-hook 'highlight-operator-mode)
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

;; Use flycheck for C/C++
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook (lambda ()
			   (setq flycheck-clang-language-standard "c++11")))
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook (lambda ()
			 (setq flycheck-clang-language-standard "c11")))
(add-hook 'objc-mode-hook 'flycheck-mode)

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
			"gnome-terminal -- ~/run-console-program.sh ./"
			compiled-buffer))
	t)))

(add-hook 'c-mode-hook (lambda ()
			 (add-to-list 'compilation-finish-functions
				      'run-compiled-program)))

(add-hook 'c++-mode-hook (lambda ()
			 (add-to-list 'compilation-finish-functions
				      'run-compiled-program)))

(global-set-key (kbd "C-b") 'guess-command-compile-file)

(with-eval-after-load "company-autoloads"
  (global-company-mode 1)

  (setq company-tooltip-limit 20
        company-minimum-prefix-length 1
        company-echo-delay 0
        company-begin-commands '(self-insert-command
                                 c-electric-lt-gt c-electric-colon
                                 completion-separator-self-insert-command)
        company-idle-delay 0.2
        company-show-numbers t
        company-tooltip-align-annotations t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          POST INIT           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Visual stuff:
(add-to-list 'default-frame-alist
	     '(font . "DejaVu Sans Mono-10.5"))
(load-theme 'monokai t)

(global-hl-line-mode t)
(set-face-background 'hl-line "#1e1f1c")
(split-window-horizontally)

;; Some custom stuff, that it's best not to touch.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("53f97243218e8be82ba035ae34c024fd2d2e4de29dc6923e026d5580c77ff702" default)))
 '(package-selected-packages
   (quote
    (magit multiple-cursors expand-region highlight-parentheses autopair highlight-operators highlight-numbers dumb-jump flycheck company-irony-c-headers company-irony company auto-complete irony drag-stuff monokai-theme ivy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
