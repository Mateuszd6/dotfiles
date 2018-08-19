;; == Automatically install packages from the list ==
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(package-refresh-contents)

(defun ensure-package-installed (&rest packages)
  (mapcar
   (lambda (package)
     (unless (package-installed-p package)
       (package-install package)))
   packages))

;; Packages to add:
(ensure-package-installed
 'ivy
 'use-package
 'autopair
 'cmake-mode
 'company-irony
 'company-irony-c-headers
 'csharp-mode
 'drag-stuff
 'dumb-jump
 'company
 'highlight-numbers
 'highlight-parentheses
 'highlight-operators
 'irony
 'magit
 'git-commit
 'monokai-theme
 'multiple-cursors
 'visible-mark
 'yasnippet
 'flx-isearch
 'buffer-move
 'expand-region
 'org-bullets
 )
