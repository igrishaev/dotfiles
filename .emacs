;; packages
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(require 'package)
(package-initialize)
;; (package-refresh-contents)

(setq my-packages
      '(
        auto-complete
        cider
        projectile
        clojure-mode
        expand-region
        helm
        jinja2-mode
        magit
        markdown-mode
        nyan-mode
        paredit
        prodigy
        wrap-region
        yaml-mode
        json-mode
        ))

(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; https://github.com/clojure-emacs/cider/
;; (add-to-list 'load-path "~/.emacs.d/cider-0.14.0")
;; (require 'cider)

;; credentials
(setq user-full-name   "Ivan Grishaev")
(setq user-mail-address "ivan@grishaev.me")

;; tramp
(setq tramp-default-method "ssh")

;; dired mode
(setq dired-recursive-deletes 'top)

;; projectile
(projectile-mode t)

;; imenu
(setq imenu-auto-rescan t)
(setq imenu-use-popup-menu nil)

;; ido
(ido-mode t)
(icomplete-mode t)
(ido-everywhere t)
(setq ido-virtual-buffers t)
(setq ido-enable-flex-matching t)

;; buffers
(require 'bs)
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer)
(global-set-key (kbd "<f2>") 'bs-show)

;; lisp
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq inferior-lisp-program "lein run -m clojure.main")

;; slime
(setq slime-net-coding-system 'utf-8-unix)

;; autocomplete
(ac-config-default)
(setq ac-disable-faces nil)
(setq ac-auto-start t)
(add-to-list 'ac-modes   'lisp-mode)
(add-to-list 'ac-sources 'ac-source-semantic)
(add-to-list 'ac-sources 'ac-source-variables)
(add-to-list 'ac-sources 'ac-source-functions)
(add-to-list 'ac-sources 'ac-source-dictionary)
(add-to-list 'ac-sources 'ac-source-words-in-all-buffer)
(add-to-list 'ac-sources 'ac-source-files-in-current-dir)
(global-auto-complete-mode t)

;; bookmarks
(setq bookmark-save-flag t)
(when (file-exists-p (concat user-emacs-directory "bookmarks"))
  (bookmark-load bookmark-default-file t))
(global-set-key (kbd "<f3>") 'bookmark-set)
(global-set-key (kbd "<f4>") 'bookmark-jump)
(global-set-key (kbd "<f5>") 'bookmark-bmenu-list)
(setq bookmark-default-file
      (concat user-emacs-directory "bookmarks"))

;; nyan
(nyan-mode t)

;; helm
(helm-mode 1)
(global-set-key (kbd "C-x i") 'helm-imenu)
(global-set-key (kbd "<M-return>") 'helm-etags-select)
(global-set-key (kbd "<s-return>") 'helm-find-files)
(global-set-key (kbd "<C-M-return>") 'select-tags-table)
(global-set-key (kbd "M-x") 'helm-M-x)

;; OS copy-paste
(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

(global-set-key (kbd "C-c c") 'pbcopy)
(global-set-key (kbd "C-c v") 'pbpaste)
(global-set-key (kbd "C-c x") 'pbcut)

;; html
(add-hook 'html-mode-hook
  (lambda ()
    (set (make-local-variable 'sgml-basic-offset) 4)))

;; tags
(defun tags-create (dir-name)
  (interactive "DDirectory: ")
  (let ((path (directory-file-name dir-name)))
    (shell-command
     (format "ctags -e -R -f %s/TAGS %s" path path))
    (visit-tags-table (format "%s/TAGS" path))))

;; highlight
(defun hl-trace ()
  (highlight-lines-matching-regexp "set_trace" 'hi-red-b))

;; markdown
(add-hook 'markdown-mode-hook #'auto-fill-mode)
(add-hook 'markdown-mode-hook (lambda () (set-fill-column 80)))

;;raml
(add-to-list 'auto-mode-alist '("\\.raml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.wl\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.emacs\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.gnus\\'" . lisp-mode))

;; python
(setenv "PYTHONDONTWRITEBYTECODE" "1")
(setq python-check-command "flake8 --max-line-length=80 --count") ;; pylint
(add-hook 'cider-mode-hook (lambda () (show-paren-mode 1)))
(add-hook 'python-mode-hook #'outline-minor-mode)
(add-hook 'python-mode-hook #'anaconda-mode)
(add-hook 'python-mode-hook #'hl-trace)
(add-hook 'python-mode-hook #'ruler-mode)
(add-hook 'python-mode-hook (lambda () (set-fill-column 80)))

;; todos
(defun hl-todos ()
  (highlight-lines-matching-regexp "todo\\|TODO\\|Todo" 'hi-yellow-b))
(add-hook 'python-mode-hook #'hl-todos)

;; theme
(load-theme 'light-blue t)

;; clojure
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'imenu-add-menubar-index)
(setq cider-font-lock-dynamically nil)
(setq cider-repl-use-pretty-printing t)

;; clojurescript mode
(add-hook 'clojurescript-mode #'paredit-mode)
(add-hook 'clojurescript-mode #'paredit-mode)

;; sql
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; aliases
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'rs 'replace-string)
(defalias 'sl 'sort-lines)
(defalias 'rr 'reverse-region)
(defalias 'rg 'rgrep)
(defalias 'dir 'dired)
(defalias 'fnd 'find-name-dired)
(defalias 'df 'diff-mode)
(defalias 'mt 'multi-term)
(defalias 'rb 'rename-buffer)
(defalias 'rev 'revert-buffer)
(defalias 'him 'helm-imenu)
(defalias 'hl 'highlight-phrase)
(defalias 'hlr 'highlight-regexp)
(defalias 'uhl 'unhighlight-regexp)
(defalias 'oc 'occur)
(defalias 'ms 'magit-status)
(defalias 'blame 'magit-blame)

;; UI
(tooltip-mode      -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(blink-cursor-mode -1)
(setq use-dialog-box nil)
(setq redisplay-dont-pause t)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq frame-title-format "Emacs %b")

;; syntax
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; pairs
(electric-pair-mode t)
;; (electric-indent-mode t)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis) ;; 'expression

;; fringe
(require 'fringe)
(fringe-mode '(8 . 0))
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; formats
(setq display-time-24hr-format t)
(display-time-mode t)
(size-indication-mode t)

;; line wrapping
(setq word-wrap t)
(global-visual-line-mode t)

;; no backups
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)

;; encodings
(setq default-buffer-file-coding-system 'utf-8)
(setq-default coding-system-for-read 'utf-8)
(setq file-name-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)

;; Line numbers
(line-number-mode   t)
(global-linum-mode  t)
(column-number-mode t)
(setq linum-format " %d")

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-c i") #'er/mark-inside-pairs)
(global-set-key (kbd "C-c o") #'er/mark-outside-pairs)
(global-set-key (kbd "C-c w") #'er/mark-method-call)

;; common
(delete-selection-mode t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq system-uses-terminfo nil)
(setq indent-line-function 'insert-tab)
(setq multi-term-program "/bin/bash")
(put 'downcase-region 'disabled nil)
(set-face-attribute 'default nil :height 140)

;; global keys
(global-unset-key "\C-z") ;; it crushes my Emacs
(global-set-key (kbd "s-d") 'delete-backward-char)

;; path
(setq exec-path (append exec-path '("/usr/local/bin" "/Library/TeX/texbin")))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Library/TeX/texbin"))

;; movement
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))

;; tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width          4)
(setq-default c-basic-offset     4)
(setq-default standart-indent    4)
(setq-default lisp-body-indent   2)
(setq lisp-indent-function  'common-lisp-indent-function)

;; new line/spaces
(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; org-mode
(setq system-time-locale "C")
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(global-set-key (kbd "C-c t") 'org-timeline)
(setq calendar-week-start-day 1)

;; window resize
(global-set-key (kbd "C-<up>") 'enlarge-window)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)

;; remapping
(setq mac-command-modifier 'control)

;; custom keys

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-h") 'split-window
(global-set-key (kbd "C-h") 'split-window-horizontally)
(global-set-key (kbd "C-v") 'split-window-vertically)
(global-set-key (kbd "C-b") 'switch-to-buffer)

;; prodigy
(prodigy-define-service
  :name "PostgreSQL"
  :command "postgres"
  :args '("-E" "-D" "/usr/local/var/postgres")
  :tags '(dev)
  :stop-signal 'sigterm
  :kill-process-buffer-on-stop nil)
