

;; blank lines
;; M-x flush-lines RET ^$ RET

;; whilespace lines
;; M-x flush-lines RET ^\s-*$ RET

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
        cider
        highlight
        projectile
        clojure-mode
        expand-region
        gist
        ido-vertical-mode
        helm
        jinja2-mode
        auto-complete
        magit
        markdown-mode
        paredit
        wrap-region
        yaml-mode
        json-mode
        groovy-mode
        dockerfile-mode
        ;; helm-cider
        ))

(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; cider
;; (add-to-list 'load-path "~/.emacs.d/cider-0.18.0")
;; (add-to-list 'load-path "~/.emacs.d/cider-master")
;; (add-to-list 'load-path "~/.emacs.d/cider-1.2.0")
(require 'cider)

;; credentials
(setq user-full-name    "Ivan Grishaev")
(setq user-mail-address "ivan@grishaev.me")

;; tramp
(setq tramp-default-method "ssh")

;; dired mode
(setq dired-recursive-deletes 'top)

;; projectile
(projectile-mode t)

;; wrap-region
(require 'wrap-region)
(wrap-region-mode t)
(wrap-region-add-wrapper "<" ">")
(wrap-region-add-wrapper "`" "`")
(wrap-region-add-wrapper "#_" "" "#" 'clojurescript-mode)
(wrap-region-add-wrapper "`" "`" "c" 'markdown-mode)   ;; code
(wrap-region-add-wrapper "`" "`" "с" 'markdown-mode)   ;; same, cyrillic
(wrap-region-add-wrapper "~~~clojure" "~~~" "l" 'markdown-mode)   ;; code block
(wrap-region-add-wrapper "~~~clojure" "~~~" "д" 'markdown-mode)   ;; same, cyrillic
(wrap-region-add-wrapper "(" ")" "(" 'markdown-mode)
(wrap-region-add-wrapper "*" "*" "i" 'markdown-mode)   ;; italic
(wrap-region-add-wrapper "*" "*" "*" 'markdown-mode)   ;; italic
(wrap-region-add-wrapper "**" "**" "b" 'markdown-mode) ;; bold
(wrap-region-add-wrapper "**" "**" "и" 'markdown-mode) ;; same, cyrillic
(wrap-region-add-wrapper "[" "]" "х"   'markdown-mode) ;; [] cyrillic

(wrap-region-add-wrapper "{% include static.html path=\"" "\" %}" "s" 'markdown-mode) ;; blog static


;; auto-complete
(ac-config-default)
(setq global-auto-complete-mode t)


;; LaTex/Clojure book

(add-hook 'latex-mode-hook #'wrap-region-mode)

;; (wrap-region-add-wrapper "\\begin{code}" "\\end{code}" "c"   'latex-mode)
;; (wrap-region-add-wrapper "\\hl{" "}" "h"   'latex-mode)
;; (wrap-region-add-wrapper "\\begin{verbatim}" "\\end{verbatim}" "c"   'latex-mode)

(wrap-region-add-wrapper "\\begin{english}\n  \\begin{clojure}" "  \\end{clojure}\n\\end{english}" "l"   'latex-mode)
(wrap-region-add-wrapper "\\begin{english}\n  \\begin{clojure}" "  \\end{clojure}\n\\end{english}" "д"   'latex-mode)


(wrap-region-add-wrapper "\\ifx\\DEVICETYPE\\MOBILE\n" "\\else \n\\fi " "m"   'latex-mode)


(wrap-region-add-wrapper "\\begin{figure}[h]" "\\end{figure}" "f"   'latex-mode)

(wrap-region-add-wrapper "$\\langle$" "$\\rangle$" "<" 'latex-mode)

(wrap-region-add-wrapper "\\index{" "}" "i" 'latex-mode)
(wrap-region-add-wrapper "\\index{" "}" "ш" 'latex-mode)

(wrap-region-add-wrapper "\\verb|" "|" "h" 'latex-mode)
(wrap-region-add-wrapper "\\emph{" "}" "e" 'latex-mode)
(wrap-region-add-wrapper "<<" ">>" "q" 'latex-mode)
(wrap-region-add-wrapper "\\textbf{" "}" "b" 'latex-mode)

(wrap-region-add-wrapper "``" "''" "`" 'latex-mode)

(wrap-region-add-wrapper "~" "" "t" 'latex-mode)

(wrap-region-add-wrapper "\\verb|" "|" "р" 'latex-mode)
(wrap-region-add-wrapper "\\emph{" "}" "у" 'latex-mode)

(wrap-region-add-wrapper "\\hyphenation{" "}" "y" 'latex-mode)
(wrap-region-add-wrapper "\\hyphenation{" "}" "н" 'latex-mode)

(wrap-region-add-wrapper "\\" "/" "\\" 'latex-mode)
(wrap-region-add-wrapper "\\" "/" "/" 'latex-mode)

(wrap-region-add-wrapper "\\texttt{" "}" "t" 'latex-mode)
(wrap-region-add-wrapper "\\texttt{" "}" "е" 'latex-mode)

(wrap-region-add-wrapper "\\code{" "}" "c" 'latex-mode)
(wrap-region-add-wrapper "\\code{" "}" "с" 'latex-mode)

(wrap-region-add-wrapper "<<" ">>" "й" 'latex-mode)
(wrap-region-add-wrapper "\\textbf{" "}" "и" 'latex-mode)

(wrap-region-add-wrapper "\\'{" "}" "'" 'latex-mode)
(wrap-region-add-wrapper "\\'{" "}" "э" 'latex-mode)

(wrap-region-add-wrapper "\\\"{" "}" "\\" 'latex-mode)


;; imenu
(setq imenu-auto-rescan t)
(setq imenu-use-popup-menu nil)

;; https://www.emacswiki.org/emacs/ImenuMode
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
    ((not symbol-list)
     (let ((ido-mode ido-mode)
           (ido-enable-flex-matching
            (if (boundp 'ido-enable-flex-matching)
                ido-enable-flex-matching t))
           name-and-pos symbol-names position)
       (unless ido-mode
         (ido-mode 1)
         (setq ido-enable-flex-matching t))
       (while (progn
                (imenu--cleanup)
                (setq imenu--index-alist nil)
                (ido-goto-symbol (imenu--make-index-alist))
                (setq selected-symbol
                      (ido-completing-read "Symbol? " symbol-names))
                (string= (car imenu--rescan-item) selected-symbol)))
       (unless (and (boundp 'mark-active) mark-active)
         (push-mark nil t nil))
       (setq position (cdr (assoc selected-symbol name-and-pos)))
       (cond
         ((overlayp position)
          (goto-char (overlay-start position)))
         (t
          (goto-char position)))))
    ((listp symbol-list)
     (dolist (symbol symbol-list)
       (let (name position)
         (cond
           ((and (listp symbol) (imenu--subalist-p symbol))
            (ido-goto-symbol symbol))
           ((listp symbol)
            (setq name (car symbol))
            (setq position (cdr symbol)))
           ((stringp symbol)
            (setq name symbol)
            (setq position
                  (get-text-property 1 'org-imenu-marker symbol))))
         (unless (or (null position) (null name)
                     (string= (car imenu--rescan-item) name))
           (add-to-list 'symbol-names name)
           (add-to-list 'name-and-pos (cons name position))))))))

;; (global-set-key (kbd "<C-i>") 'ido-goto-symbol)

;; ido
(ido-mode t)
(icomplete-mode t)
(ido-everywhere t)
(setq ido-virtual-buffers t)
(setq ido-enable-flex-matching t)
(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read "M-x " (all-completions "" obarray 'commandp))))))

;; finder
(defun show-in-finder ()
  (interactive)
  (shell-command (concat "open -R " buffer-file-name)))

;; buffers
(require 'bs)
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer)
(global-set-key (kbd "<f2>") 'bs-show)

;; spelling
;; brew install aspell --with-lang-ru
(setq ispell-program-name "aspell")
;; (setq ispell-extra-args '("--sug-mode=fast"))

;; lisp
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq inferior-lisp-program "lein run -m clojure.main")

;; slime
(setq slime-net-coding-system 'utf-8-unix)

;; bookmarks
(setq bookmark-save-flag t)
(when (file-exists-p (concat user-emacs-directory "bookmarks"))
  (bookmark-load bookmark-default-file t))
(global-set-key (kbd "<f3>") 'bookmark-set)
(global-set-key (kbd "<f4>") 'bookmark-jump)
(global-set-key (kbd "<f5>") 'bookmark-bmenu-list)
(setq bookmark-default-file
      (concat user-emacs-directory "bookmarks"))


;; helm
;; (helm-mode 1)
;; (global-set-key (kbd "<M-return>") 'helm-etags-select)
(global-set-key (kbd "M-x") 'helm-M-x)
;; (defalias 'him 'helm-imenu)
;; (global-set-key (kbd "<C-m>")   'helm-buffers-list) ;; switch-to-buffers
;; (global-set-key (kbd "C-x C-f") 'helm-find-files) ;; find-file
(global-set-key (kbd "<C-m>")   'switch-to-buffer)
(global-set-key (kbd "C-x C-f") 'find-file)
(global-set-key (kbd "<C-i>") 'helm-imenu)
;; (global-set-key (kbd "<C-i>") 'imenu)
;; (global-set-key (kbd "<C-'>") 'helm-etags-select)
(setq helm-buffer-max-length 30)



;; http://snowsyn.net/2018/10/21/buffer-ordering-with-helm/
(defun nm-around-helm-buffers-sort-transformer (candidates source)
  candidates)

;; (advice-add
;;  'helm-buffers-sort-transformer
;;  :override #'nm-around-helm-buffers-sort-transformer)


;; (global-set-key (kbd "<C-m>")   'switch-to-buffer)
;; (global-set-key (kbd "<C-i>") 'imenu)

;; empty lines

(defun drop-empty-lines ()
  (interactive)
  (flush-lines "^$"))


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

;; tables
(global-set-key (kbd "C-c C-c") 'org-table-align)

;; html
(add-hook 'html-mode-hook
  (lambda ()
    (set (make-local-variable 'sgml-basic-offset) 4)))

;; tags
(global-set-key (kbd "<C-M-return>") 'select-tags-table)
(global-set-key (kbd "C-;") 'xref-find-apropos)

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
(add-hook 'markdown-mode-hook #'wrap-region-mode)

;;raml
(add-to-list 'auto-mode-alist '("\\.raml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.wl\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.emacs\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.gnus\\'" . lisp-mode))

;; python
(setenv "PYTHONDONTWRITEBYTECODE" "1")
(setq python-check-command "flake8 --max-line-length=80 --count") ;; pylint
(add-hook 'python-mode-hook #'outline-minor-mode)
;; (add-hook 'python-mode-hook #'anaconda-mode)
(add-hook 'python-mode-hook #'hl-trace)
(add-hook 'python-mode-hook #'ruler-mode)
(add-hook 'python-mode-hook (lambda () (set-fill-column 80)))

;; files
(setq vc-follow-symlinks t)

;; todos
(defun hl-todos ()
  (highlight-lines-matching-regexp "todo\\|TODO\\|Todo" 'hi-yellow-b))
(add-hook 'python-mode-hook #'hl-todos)

;; theme
(load-theme 'light-blue t)

;; clojure
(add-hook 'cider-mode-hook (lambda () (show-paren-mode 1)))
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'imenu-add-menubar-index)
(add-hook 'cider-mode-hook #'hl-todos)
(setq cider-font-lock-dynamically nil)
(setq cider-repl-display-help-banner nil)
(setq cider-repl-use-pretty-printing t)
(global-set-key (kbd "C-x C-i ") #'cider-inspect-last-sexp)

;; clojure wrappers
(add-hook 'cider-mode-hook #'wrap-region-mode)
(wrap-region-add-wrapper "(clojure.inspector/inspect-tree " ")" "i" 'clojure-mode)

;; clojurescript mode
(add-hook 'clojurescript-mode #'paredit-mode)

;; sql
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))


;; aliases
(defalias 'cc 'cider-connect)
(defalias 'cme 'cider-macroexpand-1)
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
(set-face-attribute 'default nil :height 150)

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
(setq truncate-lines t)
(setq truncate-partial-width-windows t)
(setq global-visual-line-mode nil)

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
(global-set-key (kbd "M-r") #'er/mark-word)
;; (global-set-key (kbd "M-o") #'er/mark-outside-pairs)


;; files
(global-set-key (kbd "M-p") #'projectile-find-file)

;; common
(delete-selection-mode t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq system-uses-terminfo nil)
(setq indent-line-function 'insert-tab)
(setq multi-term-program "/bin/bash")
(put 'downcase-region 'disabled nil)
(set-face-attribute 'default nil :height 170)

;; global keys
(global-unset-key "\C-z") ;; it crushes my Emacs
(global-set-key (kbd "s-d") 'delete-backward-char)

;; path
(setq exec-path (append exec-path '("/usr/local/bin" "/Library/TeX/texbin")))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Library/TeX/texbin"))

;; (global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
;; (global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))

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

;; git
(setq magit-git-executable "/usr/local/bin/git")

;; windows
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>")'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'enlarge-window)
(global-set-key (kbd "C-<up>")   'shrink-window)
(setq split-height-threshold 1200)
(setq split-width-threshold 2000)

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
	     (set-window-dedicated-p window
    				             (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
       "Window '%s' is normal")
   (current-buffer)))

;; line width
(setq-default fill-column 80)

;; remapping
(setq mac-command-modifier 'control)

;; misc
(setq grep-save-buffers nil)
(global-set-key (kbd "M-o") #'browse-url-at-point)

;; custom keys
(define-key input-decode-map "\C-m" [C-m])
(define-key input-decode-map "\C-i" [C-i])
(define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
(define-key input-decode-map [?\C-\]] (kbd "<C-]>"))
(define-key input-decode-map [?\C-\'] (kbd "<C-'>"))

;; files & buffers
(global-set-key (kbd "RET")   'newline-and-indent)
(global-set-key (kbd "M-i")   'ispell)
(global-set-key (kbd "C-o")   'other-window)
(global-set-key (kbd "C-v")   'split-window-horizontally)
(global-set-key (kbd "C-h")   'split-window-vertically)
(global-set-key (kbd "C-t")   'delete-other-windows)
(global-set-key (kbd "C-,")   (lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "C-.")   (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "<C-[>") 'backward-word)
(global-set-key (kbd "<C-]>") 'forward-word)
(global-set-key (kbd "M-n")   'cider-repl-set-ns)
(global-set-key (kbd "M-m")   'magit-status)
(global-set-key (kbd "M-t")   'cider-test-run-test)
(global-set-key (kbd "M-p")   'projectile-find-file)
(global-set-key (kbd "M-h")   'hs-toggle-hiding)
;; (global-set-key (kbd "C-M-i")   'ido-mode)
(global-set-key (kbd "C-M-n")   'switch-to-next-buffer)
(global-set-key (kbd "C-M-p")   'switch-to-prev-buffer)
(global-set-key (kbd "C-M-o")   'other-frame)
(global-set-key (kbd "C-M-e")  'forward-whitespace)


;; speedbar
(speedbar)
(speedbar-toggle-show-all-files)
(global-set-key (kbd "C-M-s") 'speedbar-get-focus)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-command "pandoc")
 '(package-selected-packages
   '(helm slime ido-vertical-mode dockerfile-mode 0blayout highlight gist groovy-mode yaml-mode wrap-region projectile paredit markdown-mode magit json-mode jinja2-mode expand-region dedicated cider auto-complete))
 '(truncate-lines t)
 '(truncate-partial-width-windows nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
