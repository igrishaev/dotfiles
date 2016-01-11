;; cask
(package-initialize)
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; ido
(ido-mode t)

;; slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-net-coding-system 'utf-8-unix)

;; autocomplete
(ac-config-default)
(global-auto-complete-mode t)

;; nyan
(nyan-mode t)

;; helm
(helm-mode 1)
(global-set-key (kbd "C-x i") 'helm-imenu)
(global-set-key (kbd "<M-return>") 'helm-etags-select)
(global-set-key (kbd "<s-return>") 'helm-find-files)
(global-set-key (kbd "<C-M-return>") 'select-tags-table)
(global-set-key (kbd "M-x") 'helm-M-x)

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

(defun hl-todos ()
  (highlight-lines-matching-regexp "todo|TODO|Todo" 'hi-yellow-b))

;; markdown
(add-hook 'markdown-mode-hook #'auto-fill-mode)

;;raml
(add-to-list 'auto-mode-alist '("\\.raml\\'" . yaml-mode))

;; python
(setenv "PYTHONDONTWRITEBYTECODE" "1")
(setq python-check-command "flake8 --max-line-length=80 --count")
(add-hook 'cider-mode-hook (lambda () (show-paren-mode 1)))
;; (add-hook 'python-mode-hook #'highlight-parentheses-mode)
(add-hook 'python-mode-hook #'outline-minor-mode)
(add-hook 'python-mode-hook #'anaconda-mode)
;; (add-hook 'python-mode-hook (lambda () (hl-line-mode t)))
(add-hook 'python-mode-hook #'hl-trace)
(add-hook 'python-mode-hook #'hl-todos)

;; jedi
;; (setq jedi:complete-on-dot t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:server-args
;;       '("--virtual-env" "deleted"))
;; (global-set-key (kbd "<C-c .>") 'jedi:goto-definition)


(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; flycheck
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; theme
;; (load-theme 'solarized t)
(set-face-attribute 'default nil :height 140)

;; move text up/down
(global-set-key [s-up] 'move-text-up)
(global-set-key [s-down] 'move-text-down)

;; move text left/right
(defun my-indent-region (N)
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N 2))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(defun my-unindent-region (N)
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N -2))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(global-set-key [s-right] 'my-indent-region)
(global-set-key [s-left] 'my-unindent-region)

;; clojure
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'highlight-parentheses-mode)
(add-hook 'cider-mode-hook #'imenu-add-menubar-index)

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
(defalias 'mb 'magit-blame-mode)
(defalias 'mc 'magit-checkout)
(defalias 'mpl 'magit-pull)
(defalias 'mps 'magit-pull)
(defalias 'mcb 'magit-create-branch)

;; common
(show-paren-mode 1)
;; (setq show-paren-style 'parenthesis)
;; (setq show-paren-style 'expression)
(blink-cursor-mode 0)
(tool-bar-mode -1)
(global-linum-mode t)
(setq auto-save-default nil)
(setq make-backup-files nil)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq system-uses-terminfo nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq multi-term-program "/bin/bash")
(global-set-key (kbd "s-d") 'delete-backward-char)
(put 'downcase-region 'disabled nil)

;; settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))))
