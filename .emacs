;; envs
(setenv "PYTHONDONTWRITEBYTECODE" "1")

(setq exec-path (append exec-path '("/usr/local/bin")))

; (set-keyboard-coding-system nil)

(require 'imenu)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(require 'ido)
(ido-mode t)

(setq inferior-lisp-program "/usr/local/bin/sbcl")


(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)


;; common lisp
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; slime
(add-to-list 'load-path "~/.emacs.d/slime")
(require 'slime)
(setq slime-net-coding-system 'utf-8-unix)

;; the most important feature!
(require 'nyan-mode)
(nyan-mode t)

; tags
; ctags --recurse=yes --languages=python -e --exclude=migrations .
; ctags --recurse=yes --languages=javascript -e  --exclude=plugins .

(defun tags-create (dir-name)
  (interactive "DDirectory: ")
  (let ((path (directory-file-name dir-name)))
    (shell-command
     (format "ctags -e -R -f %s/TAGS %s" path path))
    (visit-tags-table (format "%s/TAGS" path))))

;; helm
(require 'helm)
(helm-mode 1)
(global-set-key (kbd "<C-return>") 'helm-imenu)
(global-set-key (kbd "<M-return>") 'helm-etags-select)
(global-set-key (kbd "<s-return>") 'helm-find-files)
(global-set-key (kbd "<C-M-return>") 'select-tags-table)

(global-set-key (kbd "s-d") 'delete-backward-char)

;; jedi
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:server-args
      '(
        "--virtual-env" "deleted"
        ))


(defun hl-trace ()
  (highlight-lines-matching-regexp "set_trace" 'hi-red-b))

(defun hl-todos ()
  (highlight-lines-matching-regexp "todo|TODO|Todo" 'hi-yellow-b))

(add-hook 'python-mode-hook #'hl-trace)
(add-hook 'python-mode-hook #'hl-todos)


(global-set-key (kbd "<C-c .>") 'jedi:goto-definition)

;; python-mode
;; (add-hook 'python-mode-hook #'highlight-parentheses-mode)
;; (add-hook 'python-mode-hook #'outline-minor-mode)
;; (add-hook 'python-mode-hook (lambda () (ggtags-mode 1)))
;; (add-hook 'python-mode-hook (lambda () (hl-line-mode t)))

(setq python-check-command "flake8 --max-line-length=80 --count")

(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'python-mode-hook (lambda () (add-to-list 'write-file-functions 'python-check)))




;; html
(add-hook 'html-mode-hook
  (lambda ()
    (set (make-local-variable 'sgml-basic-offset) 4)))

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; (require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (setq flycheck-flake8-maximum-line-length 120)

(require 'multi-term)
(setq multi-term-program "/bin/zsh")

;;(load-theme 'solarized-light t)
(color-theme-deep-blue)
(set-face-attribute 'default nil :height 140)


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

;; (require 'move-text)
(global-set-key [s-up] 'move-text-up)
(global-set-key [s-down] 'move-text-down)


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

;; scala
;; (add-to-list 'load-path "~/ensime/elisp/")
;; (require 'ensime)

;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; cider
(require 'cider)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'highlight-parentheses-mode)
(add-hook 'cider-mode-hook #'imenu-add-menubar-index)
(add-hook 'cider-mode-hook (lambda () (show-paren-mode 1)))

;; sql
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; prodigy
;; deleted
(prodigy-define-service
  :name "memcached"
  :path "/usr/bin/"
  :command "memcached"
  :cwd "/usr/bin/"
  :tags '(dev)
  :kill-process-buffer-on-stop t
  )

(prodigy-define-service
  :name "redis"
  :path "/usr/local/bin/"
  :command "redis-server"
  :cwd "/usr/local/bin/"
  :tags '(dev)
  :kill-process-buffer-on-stop t
)

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

;; settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jabber-account-list
   (quote
    (("deleted@deleted.net"
      (:connection-type . starttls)))))
 '(send-mail-function (quote mailclient-send-it))
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))))
(put 'upcase-region 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
