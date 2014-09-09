(defvar sublime-emacs-map (make-keymap) "sublimating emacs...")

;;
;; Vendor
;;

;; Scrolling (don't jump 1/2 the page when reaching the bottom)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
  
(require 'autopair)
(autopair-global-mode)

(projectile-global-mode)

(add-to-list 'load-path "~/.emacs.d/vendor/ido-hacks")
(require 'ido-hacks)
(ido-mode 1)
(ido-everywhere 1)

; works, but drag-stuff looks to be better 
(require 'move-text)
(define-key sublime-emacs-map [M-C-up]              'move-text-up)
(define-key sublime-emacs-map [M-C-down]            'move-text-down)

; CMD-d and more
(require 'multiple-cursors)
(define-key sublime-emacs-map (kbd "C-c C-")        'mc/edit-lines)
(define-key sublime-emacs-map (kbd "M-d")           'mc/mark-next-like-this)
(define-key sublime-emacs-map (kbd "s-SPC")         'set-rectangular-region-anchor)
(define-key sublime-emacs-map (kbd "C-<")           'mc/mark-previous-like-this)
(define-key sublime-emacs-map (kbd "C-c C-<")       'mc/mark-all-like-this)
(global-unset-key (kbd "s-<down-mouse-1>"))
(global-set-key (kbd "s-<mouse-1>")                 'mc/add-cursor-on-click)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

;; replace ^L with horizontal lines
(global-page-break-lines-mode)


;;
;; Basics
;;

;; super/hyper (http://whattheemacsd.com/)
(setq mac-option-modifier 'super)  ; breaks mac-special-chars
(setq ns-function-modifier 'hyper)

;; Cursor aestetic
(blink-cursor-mode t)
(setq-default cursor-type 'bar)

;; CUA mode
(cua-mode t)
(setq cua-keep-region-after-copy t)
(setq cua-enable-cua-keys nil)  ;; We're going to use our own (osx) keys
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq mouse-drag-copy-region nil)  ; stops selection with a mouse being immediately injected to the kill ring
(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill) ; shift-click to extend region

(define-key sublime-emacs-map (kbd "<escape>")      'keyboard-quit)
(define-key sublime-emacs-map (kbd "M-s")           'save-buffer)
(define-key sublime-emacs-map (kbd "M-w")           'kill-buffer)
;(define-key sublime-emacs-map (kbd "M-w")           'kill-buffer-and-window)

(defun sublime-files ()
  "Open files using projectile if in a project. Closest
approximation to sublime projects. Better, in fact."
  (interactive)
  (if (projectile-project-p)
      (helm-projectile)
    (helm-for-files)))
(define-key sublime-emacs-map (kbd "M-o")           'sublime-files)

(defun xah-new-empty-buffer ()
  "Open a new empty buffer. Not default sublime."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
(define-key sublime-emacs-map (kbd "M-t")           'xah-new-empty-buffer)

;(define-key sublime-emacs-map (kbd "M-x")          'execute-extended-command) ;M-x
(define-key sublime-emacs-map (kbd "C-.")           'smex)
(define-key sublime-emacs-map (kbd "M-.")           'smex) ; emacs default for find-tags

(setq smex-prompt-string "⌘ ")

; Standard OS X bindings
(define-key sublime-emacs-map (kbd "M-z")           'undo)
(define-key sublime-emacs-map (kbd "M-c")           'kill-ring-save)
(define-key sublime-emacs-map (kbd "M-x")           'kill-region)
(define-key sublime-emacs-map (kbd "M-v")           'yank)

; movement
(define-key sublime-emacs-map (kbd "M-<up>")        'beginning-of-buffer)
(define-key sublime-emacs-map (kbd "M-<down>")      'end-of-buffer)
(define-key sublime-emacs-map (kbd "M-<right>")     'end-of-line)
(define-key sublime-emacs-map (kbd "M-<left>")      'beginning-of-line)

; small change to emacs defaults
(define-key sublime-emacs-map (kbd "M-n")           'forward-paragraph)
(define-key sublime-emacs-map (kbd "M-p")           'backward-paragraph)
;; vim/emacs expiriment
;; (define-key sublime-emacs-map (kbd "M-h")           'backward-char)
;; (define-key sublime-emacs-map (kbd "M-j")           'next-line)
;; (define-key sublime-emacs-map (kbd "M-k")           'previous-line)
;; (define-key sublime-emacs-map (kbd "M-l")           'forward-char)


(define-key global-map (kbd "RET") 'newline-and-indent)
; Newline above and below
; http://stackoverflow.com/questions/2173324/emacs-equivalents-of-vims-dd-o-o
(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))
(define-key sublime-emacs-map (kbd "M-<return>")    'vi-open-line-below)

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))
(define-key sublime-emacs-map (kbd "M-S-<return>")  'vi-open-line-above)


; select line
(defun select-current-line (arg)
  "Select the current line"
  (interactive "p")
  (if (memq last-command '(select-current-line))
      (next-line)
    (set-mark (line-beginning-position)))
    (end-of-line))
(define-key sublime-emacs-map (kbd "M-l")           'select-current-line)

; duplicate current line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(define-key sublime-emacs-map (kbd "M-D")           'duplicate-line)

;; Indent
;; Just use python-mode indent. Set to 4 spaces. :shrug: good enough for now.
(define-key sublime-emacs-map (kbd "M-]")           'python-indent-shift-right)
(define-key sublime-emacs-map (kbd "M-[")           'python-indent-shift-left)


;; Wonderful minibuffer advice found here: https://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs/5340797#5340797
(define-minor-mode sublime-emacs
  "A minor mode so that my key settings override annoying major modes."
  t " sublime" 'sublime-emacs-map)

(sublime-emacs 1)

(defun sublime-minibuffer-setup-hook ()
  (sublime-emacs 0))

;;(setq minor-mode-overriding-map-alist 'sublime-emacs-map)
;;(setq minor-mode-map-alist sublime-emacs-map)
(provide 'sublime-emacs)
