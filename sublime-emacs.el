
;; Cursor aestetic
(blink-cursor-mode t)
(setq-default cursor-type 'bar)


(projectile-global-mode)

;; CUA mode
(cua-mode t)
(setq cua-keep-region-after-copy t)
(setq cua-enable-cua-keys nil)  ;; We're going to use our own (osx) keys
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq mouse-drag-copy-region nil)  ; stops selection with a mouse being immediately injected to the kill ring



(defvar sublime-emacs-map (make-keymap) "sublimating emacs...")

(define-key sublime-emacs-map (kbd "<escape>")      'keyboard-quit)
(define-key sublime-emacs-map (kbd "M-s")           'save-buffer)
(define-key sublime-emacs-map (kbd "M-w")           'kill-buffer)
;; (define-key sublime-emacs-map (kbd "M-w")           'kill-buffer-and-window)
;; (define-key sublime-emacs-map (kbd "M-o")           'switch-to-buffer)
(defun sublime-files ()
  "Open files using projectile if in a project."
  (interactive)
  (if (projectile-project-p)
      (helm-projectile)
    (helm-for-files)))
(define-key sublime-emacs-map (kbd "M-o")           'sublime-files)

(defun xah-new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
(define-key sublime-emacs-map (kbd "M-t")           'xah-new-empty-buffer)

;(define-key sublime-emacs-map (kbd "M-x") 'execute-extended-command) ;M-x
(define-key sublime-emacs-map (kbd "C-.")           'smex)
;(define-key sublime-emacs-map (kbd "M-p")           'smex)
(setq smex-prompt-string "⌘ ")

; Standard OS X bindings
(define-key sublime-emacs-map (kbd "M-z")           'undo)
(define-key sublime-emacs-map (kbd "M-c")           'kill-ring-save)
(define-key sublime-emacs-map (kbd "M-x")           'kill-region)
(define-key sublime-emacs-map (kbd "M-v")           'yank)

; movement
(define-key sublime-emacs-map (kbd "M-<up>")        'beginning-of-buffer)
(define-key sublime-emacs-map (kbd "M-<down>")      'end-of-buffer)
;; vim/emacs expiriment
;; (define-key sublime-emacs-map (kbd "M-h")           'backward-char)
;; (define-key sublime-emacs-map (kbd "M-j")           'next-line)
;; (define-key sublime-emacs-map (kbd "M-k")           'previous-line)
;; (define-key sublime-emacs-map (kbd "M-l")           'forward-char)


; Sublime basics

; CMD-d and more
(require 'multiple-cursors)
(define-key sublime-emacs-map (kbd "C-c C-")        'mc/edit-lines)
(define-key sublime-emacs-map (kbd "M-d")           'mc/mark-next-like-this)
(define-key sublime-emacs-map (kbd "C-<")           'mc/mark-previous-like-this)
(define-key sublime-emacs-map (kbd "C-c C-<")       'mc/mark-all-like-this)

;; (global-unset-key (kbd "M-<down-mouse-1>"))
;; (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)


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

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun vi-open-line (&optional abovep)
  "Insert a newline below the current line and put point at beginning.
With a prefix argument, insert a newline above the current line."
  (interactive "P")
  (if abovep
      (vi-open-line-above)
    (vi-open-line-below)))

;;(define-key sublime-emacs-map (kbd "<return>") 'newline-and-indent)
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key sublime-emacs-map (kbd "M-<return>")    'vi-open-line-below)
(define-key sublime-emacs-map (kbd "M-S-<return>")  'vi-open-line-above)


; Move selected text
(require 'move-text)  ; works, but drag-stuff looks to be better 
(define-key sublime-emacs-map [M-C-up]              'move-text-up)
(define-key sublime-emacs-map [M-C-down]            'move-text-down)
                    
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
  (yank)
)
(define-key sublime-emacs-map (kbd "M-D")           'duplicate-line)

;; Wonderful minibuffer advice found here: https://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs/5340797#5340797
(define-minor-mode sublime-emacs
  "A minor mode so that my key settings override annoying major modes."
  t " sublime" 'sublime-emacs-map)

(sublime-emacs 1)

(defun sublime-minibuffer-setup-hook ()
  (sublime-emacs 0))


(provide 'sublime-emacs)


; Keeps these out of the minibuffer (which breaks ESC as exit)
;(add-hook 'minibuffer-setup-hook `sublime-minibuffer-setup-hook)

; todo
; m-/    toggle comment (default binding for hippie-expand)
; M-l    select line (http://www.emacswiki.org/emacs/CopyingWholeLines)
; TAB    might not be a problem, seems neat
; M-o    open file (git-find-file?) (dired?)
; M-f    find
; M-[    indent/outdent (http://stackoverflow.com/questions/11623189/how-to-bind-keys-to-indent-unindent-region-in-emacs)
