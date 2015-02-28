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

(ido-mode 1)
(add-to-list 'load-path "~/.emacs.d/vendor/ido-hacks")
(require 'ido-hacks nil t)
(ido-everywhere 1)
(require 'ido-vertical-mode)  ;; I <3 u
(ido-vertical-mode 1)
(if (commandp 'ido-vertical-mode) 
    (progn
      (ido-vertical-mode 1)
      (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)))

;; Revisit `helm-mode' for `helm-grep', `helm-regexp', and other amazing stuff
;; (https://lists.gnu.org/archive/html/help-gnu-emacs/2014-01/msg00440.html)
;; (require 'helm-config) 
;; (helm-mode 1)
;; (find-file-read-only . ido)

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
(define-key sublime-emacs-map (kbd "M-w")           'quit-window)
;(define-key sublime-emacs-map (kbd "M-w")           'kill-buffer-and-window)

;; TODO: This `find' overrides an important emacs convention. Perhaps
;; it's best to learn the emacs way in this case
;; (define-key sublime-emacs-map (kbd "M-f")           'isearch-forward)
;; (define-key sublime-emacs-map (kbd "M-F")           'rgrep)

;; TODO: the default `comment-dwim' might again be better in this case
;; (define-key sublime-emacs-map (kbd "M-/")           'subl/comment-line)


(defun subl/show-files ()
  "Open files using projectile if in a project. Closest
approximation to sublime projects. Better, in fact."
  ;; { "keys": ["super+p"], "command": "show_overlay", "args": {"overlay": "goto", "show_files": true} },
  (interactive)
  (if (projectile-project-p)
      (helm-projectile)
    (helm-for-files)))
(define-key sublime-emacs-map (kbd "M-o")           'helm-for-files)
(define-key sublime-emacs-map (kbd "M-O")           'subl/show-files)

(defun xah-new-empty-buffer ()
  "Open a new empty buffer. Not default sublime."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
(define-key sublime-emacs-map (kbd "M-t")           'xah-new-empty-buffer)

;; TODO: Use sublime's command_palette binding (M-P)
;; { "keys": ["super+shift+p"], "command": "show_overlay", "args": {"overlay": "command_palette"} },
(setq smex-prompt-string "⌘ ")
(define-key sublime-emacs-map (kbd "C-.")           'smex)
(define-key sublime-emacs-map (kbd "C-;")           'smex)
(define-key sublime-emacs-map (kbd "M-.")           'smex) ; emacs default for find-tags
;(define-key sublime-emacs-map (kbd "M-x")          'execute-extended-command) ;M-x

;; M-k is the sublime cord default
;; probably going to override this to be smex
(define-prefix-command 'sublime-k)
(global-set-key (kbd "M-k") 'sublime-k)  ; defaut: kill-sentence (wont be missed)
(define-key sublime-k (kbd "s") 'split-window-horizontally) ; PERSONAL
(define-key sublime-k (kbd "o") 'delete-other-windows)	    ; PERSONAL
;; { "keys": ["super+k", "super+b"], "command": "toggle_side_bar" },
;; { "keys": ["super+k", "super+u"], "command": "upper_case" },
(define-key sublime-k (kbd "M-u") 'upcase-word)
;; { "keys": ["super+k", "super+l"], "command": "lower_case" },
(define-key sublime-k (kbd "M-l") 'downcase-word)
;; { "keys": ["super+k", "super+space"], "command": "set_mark" },
;; { "keys": ["super+k", "super+a"], "command": "select_to_mark" },
;; { "keys": ["super+k", "super+w"], "command": "delete_to_mark" },
;; { "keys": ["super+k", "super+x"], "command": "swap_with_mark" },
;; { "keys": ["super+k", "super+g"], "command": "clear_bookmarks", "args": {"name": "mark"} },
;; { "keys": ["super+k", "super+y"], "command": "yank" },
;; { "keys": ["super+k", "super+k"], "command": "run_macro_file", "args": {"file": "res://Packages/Default/Delete to Hard EOL.sublime-macro"} },
;; { "keys": ["super+k", "super+backspace"], "command": "run_macro_file", "args": {"file": "res://Packages/Default/Delete to Hard BOL.sublime-macro"} },
;; { "keys": ["super+k", "super+c"], "command": "show_at_center" },



; Standard OS X bindings
(define-key sublime-emacs-map (kbd "M-z")           'undo)
(define-key sublime-emacs-map (kbd "M-c")           'kill-ring-save)
(define-key sublime-emacs-map (kbd "M-x")           'kill-region)
(define-key sublime-emacs-map (kbd "M-v")           'yank)
(define-key sublime-emacs-map (kbd "M-a")           'mark-whole-buffer)
(define-key sublime-emacs-map (kbd "M-'")           'er/expand-region)


; movement
(define-key sublime-emacs-map (kbd "M-<up>")        'beginning-of-buffer)
(define-key sublime-emacs-map (kbd "M-<down>")      'end-of-buffer)
(define-key sublime-emacs-map (kbd "M-<right>")     'end-of-line)
(define-key sublime-emacs-map (kbd "M-<left>")      'beginning-of-line)

; small change to emacs defaults
(define-key sublime-emacs-map (kbd "M-n")           'forward-paragraph)   ; PERSONAL
(define-key sublime-emacs-map (kbd "M-p")           'backward-paragraph)  ; PERSONAL
;; vim/emacs expiriment
;; (define-key sublime-emacs-map (kbd "M-h")           'backward-char)
;; (define-key sublime-emacs-map (kbd "M-j")           'next-line)
;; (define-key sublime-emacs-map (kbd "M-k")           'previous-line)
;; (define-key sublime-emacs-map (kbd "M-l")           'forward-char)


(define-key global-map (kbd "RET") 'newline-and-indent)
; Newline above and below
; http://stackoverflow.com/questions/2173324/emacs-equivalents-of-vims-dd-o-o
(defun subl/open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))
(define-key sublime-emacs-map (kbd "M-<return>")    'subl/open-line-below)

(defun subl/open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))
(define-key sublime-emacs-map (kbd "M-S-<return>")  'subl/open-line-above)


; select line
(defun subl/expand-selection-to-line (arg)
  "Select the current line"
  ;; { "keys": ["super+l"], "command": "expand_selection", "args": {"to": "line"} },
  ;; TODO: Doesn't expand current selected region to line
  (interactive "p")
  (if (memq last-command '(subl/expand-selection-to-line))
      (progn
	(next-line)
	(beginning-of-line))
      (progn
	(set-mark (line-beginning-position))
	(next-line)
	(beginning-of-line))))
(define-key sublime-emacs-map (kbd "M-l")           'subl/expand-selection-to-line)

; duplicate current line
(defun subl/duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(define-key sublime-emacs-map (kbd "M-D")           'subl/duplicate-line)

;; Indent
;; Just use python-mode indent. Set to 4 spaces. :shrug: good enough for now.
;; TODO: This only works after a python file has been opened
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
