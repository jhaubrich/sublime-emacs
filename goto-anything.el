;; instant recursive grep on a directory with helm
(defun instant-rgrep-using-helm ()
  "Recursive grep in a directory."
  (interactive)
  (let ((helm-after-initialize-hook #'helm-follow-mode))
    (helm-do-grep)))


;; instant search across all buffers with helm
(defun instant-search-using-helm ()
  "Multi-occur in all buffers backed by files."
  (interactive)
  (let ((helm-after-initialize-hook #'helm-follow-mode))
    (helm-multi-occur
     (delq nil
           (mapcar (lambda (b)
                     (when (buffer-file-name b) (buffer-name b)))
                   (buffer-list))))))

;; set keybindings
(global-set-key (kbd "C-M-s") 'instant-search-using-helm)
(global-set-key (kbd "C-M-S-s") 'helm-resume)
(global-set-key (kbd "C-M-g") 'instant-rgrep-using-helm)
