;;; Code:

(require 'cl-lib)

(defgroup ov nil
  "Group for ov.el"
  :prefix "ov-" :group 'development)

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Overlay-Properties.html
(defvar ov-prop-list '(priority
                       window
                       category
                       face
                       mouse-face
                       display
                       help-echo
                       field
                       modification-hooks
                       insert-in-front-hooks
                       insert-behind-hooks
                       invisible
                       intangible
                       isearch-open-invisible
                       isearch-open-invisible-temporary
                       before-string
                       after-string
                       line-prefix
                       wrap-prefix
                       evaporate
                       local-map
                       keymap))

;; Make overlay / Set properties -----------------------------------------------
(defun ov (beg end &rest properties)
  ;; To pass properties to `ov-set'
  (when (listp (car-safe properties))
    (setq properties (car properties)))
  (let ((o (ov-make beg end)))
    (ov-set o properties)
    o))

;; Alias                             ;; Argument
(defalias 'ov-create  'make-overlay) ;; (beg end)
(defalias 'ov-make    'make-overlay) ;; (beg end)

(defun ov-match (string &optional beg end)
  "Return overlay list"
  (save-excursion
    (goto-char (or beg (point-min)))
    (let (ov-or-ovs)
      (ov-recenter (point-max))
      (while (search-forward string end t)
        (setq ov-or-ovs (cons (ov-make (match-beginning 0)
                                            (match-end 0)) ov-or-ovs)))
      ov-or-ovs)))

(defun ov-regexp (regexp &optional beg end)
  "Return overlay list"
  (save-excursion
    (goto-char (or beg (point-min)))
    (let (ov-or-ovs)
      (ov-recenter (point-max))
      (while (re-search-forward regexp end t)
        (setq ov-or-ovs (cons (ov-make (match-beginning 0)
                                            (match-end 0)) ov-or-ovs)))
      ov-or-ovs)))

(defun ov-line (&optional point)
  (let (o)
    (save-excursion
      (goto-char (or point (point)))
      (setq o (ov-make (point-at-bol) (min (1+ (point-at-eol)) (point-max)))))
    o))

(defun ov-region ()
  (if mark-active
      (let ((o (ov-make (region-beginning) (region-end))))
        (deactivate-mark t)
        o)
    (error "Error: Need to make region")))

(defun ov-set (ov-or-ovs &rest properties)
  (unless (and ov-or-ovs properties)
    (error "Error: arguments are OV and PROPERTIES"))
  (or (listp ov-or-ovs) (setq ov-or-ovs (cons ov-or-ovs nil)))
  (when (listp (car-safe properties))
    (setq properties (car properties)))
  (let ((len (length properties))
        (i 0))
    (unless (cl-evenp len)
      (error "Error: invalid properties pairs"))
    (mapc (lambda (ov)
            (while (< i len)
              (overlay-put ov (nth i properties) (nth (setq i (1+ i)) properties))
              (setq i (1+ i)))
            (setq i 0))
          ov-or-ovs)
    nil))
(defalias 'ov-put 'ov-set)


;; Delete overlay --------------------------------------------------------------
;;;###autoload
(defun ov-clear (&optional beg end property value)
  (interactive)
  (let ((args (cond ((and (numberp beg) (numberp end))
                     (list beg end property value))
                    ((and (not property) (not value))
                     (list nil nil beg end)))))
    (ov-recenter (point-max))
    (apply 'remove-overlays args)))

(defmacro ov-reset (ov-or-ovs-variable)
  `(progn
     (mapc (lambda (ov)
             (delete-overlay ov))
           (if (listp ,ov-or-ovs-variable)
               ,ov-or-ovs-variable
             (cons ,ov-or-ovs-variable nil)))
     (setq ,ov-or-ovs-variable nil)))


;; Look up overlay parameters, etc ---------------------------------------------
;; Alias                                ;; Argument
(defalias 'ov-p    'overlayp)           ;; (ov)
(defalias 'ov?     'overlayp)           ;; (ov)
(defalias 'ov-val  'overlay-get)        ;; (ov property)

(defun ov-beg (ov)
  (if (ov? ov) (overlay-start ov) nil))

(defun ov-end (ov)
  (if (ov? ov) (overlay-end ov) nil))

(defun ov-buf (ov)
  (if (ov? ov) (overlay-buffer ov) nil))

(defun ov-prop (ov)
  (if (ov? ov) (overlay-properties ov) nil))

(defun ov-spec (ov-or-ovs)
  (or (listp ov-or-ovs) (setq ov-or-ovs (cons ov-or-ovs nil)))
  (mapcar (lambda (ov)
            (list (ov-beg ov) (ov-end ov)
                  (ov-buf ov) (overlay-properties ov)))
          ov-or-ovs))


;; Get present overlay object --------------------------------------------------
(defun ov-at (&optional point)
  (or point (setq point (point)))
  (car (overlays-at point)))

(defalias 'ov-in 'overlays-in)

(defun ov-all ()
  (ov-in (point-min) (point-max)))

(defun ov-backwards (&optional point)
  (ov-in (point-min) (or point (point))))

(defun ov-forwards (&optional point)
  (ov-in (or point (point)) (point-max)))


;; Overlay manipulation --------------------------------------------------------
;; Alias                                  ;; Argument
(defalias 'ov-recenter 'overlay-recenter) ;; (point)
(defalias 'ov-move     'move-overlay)     ;; (ov beg end &optional buffer)

(defmacro ov-timeout (time func func-after)
  (declare (indent 1))
  `(progn
    ,(if (symbolp func-after)
         (run-with-timer time nil `(lambda () (funcall ',func-after)))
       (run-with-timer time nil `(lambda () ,(funcall `(lambda () ,func-after)))))
    ,(if (symbolp func)
         (funcall func)
       (funcall `(lambda () ,func)))))

(defun ov-next (&optional point property value)
  (or point (setq point (point)))
  (cond ((and (not property) (not value))
         (ov-at (next-overlay-change point)))
        ((and property (not value))
         (save-excursion
           (goto-char (next-overlay-change point))
           (let (ov)
             (while (and (not (memq property (ov-prop (setq ov (ov-at (point))))))
                         (not (eobp)))
               (goto-char (next-overlay-change (point))))
             ov)))
        (t
         (save-excursion
           (goto-char (next-overlay-change point))
           (let (ov)
             (while (and (not (and (memq property (ov-prop (setq ov (ov-at (point)))))
                                   (equal value (ov-val ov property))))
                         (not (eobp)))
               (goto-char (next-overlay-change (point))))
             ov)))))

(defun ov-prev (&optional point property value)
  (or point (setq point (point)))
  (cond ((and (not property) (not value))
         (ov-at (previous-overlay-change point)))
        ((and property (not value))
         (save-excursion
           (goto-char (previous-overlay-change point))
           (let (ov)
             (while (and (not (memq property (ov-prop (setq ov (ov-at (1- (point)))))))
                         (not (bobp)))
               (goto-char (previous-overlay-change (point))))
             ov)))
        (t
         (save-excursion
           (goto-char (previous-overlay-change point))
           (let (ov)
             (while (and (not (and (memq property (ov-prop (setq ov (ov-at (1- (point))))))
                                   (equal value (ov-val ov property))))
                         (not (bobp)))
               (goto-char (previous-overlay-change (point))))
             ov)))))


;; Impliment pseudo read-only overlay function ---------------------------------
(defun ov-read-only (ov-or-ovs)
  (or (listp ov-or-ovs) (setq ov-or-ovs (cons ov-or-ovs nil)))
  (mapc (lambda (ov)
          (overlay-put ov 'modification-hooks    '(ov--read-only))
          (overlay-put ov 'insert-in-front-hooks '(ov--read-only)))
        ov-or-ovs))

;; http://lists.gnu.org/archive/html/emacs-devel/2002-08/msg00428.html
(defvar ov-save-comint-last-prompt-overlay nil)
(defun ov--read-only (overlay after start end &optional len)
  (if (and (not after)
           (or (< (ov-beg overlay) start)
               (> (ov-end overlay) end)))
      (error "")))

(defadvice comint-output-filter (around swap-read-only activate)
  "Add a read-only equivalency to the last prompt overlay."
  ;; Caution: in Emacs <~21.2, a new overlay gets created for each
  ;; prompt... in later versions, text-properties for old prompts
  ;; are used instead, and the original overlay is recycled.  In
  ;; this case, we can advise snapshot-last-prompt to remove the
  ;; read-only *text properties* (not the overlay properties).
  ;; Here we test to ensure the prompt isn't in the same position as
  ;; the process-mark before removing the read-only overlay stuff.
  (when (and ov-save-comint-last-prompt-overlay
             (not (equal
                   (marker-position (process-mark (get-buffer-process
                                                   (current-buffer))))
                   (ov-end
                    ov-save-comint-last-prompt-overlay))))
    (overlay-put ov-save-comint-last-prompt-overlay
                 'modification-hooks nil)
    (overlay-put ov-save-comint-last-prompt-overlay
                 'insert-in-front-hooks' nil))
  ad-do-it
  (when comint-last-prompt-overlay
    (setq ov-save-comint-last-prompt-overlay
          comint-last-prompt-overlay)
    (overlay-put comint-last-prompt-overlay 'intangible t)
    (overlay-put comint-last-prompt-overlay 'modification-hooks
                 '(idlwave-shell-comint-signal-read-only))
    (overlay-put comint-last-prompt-overlay 'insert-in-front-hooks
                 '(idlwave-shell-comint-signal-read-only))))

(defadvice comint-snapshot-last-prompt (after remove-text-read-only activate)
  "Remove the read-only text properties potentially set by snapshot"
  (when comint-last-prompt-overlay
    (remove-text-properties
     (ov-beg comint-last-prompt-overlay)
     (ov-end comint-last-prompt-overlay)
     '(modification-hooks nil insert-in-front-hooks nil))))


(provide 'ov)
;;; ov.el ends here
