;;; Code:

(require 'cl-lib)


(defgroup ov nil
  "Group for ov.el"
  :prefix "ov-" :group 'development)

(defmacro ov (beg end &rest properties)
  (declare (indent 1))
  `(let* ((prop (list ,@properties))
          (length (length prop))
          (pairs (/ length 2))
          (ov (make-overlay ,beg ,end))
          (i 0))
     (unless (evenp length) (error "Error: invalid properties pairs"))
     (while (< i pairs)
       (overlay-put ov (pop prop) (pop prop))
       (setq i (1+ i)))))

;; (ov 3 20 'face 'helm-match 'aaa t)
;; (ov 300 290 'face 'helm-match 'aaa nil)
;; (ov 310 320 'face 'helm-match 'aaa t)
;; (ov-clear-all 'aaa nil)

;;;###autoload
(defun ov-clear-all (&optional property value)
  (interactive)
  (overlay-recenter (point-max))
  (remove-overlays nil nil property value))

(defun ov-clear (&optional beg end property value)
  (let ((args (list beg end property value)))
    (overlay-recenter (point-max))
    (apply 'remove-overlays args)))

(defun ov-list ()
  (overlays-in (point-min) (point-max)))

(defun ov-match (string)
  (save-excursion
    (goto-char (point-min))
    (let (ov-or-ovs)
      (overlay-recenter (point-max))
      (while (search-forward string nil t)
        (setq ov-or-ovs (cons (make-overlay (match-beginning 0) (match-end 0)) ov-or-ovs)))
      ov-or-ovs)))

(defun ov-regexp (regexp)
  (save-excursion
    (goto-char (point-min))
    (let (ov-or-ovs)
      (overlay-recenter (point-max))
      (while (re-search-forward regexp nil t)
        (setq ov-or-ovs (cons (make-overlay (match-beginning 0) (match-end 0)) ov-or-ovs)))
      ov-or-ovs)))

(defun ov-map-set (ov-or-ovs properties)
  (or (listp ov-or-ovs) (setq ov-or-ovs (cons ov-or-ovs nil)))
  (unless (evenp (length properties))
    (error "Error: invalid properties pairs"))
  (mapc (lambda (ov)
          (let ((props properties))
            (while props
              (overlay-put ov (pop props) (pop props)))))
        ov-or-ovs)
  nil)
(defalias 'ov-map-put 'ov-map-set)

(defmacro ov-map-clear (ov-or-ovs)
  (or (listp ov-or-ovs) (setq ov-or-ovs (cons ov-or-ovs nil)))
  `(progn
     (mapc (lambda (ov)
             (delete-overlay ov))
           ,ov-or-ovs)
     (if (symbolp ',ov-or-ovs)
         (setq ,ov-or-ovs nil))))

;;(setq aaq (ov-regexp "def"))
;;(setq aaq (ov-match "def"))
;;(ov-map-set aaq '(face helm-match aaa t))
;;(ov-map-clear aaq)

;;(ov-clear-all)
;; (let ((o (ov-regexp "def")))
;;   (run-with-timer 0.5 nil
;;                   (lambda ()
;;                     (ov-clear-all)))
;;   (ov-map-set o '(face helm-match aaa t))
;;   )

(defmacro ov-timeout (time func func-after)
  (declare (indent 1))
  `(progn
    ,(if (symbolp func-after)
         (run-with-timer time nil `(lambda () (funcall ',func-after)))
       (run-with-timer time nil `(lambda () ,(funcall `(lambda () ,func-after)))))
    ,(if (symbolp func)
         (funcall func)
       (funcall `(lambda () ,func)))))
;; (ov-timeout 0.5
;;   '(ov-map-set (ov-regexp "def") '(face helm-match aaa t))
;;   '(ov-clear-all 'aaa t))
;; (defun fn1 ()
;;   (ov-map-set (ov-regexp "def") '(face helm-match aaa t)))
;; (defun fn2 ()
;;   (ov-clear-all 'aaa t))
;; (ov-timeout 0.5 fn1 fn2)

(defun ov-spec (ov)
  (list (overlay-start ov) (overlay-end ov)
        (overlay-buffer ov) (overlay-properties ov)))

(defun ov-map-spec (ov-or-ovs)
  (or (listp ov-or-ovs) (setq ov-or-ovs (cons ov-or-ovs nil)))
  (mapcar (lambda (ov)
            (list (overlay-start ov) (overlay-end ov)
                  (overlay-buffer ov) (overlay-properties ov)))
          ov-or-ovs))

(defalias 'ov-recenter 'overlay-recenter)

(defun ov-create (beg end)
  (make-overlay beg end))
(defalias 'ov-make   'ov-create)

(defun ov-props (ov)
  (overlay-properties ov))

(defun ov-set (ov property value)
  (overlay-put ov property value))
(defalias 'ov-put 'ov-set)

(defun ov-value (ov property)
  (overlay-get ov property))

(defun ov-at (&optional point)
  (or point (setq point (point)))
  (car (overlays-at point)))

(defun ov-beg (ov)
  (overlay-start ov))

(defun ov-end (ov)
  (overlay-end ov))

(defun ov-p (ov)
  (overlayp ov))
(defalias 'ov? 'ov-p)


(defun ov-line-at (&optional properties point)
  (let (o)
    (save-excursion
      (goto-char (or point (point)))
      (setq o (make-overlay (point-at-bol) (min (1+ (point-at-eol)) (point-max))))
      (when properties
        (ov-map-set o properties)))
    o))
;; (ov-line-at '(face helm-match))
;; (ov-line-at '(face helm-match) 4000)
;; (ov-timeout 0.6 '(ov-line-at '(face helm-match)) '(ov-clear-all))

(setq ooo (ov-line-at))

(ov-read-only ooo)




(defun ov-read-only (ov-or-ovs)
  (or (listp ov-or-ovs) (setq ov-or-ovs (cons ov-or-ovs nil)))
  (mapc (lambda (ov)
          (overlay-put ov 'modification-hooks    '(ov--read-only))
          (overlay-put ov 'insert-in-front-hooks '(ov--read-only)))
        ov-or-ovs))

;; Impliment read-only overlay function
;; http://lists.gnu.org/archive/html/emacs-devel/2002-08/msg00428.html
(defvar ov-save-comint-last-prompt-overlay nil)
(defun ov--read-only (overlay after start end &optional len)
  (if (and (not after)
           (or (< (overlay-start overlay) start)
               (> (overlay-end overlay) end)))
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
                   (overlay-end
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
     (overlay-start comint-last-prompt-overlay)
     (overlay-end comint-last-prompt-overlay)
     '(modification-hooks nil insert-in-front-hooks nil))))

















(provide 'ov)
;;; ov.el ends here
