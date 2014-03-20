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


(defun ov (beg end &rest properties)
  ;; To pass properties to `ov-set'
  (when (listp (car-safe properties))
    (setq properties (car properties)))
  (let ((o (make-overlay beg end)))
    (ov-set o properties)
    o))
;; (ov 20 50 'face 'helm-match 'display "aaa")
;; (ov 20 50 '(face helm-match display "ttt"))
;; (ov-set (ov-line-at) 'face 'helm-match 'display "aaa")
;; (ov-set (ov-line-at) '(face helm-match display "ttt"))


;;;###autoload
(defun ov-clear (&optional beg end property value)
  (interactive)
  (let ((args (cond ((and (numberp beg) (numberp end))
                     (list beg end property value))
                    ((and (not property) (not value))
                     (list nil nil beg end)))))
    (overlay-recenter (point-max))
    (apply 'remove-overlays args)))
;; (ov-clear (point) (point-max) 'aaa t)
;; (ov-clear 'aaa t)                      ; Clear overlay if it has aaa property and its value t
;; (ov-clear)                             ; Clear all overlay

(defmacro ov-reset (ov-or-ovs-variable)
  `(progn
     (mapc (lambda (ov)
             (delete-overlay ov))
           (if (listp ,ov-or-ovs-variable)
               ,ov-or-ovs-variable
             (cons ,ov-or-ovs-variable nil)))
     (setq ,ov-or-ovs-variable nil)))

(defun ov-list ()
  (overlays-in (point-min) (point-max)))

(defun ov-match (string)
  "Return overlay list"
  (save-excursion
    (goto-char (point-min))
    (let (ov-or-ovs)
      (overlay-recenter (point-max))
      (while (search-forward string nil t)
        (setq ov-or-ovs (cons (make-overlay (match-beginning 0) (match-end 0)) ov-or-ovs)))
      ov-or-ovs)))

(defun ov-regexp (regexp)
  "Return overlay list"
  (save-excursion
    (goto-char (point-min))
    (let (ov-or-ovs)
      (overlay-recenter (point-max))
      (while (re-search-forward regexp nil t)
        (setq ov-or-ovs (cons (make-overlay (match-beginning 0) (match-end 0)) ov-or-ovs)))
      ov-or-ovs)))

;;(setq aaq (ov-regexp "def"))
;;(setq aaq (ov-match "def"))
;;(ov-set aaq '(face helm-match aaa t))
;;(ov-clear aaq)
;;(ov-spec aaq)
;;(ov-reset aaq)

;;(setq bbq (ov-line-at))
;;(ov-set bbq 'face 'helm-match)
;;(ov-map-clear bbq)

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


(defun ov-spec (ov-or-ovs)
  (or (listp ov-or-ovs) (setq ov-or-ovs (cons ov-or-ovs nil)))
  (mapcar (lambda (ov)
            (list (overlay-start ov) (overlay-end ov)
                  (overlay-buffer ov) (overlay-properties ov)))
          ov-or-ovs))

(defalias 'ov-recenter 'overlay-recenter)
(defalias 'ov-move 'move-overlay)

(defun ov-create (beg end)
  (make-overlay beg end))
(defalias 'ov-make   'ov-create)

(defun ov-props (ov)
  (overlay-properties ov))

(defun ov-set (ov-or-ovs &rest properties)
  (unless (and ov-or-ovs properties)
    (error "Error: arguments are OV and PROPERTIES"))
  (or (listp ov-or-ovs) (setq ov-or-ovs (cons ov-or-ovs nil)))
  (when (listp (car-safe properties))
    (setq properties (car properties)))
  (let ((len (length properties))
        (i 0))
    (unless (evenp len)
      (error "Error: invalid properties pairs"))
    (mapc (lambda (ov)
            (while (< i len)
              (overlay-put ov (nth i properties) (nth (setq i (1+ i)) properties))
              (setq i (1+ i)))
            (setq i 0))
          ov-or-ovs)))
(defalias 'ov-put 'ov-set)
;; (ov-set (ov-line-at) '(face helm-match))
;; (ov-set (ov-line-at) 'face 'helm-match)
;; (ov-set (ov-line-at))


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

(defun ov-line-at (&optional point)
  (let (o)
    (save-excursion
      (goto-char (or point (point)))
      (setq o (make-overlay (point-at-bol) (min (1+ (point-at-eol)) (point-max)))))
    o))

;; (setq ooo (ov-line-at))
;; (ov-read-only ooo)
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
