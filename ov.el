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
    (let (ovs)
      (overlay-recenter (point-max))
      (while (search-forward string nil t)
        (setq ovs (cons (make-overlay (match-beginning 0) (match-end 0)) ovs)))
      ovs)))

(defun ov-regexp (regexp)
  (save-excursion
    (goto-char (point-min))
    (let (ovs)
      (overlay-recenter (point-max))
      (while (re-search-forward regexp nil t)
        (setq ovs (cons (make-overlay (match-beginning 0) (match-end 0)) ovs)))
      ovs)))

(defun ov-map-set (ovs properties)
  (or (listp ovs) (setq ovs (cons ovs nil)))
  (unless (evenp (length properties))
    (error "Error: invalid properties pairs"))
  (mapc (lambda (ov)
          (let ((props properties))
            (while props
              (overlay-put ov (pop props) (pop props)))))
        ovs)
  nil)
(defalias 'ov-map-put 'ov-map-set)

(defmacro ov-map-clear (ovs)
  (or (listp ovs) (setq ovs (cons ovs nil)))
  `(progn
     (mapc (lambda (ov)
             (delete-overlay ov))
           ,ovs)
     (if (symbolp ',ovs)
         (setq ,ovs nil))))

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

(defun ov-map-spec (ovs)
  (or (listp ovs) (setq ovs (cons ovs nil)))
  (mapcar (lambda (ov)
            (list (overlay-start ov) (overlay-end ov)
                  (overlay-buffer ov) (overlay-properties ov)))
          ovs))

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
      (ov-map-set o properties))))
;; (ov-line-at '(face helm-match))
;; (ov-line-at '(face helm-match) 4000)
;; (ov-timeout 0.6 '(ov-line-at '(face helm-match)) '(ov-clear-all))



















(provide 'ov)
;;; ov.el ends here
