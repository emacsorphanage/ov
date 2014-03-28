;;; ov.el --- Overlay library for Emacs Lisp -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2014 by Shingo Fukuyama

;; Version: 1.0
;; Author: Shingo Fukuyama - http://fukuyama.co
;; URL: https://github.com/ShingoFukuyama/ov.el
;; Created: Mar 20 2014
;; Keywords: overlay
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Code:


(defun ov-test-insert-dammy-text ()
  (insert
   ";; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.")
  (goto-char (point-min)))


(ert-deftest ov-test/ov ()
  (ov-test-insert-dammy-text)
  (should (ov? (ov 1 10 'face 'warning 'display "fff")))
  (should (ov? (ov 1 10 '(face 'success 'display "eee"))))
  (should (ov? (ov 1 10)))
  (should-error (ov 10)))

(ert-deftest ov-test/ov-make ()
  (ov-test-insert-dammy-text)
  (should (ov? (ov-make 2 5)))
  (should (ov? (ov-make (point-min) (point-max))))
  (should-error (ov-make))
  (should-error (ov-make 10)))

(ert-deftest ov-test/ov-line ()
  (ov-test-insert-dammy-text)
  (should (ov? (ov-line)))
  (should (ov? (ov-line (point-min))))
  (should (ov? (ov-line (point-max))))
  (should (ov? (ov-line (/ (point-max) 2))))
  (should-error (ov-line "a")))

(ert-deftest ov-test/ov-match ()
  (ov-test-insert-dammy-text)
  (should (listp (ov-match "is")))
  (should (ov? (car (ov-match "t"))))
  (should-error (ov-match 'a))
  (should-error (ov-match 1))
  (should-error (ov-match)))

(ert-deftest ov-test/ov-regexp ()
  (ov-test-insert-dammy-text)
  (should (listp (ov-regexp "^;;")))
  (should (ov? (car (ov-regexp "^;;"))))
  (should-error (ov-regexp 'a))
  (should-error (ov-regexp 1))
  (should-error (ov-regexp)))

(ert-deftest ov-test/ov-set ()
  (ov-test-insert-dammy-text)
  (should-not (ov-set (ov-line) 'face 'warning))
  (should (eq 1 (length (ov-all))))
  (ov-clear)
  (should-not (ov-set (ov-line) '(face warning)))
  (should (eq 1 (length (ov-all))))
  (ov-clear)
  (should-not (ov-set (ov-match "the") '(face warning)))
  (should (eq 8 (length (ov-all))))
  (ov-clear)
  (should-not (ov-set "^;;" '(face warning)))
  (should (eq 8 (length (ov-all))))
  (ov-clear))

(ert-deftest ov-test/ov-clear ()
  (ov-test-insert-dammy-text)
  (ov-set "the" 'face 'underline)
  (ov-clear)
  (should-not (ov-all))
  (ov-set "the" 'face 'underline)
  (ov-clear 'face 'underline)
  (should-not (ov-all))
  (ov-set "the" 'face 'underline)
  (ov-clear 'face 'success)
  (should (< 1 (length (ov-all))))
  (ov-set "the" 'face 'underline)
  (goto-char (point-min))
  (re-search-forward "the" nil t 3)
  (ov-clear (point) (point-max) 'face 'underline)
  (should (< 2 (length (ov-all)))))

(ert-deftest ov-test/ov-reset ()
  (ov-test-insert-dammy-text)
  (setq ov1 10)
  (setq ov2 10)
  (setq ov1 (ov-make 1 10))
  (ov-reset ov1)
  (should-not ov1)
  (setq ov2 (ov-match "the"))
  (ov-set ov2 'display "THE")
  (ov-reset ov2)
  (should-not ov2))

(ert-deftest ov-test/ov-spec ()
  (ov-test-insert-dammy-text)
  (setq ov1 (ov-line))
  (setq ov2 (ov-regexp "^;.+$"))
  (should (listp (ov-spec ov1)))
  (should (listp (ov-spec ov2)))
  (should (eq 4 (length (car (ov-spec ov1)))))
  (should (eq 4 (length (car (ov-spec ov2))))))

(ert-deftest ov-test/ov-at ()
  (ov-test-insert-dammy-text)
  (ov-set "the" 'face '(:overline t :foreground "#ff0000"))
  (re-search-forward "the" nil t 3)
  (should-not (ov? (ov-at))) ;; at the end of an overlay
  (backward-char 1)
  (should     (ov? (ov-at)))
  (re-search-backward "the" nil t)
  (should     (ov? (ov-at)))
  (backward-char 1)
  (should-not (ov? (ov-at))))

(ert-deftest ov-test/ov-timeout ()
  (ov-test-insert-dammy-text)
  (ov-timeout 0.3
    '(ov-set "the" 'face 'warning)
    '(ov-clear 'face 'warning))
  (should (< 1 (length (ov-all))))
  (sleep-for 0.5)
  (should-not (ov-all))
  (defun ov-1 () (ov-set "the" 'face 'warning))
  (defun ov-2 () (ov-clear 'face 'warning))
  (ov-timeout 0.3 ov-1 ov-2)
  (should (< 1 (length (ov-all))))
  (sleep-for 0.5)
  (should-not (ov-all)))

(ert-deftest ov-test/ov-next ()
  (ov-test-insert-dammy-text)
  (ov-set "the" 'face 'success 'aaa t)
  (ov-set ".$" 'face 'warning 'bbb t)
  (should (eq (save-excursion (re-search-forward "the" nil t))
              (save-excursion (forward-line 1) (ov-end (ov-next)))))
  (should (eq (point-at-eol)
              (ov-end (ov-next nil 'bbb))))
  (should (eq (point-at-eol)
              (ov-end (ov-next nil 'bbb t)))))

(ert-deftest ov-test/ov-prev ()
  (ov-test-insert-dammy-text)
  (ov-set "the" 'face 'success 'aaa t)
  (ov-set "^;" 'face 'warning 'bbb t)
  (goto-char (point-max))
  (should (eq (save-excursion (re-search-backward "the" nil t))
              (ov-beg (ov-prev))))
  (should (eq (point-at-bol)
              (ov-beg (ov-prev nil 'bbb))))
  (should (eq (point-at-bol)
              (ov-beg (ov-prev nil 'bbb t)))))

(provide 'ov-test)
;;; ov-test.el ends here
