
# ov.el [![Build Status](https://travis-ci.org/ShingoFukuyama/ov.el.svg?branch=master)](https://travis-ci.org/ShingoFukuyama/ov.el)

Simple way to manipulate overlay for Emacs.

![ov.el](https://raw2.github.com/ShingoFukuyama/images/master/ov1.gif)

Overlay is capable of manipulating text appearance, cursor behavior, etc.  
It doesn't affect font-lock or text-properties.

## Command

You can always do `M-x ov-clear` to clear all overlays in the current buffer.

## Functions

### Make overlay / Set properties

* [ov](#ov-beg-end-rest-properties) `(beg end &rest properties)`
* [ov-make](#ov-make-beg-end-optional-buffer-front-nonsticky-rear-sticky) `(beg end)`
* [ov-line](#ov-line-optional-point) `(&optional point)`
* [ov-match](#ov-match-string-optional-beg-end) `(string &optional beg end)`
* [ov-regexp](#ov-regexp-regexp-optional-beg-end) `(regexp &optional beg end)`
* [ov-set](#ov-set-ov-or-ovs-or-regexp-rest-properties) `(ov-or-ovs-or-regexp &rest properties)`
* [ov-region](#ov-region)

### Clear overlay

* [ov-clear](#ov-clear-optional-prop-or-beg-val-or-end-beg-end) `(&optional beg end property value)`
* [ov-reset](#ov-reset-ov-or-ovs-variable) `(ov-or-ovs-variable)`

### Look up overlay parameters, etc

* [ov-p](#ov-p-ov) `(ov)`
* [ov-beg](#ov-beg-ov) `(ov)`
* [ov-end](#ov-end-ov) `(ov)`
* [ov-buf](#ov-buf-ov) `(ov)`
* [ov-val](#ov-val-ov-property) `(ov property)`
* [ov-prop](#ov-prop-ov) `(ov)`
* [ov-spec](#ov-spec-ov-or-ovs) `(ov-or-ovs)`

### Get an existing overlay or overlay list

* [ov-at](#ov-at-optional-point) `(&optional point)`
* [ov-in](#ov-in-prop-or-beg-val-or-end-beg-end) `(prop-or-beg val-or-end beg end)`
* [ov-all](#ov-all)
* [ov-backwards](#ov-backwards-optional-point) `(&optional point)`
* [ov-forwards](#ov-forwards-optional-point) `(&optional point)`

### Overlay manipulation

* [ov-move](#ov-move-ov-beg-end-optional-buffer) `(ov beg end &optional buffer)`
* [ov-next](#ov-next-optional-point-or-prop-prop-or-point-val) `(&optional point property value)`
* [ov-prev](#ov-prev-optional-point-or-prop-prop-or-point-val) `(&optional point property value)`
* [ov-goto-next](#ov-goto-next-optional-point-or-prop-prop-or-point-val) `(&optional point property value)`
* [ov-goto-prev](#ov-goto-prev-optional-point-or-prop-prop-or-point-val) `(&optional point property value)`

<!-- * [ov-read-only](#ov-read-only-ov-or-ovs) `(ov-or-ovs)` -->
<!-- * [ov-timeout](#ov-timeout-time-func-func-after) `(time func func-after)` -->

## Make overlay / Set properties

:link: [Overlay Properties](http://www.gnu.org/software/emacs/manual/html_node/elisp/Overlay-Properties.html)

#### ov `(beg end &rest properties)`

Make an overlay from `beg` and `end`, then set `properties` if it's specified.

Return: `overlay`

```cl
(ov 10 40 'face 'font-lock-warning-face 'intangible t)
(ov 30 80 '(face font-lock-warning-face intangible t))
(ov (point-min) (point) 'face '(:background "#00ff00" :height 1.5))
(ov (point-min) (point-max) '(face (:background "#00ff00" :height 1.5)))
;; Just make an overlay without setting properties
(setq ov1 (ov 5 15))    ; => #<overlay from 5 to 15 in *scratch*>
```

You can always do `M-x ov-clear` to clear all overlays in the current buffer.

#### ov-make `(beg end &optional buffer front-advance rear-advance)`

Just make an overlay from `beg` and `end`.

Return: `overlay`  
Alias: `ov-create`

```cl
(setq ov1 (ov-make 10 55))           ; => #<overlay from 10 to 55 in *scratch*>
(setq ov2 (ov-make (point-min) 25))  ; => #<overlay from 1 to 25 in *scratch*>
;; Third argument is buffer object. If it's ommited or nil, it will be current buffer object.
(setq ov3 (ov-make 10 20 (get-buffer "README.md"))) ; => #<overlay from 10 to 20 in README.md>
;; If fourth argument is t, overlay won't be inherited by inserting (like front-nonsticky in text-property).
;; If fifth  argument is t, overlay will  be inherited by inserting (like rear-sticky in text-property).
(setq ov4 (ov-make 15 30 nil t nil)) ; => #<overlay from 15 to 30 in *scratch*>
```

#### ov-line `(&optional point)`

Make an overlay from the beginning of the line to the beginning of the next line, which include `point`.

Return: `overlay`

```cl
(setq ov1 (ov-line))  ; => #<overlay from 734 to 827 in *scratch*>
```

#### ov-match `(string &optional beg end)`

Make overlays that match the `string`. `beg` and `end` are specify the area.

Return: `overlay list`

```cl
(setq ov1 (ov-match "setq"))
(setq ov2 (ov-match "setq" 1 200))
```

#### ov-regexp `(regexp &optional beg end)`

Make overlays that match the `regexp`. `beg` and `end` are specify the area.

Return: `overlay list`

```cl
(setq ov1 (ov-regexp "setq"))
(setq ov2 (ov-regexp "setq" 100 550))
```

#### ov-set `(ov-or-ovs-or-regexp &rest properties)`

Set properties and values in an overlay or overlays alternately.  
If `ov-or-ovs-or-regexp` is string, it use `ov-regexp` to make overlays.  
If you want to use literal string, use `(ov-match "string")` instead.

Return: `nil`  
Alias: `ov-put`

```cl
(setq ov1 (ov-make 335 700))
(ov-set ov1 'face 'font-lock-warning-face 'intangible t)

(setq ov2 (ov-match "set"))
(ov-set ov2 '(face font-lock-function-name-face intangible t))

(ov-set (ov-regexp "^.ov-") 'display "λλ-" 'before-string "(" 'line-prefix "<λ>")
(ov-set "^;;.+$" 'face 'warning)

(ov-set (ov-line) 'before-string (propertize ">>> " 'face 'font-lock-warning-face))
(ov-set (ov-line) `(before-string ,(propertize ">>> " 'face 'font-lock-warning-face)))
```

#### ov-region

Make an overlay from a region.  
When you make a region, do `M-: (ov-set (ov-region) 'face '(:box t))`.

Return: `overlay`

```cl
(ov-set (ov-region) 'face '(:box t))
```

## Clear overlay

#### ov-clear `(&optional prop-or-beg val-or-end beg end)`

Clear `beg` and `end` of overlays whose `property` has `value`.

Arguments pattern:

```cl
(ov-clear PROPERTY VALUE BEG END)
(ov-clear PROPERTY VALUE)
(ov-clear BEG END)
(ov-clear PROPERTY)
(ov-clear)
```

```cl
(ov-clear 100 550 'my-fancy-comment t)
(ov-clear 'face 'font-lock-function-name-face)
(ov-clear 200 1000)
(ov-clear 'face)
(ov-clear) ;; clear overlays in the whole buffer
```

#### ov-reset `(ov-or-ovs-variable)`

Clear overlays in `ov-or-ovs-variable`. The variable is going to be nil.

```cl
(setq ov1 (ov-line))
(ov-set ov1  'invisible t)
(ov-reset ov1)

(setq ov2 (ov-match "ov-"))
(ov-set ov2 '(face (:underline t)))
(ov-reset ov2)
```


## Look up overlay parameters, etc

#### ov-p `(ov)`

Check whether `ov` is overlay or not.

Alias: `ov?`

```cl
(setq ov1 99)
(setq ov2 (ov-make 10 20))
(setq ov3 ov2)
(ov-p ov1) ; => nil
(ov-p ov2) ; => t
(ov-p ov3) ; => t
```

#### ov-beg `(ov)`

Get the beginning of an overlay.

Return: `point`

```cl
(setq ov1 (ov-make 200 700))
(ov-beg ov1) ; => 200
```

#### ov-end `(ov)`

Get the end of an overlay.

Return: `point`

```cl
(setq ov1 (ov-make 200 700))
(ov-end ov1) ; => 700
```

#### ov-buf `(ov)`

Get the buffer object of an overlay.

Return: `buffer object`

```cl
(setq ov1 (ov-make 200 700))
(ov-buf ov1)               ; => #<buffer *scratch*>
(buffer-name (ov-buf ov1)) ; => "*scratch*"
```

#### ov-val `(ov property)`

Get the value of `property` from an overlay.

Return: `value` of property

```cl
(setq ov1 (ov-line))
(ov-set ov1 'aaa "abc" 'bbb 22 'ccc 45)
(ov-val ov1 'bbb)           ; => 22
(ov-val ov1 'aaa)           ; => "abc"
```

#### ov-prop `(ov)`

Get the properties from an overlay.

Return: `properties list`

```cl
(setq ov1 (ov-make (point-min) (point-max)))
(ov-set ov1 '(face (:overline t) mouse-face (:underline t)))
(ov-prop ov1) ; => (mouse-face (:underline t) face (:overline t))
```

#### ov-spec `(ov-or-ovs)`

Make a specification list from an overlay or overlay list.

Return: `list ((beginning end buffer (properties)) (beginning end buffer (properties))...)` or `nil`

```cl
(setq ov1 (ov-match "ov-spec"))
(ov-set ov1 'evaporate t)
(ov-spec ov1)
;; => ((5802 5809 README.md (evaporate t)) (5782 5789 README.md (evaporate t)) ...)

(setq ov2 (ov-line))
(ov-spec ov2) ; => ((5879 5921 README.md nil))
```

## Get an existing overlay or overlay list

#### ov-at `(&optional point)`

Get an overlay from `point` or when the cursor is at an existing overlay.

Return: `overlay` or `nil`

```cl
(save-excursion
  (ov-set (ov-make 20 50) 'face '(:box t))
  (goto-char 30)
  (ov-at))        ; => #<overlay from 20 to 50 in *scratch*>
```

#### ov-in `(prop-or-val val-or-end beg end)`

Get overlays within from `beg` to `end`.

Return: `overlay list`

Arguments pattern:

```
(ov-in PROPERTY VALUE BEG END)
(ov-in PROPERTY VALUE)
(ov-in PROPERTY)
(ov-in BEG END)
(ov-in)
```

```cl
;; Get all overlays
(setq ov1 (ov-in))
;; Get overlays between 10 and 500
(setq ov2 (ov-in 10 500))
;; Get the overlays which has a specific property and its value
(setq ov3 (ov-in 'face 'warning))
;; Get the overlays which has a specific property
(setq ov4 (ov-in 'face))
;; Get the overlays between 10 and 500, which has a specific property and its value
(setq ov5 (ov-in 'face 'worning 10 500))
;; If 'any specified to val-or-end, it matches any value
(setq ov6 (ov-in 'face 'any 10 500))
```

#### ov-all

Get overlays in the whole buffer.

Return: `overlay list` or `nil`

```cl
(setq ov1 (ov-all))
```

#### ov-backwards `(&optional point)`

Get overlays within from the beginning of the buffer to `point`.

Return: `overlay list` or `nil`

```cl
(setq ov1 (ov-backwards))
(setq ov2 (ov-backwards 1200))
```

#### ov-forwards `(&optional point)`

Get overlays within from `point` to the end of the buffer.

Return: `overlay list` or `nil`

```cl
(setq ov1 (ov-forwards))
(setq ov2 (ov-forwards 1000))
```

## Overlay manipulation

#### ov-move `(ov beg end &optional buffer)`

Move an existing overlay position to another position.

Return: `overlay`

```cl
(setq ov1 (ov-line))
(ov-set ov1 'face 'underline)
(ov-move ov1 (point-at-bol) (point-at-eol))
;; Move overlay ov1 to another buffer
(progn
  (with-current-buffer (get-buffer-create "test")
    (insert "aaaaaaaaaaaaaaa"))
  (ov-move ov1 (point-min) (point-max) (get-buffer "test"))
  (pop-to-buffer "test"))
```

<!-- #### ov-timeout `(time func func-after)` -->

<!-- Execute `func-after` after `time` seconds passed since `func` done. -->

<!-- ```cl -->
<!-- (ov-timeout 0.5 -->
<!--   '(ov-set "ov" '(face (:background "#ff0000") aaa t)) -->
<!--   '(ov-clear 'aaa t)) -->

<!-- (defun ov-fn1 () -->
<!--   (ov-set (ov-match "ov") '(face (:background "#ff9900") bbb t))) -->
<!-- (defun ov-fn2 () -->
<!--   (ov-clear 'bbb t)) -->
<!-- (ov-timeout 1.2 ov-fn1 ov-fn2) -->
<!-- ``` -->

#### ov-next `(&optional point-or-prop prop-or-point val)`

Get the next existing overlay from `point-or-prop`. You can also specify `prop-or-point` and its `val`.

Return: `overlay`

Arguments pattern:

```
(ov-next POINT PROPERTY VALUE)
(ov-next PROPERTY VALUE)
(ov-next PROPERTY)
(ov-next)
```

```cl
(ov-set "^.." '(face (:background "#ff9900") aaa t))
(ov-set "..$" '(face (:background "#7700ff") bbb t))
(ov-next)         ; => #<overlay from 436 to 438 in *scratch*>
(goto-char (ov-beg (ov-next nil 'aaa)))
(goto-char (ov-end (ov-next nil 'bbb t)))

(ov-next 'aaa)
(ov-next 'aaa t)
(ov-next 300 'aaa)
(ov-next (point) 'aaa t)
```

#### ov-prev `(&optional point-or-prop prop-or-point val)`

Get the previous existing overlay from `point`. You can also specify `property` and its `value`.

Return: `overlay`

Arguments pattern:

```
(ov-prev POINT PROPERTY VALUE)
(ov-prev PROPERTY VALUE)
(ov-prev PROPERTY)
(ov-prev)
```

```cl
(ov-set (ov-match "o") '(face (:box t) my-char o))
(ov-set (ov-match "t") '(face (:box t) my-char t))
(ov-prev)         ; => #<overlay from 482 to 483 in *scratch*>
(goto-char (ov-beg (ov-prev (point) 'face)))
(goto-char (ov-end (ov-prev nil 'my-char 'o)))

(ov-prev 'my-char)
(ov-prev 'my-char 'o)
(ov-prev 300 'my-char)
(ov-prev (point) 'my-char 'o)
```

#### ov-goto-next `(&optional point-or-prop prop-or-point val)`

Move cursor to the next overlay position. You can specify arguments the same as `ov-next` above.

```cl
(ov-goto-next)
(ov-goto-next 'face)
(ov-goto-next 'face 'warning)
(ov-goto-next 300 'face 'warning)
```

#### ov-goto-prev `(&optional point-or-prop prop-or-point val)`

Move cursor to the previous overlay position. You can specify arguments the same as `ov-prev` above.

```cl
(ov-goto-prev)
(ov-goto-prev 'face)
(ov-goto-prev 'face 'warning)
(ov-goto-prev 300 'face 'warning)
```

<!-- #### ov-read-only `(ov-or-ovs)` -->

<!-- It implements a read-only like feature for overlay. It's not as good as that of the text property. -->

<!-- ```cl -->
<!-- (setq ov1 (ov-match "setq")) -->
<!-- (ov-set ov1 'face 'success) -->
<!-- (ov-read-only ov1) -->
<!-- ;; You will be able to prevent some commands from modifying "setq" strings. -->
<!-- ``` -->

## Useful examples

#### Add keybinds specific to overlay area

Assign keybind that works only where the cursor is on the overlays.

```
(defvar ov1-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-d") 'ov-clear)
    (define-key map (kbd "M-b")
      (lambda () (interactive) (ov-set (ov-at) 'face '(:box t))))
    (define-key map (kbd "M-n")
      (lambda () (interactive) (if (ov-goto-next 'ov1) (backward-char 1))))
    (define-key map (kbd "M-p")
      (lambda () (interactive) (ov-goto-prev 'ov1)))
    map))
(ov-set "key" 'face 'warning 'keymap ov1-map 'ov1 t)
```

#### Evaporative overlay

When you modify one of the overlaid text, all their overlays will be evaporated.

```cl
(defun my-ov-evaporate-ov1 (_ov _after _beg _end &optional _len)
  (ov-clear 'ov1))
(ov-set "ov-" 'face 'warning
              'ov1 t
              'modification-hooks '(my-ov-evaporate-ov1))
```


## Reference

* :link: [Overlay Properties](http://www.gnu.org/software/emacs/manual/html_node/elisp/Overlay-Properties.html)
* :link: [Face Attributes](http://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html)
* :link: [Managing Overlays](http://www.gnu.org/software/emacs/manual/html_node/elisp/Managing-Overlays.html)
* :link: [comint read-only prompt](http://lists.gnu.org/archive/html/emacs-devel/2002-08/msg00428.html)
