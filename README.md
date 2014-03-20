# ov.el

Simple way to manipulate overlay for Emacs.

## Functions

### Make overlay / Set properties

* [ov](#ov-beg-end-rest-properties) `(beg end &rest properties)`
* [ov-make](#ov-make-beg-end) `(beg end)`
* [ov-line](#ov-line-optional) `(&optional point)`
* [ov-match](#ov-match-string-optional-beg-end) `(string &optional beg end)`
* [ov-regexp](#ov-regexp-regexp-optional-beg-end) `(regexp &optional beg end)`
* [ov-set](#ov-set-ov-or-ovs-rest-properties) `(ov-or-ovs &rest properties)`
* [ov-region](#ov-region)

### Clear overlay

* [ov-clear](#ov-clear-optional-beg-end-property-value) `(&optional beg end property value)`
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
* [ov-in](#ov-in-beg-end) `(beg end)`
* [ov-all](#ov-all)
* [ov-backwards](#ov-backwards-optional-point) `(&optional point)`
* [ov-forwards](#ov-forwards-optional-point) `(&optional point)`

### Overlay manipulation

* [ov-move](#ov-move-ov-beg-end-optional-buffer) `(ov beg end &optional buffer)`
* [ov-timeout](#ov-timeout-time-func-func-after) `(time func func-after)`
* [ov-next](#ov-next-optional-property-value) `(&optional point property value)`
* [ov-prev](#ov-prev-optional-property-value) `(&optional point property value)`
* [ov-read-only](#ov-read-only) `(ov-or-ovs)`


## Make overlay / Set properties

#### ov `(beg end &rest properties)`

Make an overlay from `beg` and `end`, then set `properties`.

Return: overlay

```cl
(ov 10 40 'face 'font-lock-warning-face 'intangible t)
(ov 30 80 '(face font-lock-warning-face intangible t))
(ov (point-min) (point) 'face '(:background "#00ff00" :height 1.5))
(ov (point-min) (point-max) '(face (:background "#00ff00" :height 1.5)))
```

#### ov-make `(beg end)`

Just make an overlay from `beg` and `end`.

Return: overlay  
Alias: `ov-create`

```cl
(setq ov1 (ov-make 10 55))           ; => #<overlay from 10 to 55 in *scratch*>
(setq ov2 (ov-make (point-min) 25))  ; => #<overlay from 1 to 25 in *scratch*>
```

#### ov-line `(&optional point)`

Make an overlay from the beginning of the line to the beginning of the next line, which include `point`.

Return: overlay

```cl
(setq ov1 (ov-line))  ; => #<overlay from 734 to 827 in *scratch*>
```

#### ov-match `(string &optional beg end)`

Make overlays that match the `string`. `beg` and `end` are specify the area.

Return: overlay list

```cl
(setq ov1 (ov-match "setq"))
(setq ov2 (ov-match "setq" 1 200))
```

#### ov-regexp `(regexp &optional beg end)`

Make overlays that match the `regexp`. `beg` and `end` are specify the area.

Return: overlay list

```cl
(setq ov1 (ov-regexp "setq"))
(setq ov2 (ov-regexp "setq" 100 550))
```

#### ov-set `(ov-or-ovs &rest properties)`

Set properties and values in an overlay or overlays alternately.

Alias: `ov-put`

```cl
(setq ov1 (ov-make 335 700))
(ov-set ov1 'face 'font-lock-warning-face 'intangible t)

(setq ov2 (ov-match "set"))
(ov-set ov2 '(face font-lock-function-name-face intangible t))

(ov-set (ov-regexp "^.ov-") 'display "λλ-" 'before-string "(" 'line-prefix "<λ>")

(ov-set (ov-line) 'before-string (propertize ">>> " 'face 'font-lock-warning-face))
(ov-set (ov-line) `(before-string ,(propertize ">>> " 'face 'font-lock-warning-face)))
```

#### ov-region

Make an overlay from a region.  
When you make a region, do `M-: (ov-set (ov-region) 'face '(:box t))`.

Return: overlay

```cl
(ov-set (ov-region) 'face '(:box t))
```

## Clear overlay

#### ov-clear `(&optional beg end property value)`

Clear `beg` and `end` of overlays whose `property` has `value`.  


```cl
(ov-clear 100 550 'my-fancy-comment t)
(ov-clear 'face 'font-lock-function-name-face)
(ov-clear 200 1000)
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

Return: point

```cl
(setq ov1 (ov-make 200 700))
(ov-beg ov1) ; => 200
```

#### ov-end `(ov)`

Get the end of an overlay.

Return: point

```cl
(setq ov1 (ov-make 200 700))
(ov-end ov1) ; => 700
```

#### ov-buf `(ov)`

Get the buffer object of an overlay.

Return: buffer object

```cl
(setq ov1 (ov-make 200 700))
(ov-buf ov1)               ; => #<buffer *scratch*>
(buffer-name (ov-buf ov1)) ; => "*scratch*"
```

#### ov-val `(ov property)`

Get the value of `property` from an overlay.

Return: value

```cl
(setq ov1 (ov-line))
(ov-set ov1 'aaa "abc" 'bbb 22 'ccc 45)
(ov-val ov1 'bbb)           ; => 22
(ov-val ov1 'aaa)           ; => "abc"
```

#### ov-prop `(ov)`

Get the properties from an overlay.

Return: properties list

```cl
(setq ov1 (ov-make (point-min) (point-max)))
(ov-set ov1 '(face (:overline t) mouse-face (:underline t)))
(ov-prop ov1) ; => (mouse-face (:underline t) face (:overline t))
```

#### ov-spec `(ov-or-ovs)`

Make a specification list from an overlay or overlay list.

Return: list ((beginning end buffer properties) (beginning end buffer properties)...) or nil

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

Return: overlay or nil

```cl
(save-excursion
  (ov-set (ov-make 20 50) 'face '(:box t))
  (goto-char 30)
  (ov-at))        ; => #<overlay from 20 to 50 in *scratch*>
```

#### ov-in `(beg end)`

Get overlays within from `beg` to `end`.

Return: overlay list

#### ov-all

Get overlays in the whole buffer.

Return overlay list or nil

```cl
(setq ov1 (ov-all))
```

#### ov-backwards `(&optional point)`

Get overlays within from the beginning of the buffer to `point`.

Return: overlay list or nil

```cl
(setq ov1 (ov-backwards))
(setq ov2 (ov-backwards 1200))
```

#### ov-forwards `(&optional point)`

Get overlays within from the beginning of the buffer to `point`.

Return: overlay list or nil

```cl
(setq ov1 (ov-forwards))
(setq ov2 (ov-forwards 1000))
```

## Overlay manipulation

#### ov-move `(ov beg end &optional buffer)`

Move an existing overlay position to other position.

Return: overlay

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

#### ov-timeout `(time func func-after)`

Execute `func-after` after `time` seconds passed since `func` done.

```cl
(ov-timeout 0.5
  '(ov-set (ov-regexp "ov") '(face (:background "#ff0000") aaa t))
  '(ov-clear 'aaa t))

(defun ov-fn1 ()
  (ov-set (ov-match "ov") '(face (:background "#ff9900") bbb t)))
(defun ov-fn2 ()
  (ov-clear 'bbb t))
(ov-timeout 1.2 ov-fn1 ov-fn2)
```

#### ov-next `(&optional point property value)`

Get the next existing overlay from `point`. You can also specify `property` and its `value`.

```cl
(ov-set (ov-regexp "^..") '(face (:background "#ff9900") aaa t))
(ov-set (ov-regexp "..$") '(face (:background "#7700ff") bbb t))
(ov-next)         ; => #<overlay from 436 to 438 in *scratch*>
(goto-char (ov-beg (ov-next nil 'aaa)))
(goto-char (ov-end (ov-next nil 'bbb t)))
```

#### ov-prev `(&optional point property value)`

Get the previous existing overlay from `point`. You can also specify `property` and its `value`.

```cl
(ov-set (ov-match "o") '(face (:box t) my-char o))
(ov-set (ov-match "t") '(face (:box t) my-char t))
(ov-prev)         ; => #<overlay from 482 to 483 in *scratch*>
(goto-char (ov-beg (ov-prev (point) 'face)))
(goto-char (ov-end (ov-prev nil 'my-char 'o)))
```

#### ov-read-only `(ov-or-ovs)`

It implements a read-only like feature for overlay. It's not as good as that of the text property.

```cl
(setq ov1 (ov-match "setq"))
(ov-set ov1 'face 'success)
(ov-read-only ov1)
;; You will be able to prevent "setq"s from some deleting commands.
```
