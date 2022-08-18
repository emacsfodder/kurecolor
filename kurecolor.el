;;; kurecolor.el --- color editing goodies
;;
;;; Author: Jason Milkins <jasonm23@gmail.com>
;;
;;; Version: 1.4.4
;;
;;; Package-Requires: ((emacs "24.1") (s "1.12"))
;;
;;; Homepage: https://github.com/emacsfodder/kurecolor.el
;;
;;; Licence: MIT
;;
;;; Commentary:
;; # Kurecolor
;;
;; A collection of tools aimed at those working with color, useful for CSS,
;; Emacs themes, etc.
;;
;; [![](https://github.com/emacsfodder/kurecolor/actions/workflows/kurecolor-tests.yml/badge.svg)](https://github.com/emacsfodder/kurecolor/actions/workflows/kurecolor-tests.yml)
;; [![MELPA](https://stable.melpa.org/packages/kurecolor-badge.svg)](https://stable.melpa.org/#/kurecolor)
;; [![MELPA](https://melpa.org/packages/kurecolor-badge.svg)](https://melpa.org/#/kurecolor)
;;
;; Use rainbow-mode when using kurecolor commands, for instant feedback on color changes.
;;
;; ## Installing
;;
;; Kurecolor is on MELPA, you can install using `package.el'
;;
;;     M-x package-install kurecolor
;;
;; - - -
;; [[ function-list ]]
;;
;; [[ function-docs ]]
;;
;; ### Test/Examples
;;
;; The examples documented here are live tests (in `kurecolor-examples.el'). You can
;; run them using `bin/test' from the package folder.`
;;
;; ## Ephemera
;;
;; For those interested in such things, the name Kurecolor is
;; unashamedly nicked from a high end marker pen company.  Hopefully
;; this outrage will fall silently under their radar, and I won't have
;; to change it due to some frivilous and paranoid law
;; suit. (seriously guys, this is just free advertising.)
;;
;; I have not been pressured into saying this, however, Kurecolor
;; markers and art supplies are the very best!  Buy some (many!) NOW (Like REALLY
;; Immediately!!) for you, your mum and your pet chinchilla Frank.
;;
;; Since the question comes up occassionally, the mode-line hack used
;; in the presentation is based on original work by Armit Patel. I
;; gisted this a while back, you can get it from.
;; https://gist.github.com/jasonm23/8554119
;;
;;; Code:

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24.1 or later"))

(eval-when-compile
 (if (= 24 emacs-major-version)
     (progn
      (require 'cl)
      (require 'cl-lib t))
   (require 'cl-lib)))

(require 's)
(require 'dash)

(defgroup kurecolor nil
  "Kurecolor customizations."
  :group 'tools)

(defcustom kurecolor-color-group-format
  "%s, "
  "Used by color group functions."
  :type 'string
  :group 'kurecolor)

(defcustom  kurecolor-color-adjust-brightness-step
  5
  "Amount (%) to step adjust color brightness."
  :type 'integer
  :group 'kurecolor)

(defcustom kurecolor-color-adjust-saturation-step
  5
  "Amount (%) to step adjust color saturation."
  :type 'integer
  :group 'kurecolor)

(defcustom kurecolor-color-adjust-hue-step
  5
  "Amount (°) to step adjust color hue."
  :type 'integer
  :group 'kurecolor)

(defun kurecolor-hex-to-rgb (hex)
  "Convert a `rgb' HEX color to a list `(r g b)'.

The `r',`g',`b' values range between `0.0..1.0'."
  (setq hex (replace-regexp-in-string "#" "" hex))
  (mapcar #'(lambda (s) (/ (string-to-number s 16) 255.0))
          (list (substring hex 0 2)
                (substring hex 2 4)
                (substring hex 4 6))))

(defun kurecolor-hex-to-rgba (hex)
  "Convert a `rgba' HEX color to a list `(r g b a)'.

`rgba' HEX colors = `#RRGGBBAA' (i.e. CSS hex rgba)

e.g. `#FFFFFF00' white with no opacity.
     `#000000FF' black with no transparency.

The returned list `(rgba)' values will range between `0.0..1.0'."
  (if (= (length hex) 9)
   (let* ((hex (replace-regexp-in-string "#" "" hex))
          (rgba (mapcar #'(lambda (s)
                            (let ((n (string-to-number s 16)))
                             (/ n 255.0)))
                       (list (substring hex 0 2)
                             (substring hex 2 4)
                             (substring hex 4 6)
                             (substring hex 6 8)))))
     rgba)
   (user-error "Hex color %s does not contain an alpha value")))

(defun kurecolor-hex-to-hsv (hex)
  "Convert a `rgb' HEX color to `h' `s' `v'."
  (kurecolor-rgb-to-hsv (kurecolor-hex-to-rgb hex)))

(defun kurecolor-hsv-to-hex (h s v)
  "Convert H S V to a `rgb' hex color."
  (kurecolor-rgb-to-hex (kurecolor-hsv-to-rgb h s v)))

(defun kurecolor-rgb-to-hex (rgb)
  "RGB as a list `(r g b)' to `rgb' hex color.

The `r',`g',`b' values can range between `0.0..1.0'."
  (cl-destructuring-bind
      (red green blue)
      (mapcar 'kurecolor-to-8bit rgb)
    (format "#%02X%02X%02X" red green blue)))

(defun kurecolor-rgb-to-hsv (rgb)
  "Convert RGB, a list of `(r g b)' to list `(h s v)'.
The values can range between `0.0..1.0'."
  (cl-destructuring-bind
      (red green blue) rgb
    (let*
        ((val (max red green blue))
         (delta (- val (min red green blue)))
         (sat (if (cl-plusp val)
                  (/ delta val)
                0.0))
         (normalize #'(lambda
                        (constant right left)
                        (let ((hue (+ constant (/ (* 60.0 (- right left)) delta))))
                          (if (cl-minusp hue)
                              (+ hue 360.0)
                            hue)))))
      (list (/ (cond
                ((zerop sat) 0.0)
                ((= red val) (funcall normalize 0.0 green blue)) ; dominant red
                ((= green val) (funcall normalize 120.0 blue red)) ; dominant green
                (t (funcall normalize 240.0 red green)))
               360.0)
            sat
            val))))

(defun kurecolor-hsv-to-rgb (h s v)
  "Convert hsv (H S V) to red green blue.
Note: args H S V are expected to be a values from `0.0..1.0'"
  (let* ((i (floor (* h 6.0)))
         (f (- (* h 6.0) i))
         (p (* v (- 1.0 s)))
         (q (* v (- 1.0 (* f s))))
         (d (* v (- 1.0 (* (- 1.0 f) s))))
         (m (% i 6)))
    (cond
     ((= m 0) `(,v ,d ,p))
     ((= m 1) `(,q ,v ,p))
     ((= m 2) `(,p ,v ,d))
     ((= m 3) `(,p ,q ,v))
     ((= m 4) `(,d ,p ,v))
     ((= m 5) `(,v ,p ,q)))))

(defun kurecolor-replace-current (fn &rest args)
  "Get the current unspaced string at point.
Replace with the return value of the function FN with ARGS"
  (let (pos1 pos2 len replacement excerpt change)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (when (looking-at "#") (forward-char 1))
        (setq pos1 (car (bounds-of-thing-at-point 'symbol))
              pos2 (cdr (bounds-of-thing-at-point 'symbol)))
        (when (> pos1 0)
          (setq pos1 (- pos1 1)))))
    (setq excerpt (buffer-substring-no-properties pos1 pos2))
    (if args
        (progn (setq change (car args))
               (setq replacement (funcall fn excerpt change)))
      ;; no args
      (setq replacement (funcall fn excerpt)))
    (delete-region pos1 pos2)
    (insert replacement)))

(defun kurecolor--all-hex-colors-in-region-apply (func &rest args)
  "Use FUNC and ARGS to modify all hex colors found in region.
When region is not set, act on the whole buffer.

For example, to set the brightness on all colors in region to 50%.

```
(kurecolor--all-hex-colors-in-region-apply kurecolor-hex-set-brightness 0.5)
```"
  (let ((regexp "#[[:xdigit:]]\\{3,6\\}")
        (pos (point))
        (region-active (region-active-p))
        (begin (if (region-active-p)
                (region-beginning)
                0))
        (end (if (region-active-p)
                 (region-end)
                 (buffer-size))))
    (save-mark-and-excursion
       (when region-active (goto-char begin))
       (while (ignore-errors (re-search-forward regexp end))
         (let* ((a     (match-beginning 0))
                (b     (match-end 0))
                (color (match-string-no-properties 0))
                (re-colored (apply func `(,color ,@args))))
           (replace-string-in-region color re-colored a b)
           (goto-char b))))))

(defun kurecolor-adjust-brightness (hex amount)
  "Adjust the HEX color brightness by AMOUNT `-1.0..1.0'."
  (cl-destructuring-bind (hue sat val) (kurecolor-hex-to-hsv hex)
    (setq val (kurecolor-clamp (+ amount val) 0.0 1.0))
    (kurecolor-rgb-to-hex
     (kurecolor-hsv-to-rgb hue sat val))))

(defun kurecolor-adjust-saturation (hex amount)
  "Adjust the HEX color saturation by AMOUNT `-1.0..1.0'."
  (cl-destructuring-bind (hue sat val) (kurecolor-hex-to-hsv hex)
    (setq sat (kurecolor-clamp (+ sat amount) 0.0 1.0))
    (kurecolor-rgb-to-hex
     (kurecolor-hsv-to-rgb hue sat val))))

(defun kurecolor-adjust-hue (hex amount)
  "Adjust the HEX color hue by AMOUNT `0.0..1.0'."
  (cl-destructuring-bind (hue sat val) (kurecolor-hex-to-hsv hex)
    (setq hue (mod (+ hue amount) 1.0))
    (kurecolor-rgb-to-hex
     (kurecolor-hsv-to-rgb hue sat val))))

(defun kurecolor-hex-get-hue (hex)
  "Get the hue of HEX color."
  (car (kurecolor-hex-to-hsv hex)))

(defun kurecolor-hex-get-saturation (hex)
  "Get the saturation of HEX color."
  (cadr (kurecolor-hex-to-hsv hex)))

(defun kurecolor-hex-get-brightness (hex)
  "Get the brightness of HEX color."
  (cadr (cdr (kurecolor-hex-to-hsv hex))))

(defun kurecolor-hex-set-brightness (hex val)
  "Change a HEX color's brightness VAL, amount values from `0.0..1.0'.
returns a `rgb' hex color."
  (cl-destructuring-bind (hue sat skip) (kurecolor-hex-to-hsv hex)
    (kurecolor-rgb-to-hex (kurecolor-hsv-to-rgb hue sat val))))

(defun kurecolor-hex-set-hue (hex hue)
  "Change a HEX color's HUE, amount values from `0.0..1.0'.
returns a `rgb' hex color."
  (cl-destructuring-bind (skip sat val) (kurecolor-hex-to-hsv hex)
    (kurecolor-rgb-to-hex (kurecolor-hsv-to-rgb hue sat val))))

(defun kurecolor-hex-set-saturation (hex sat)
  "Change a HEX color's saturation SAT, amount values from `0.0..1.0'.
returns a `rgb' hex color."
  (cl-destructuring-bind (hue skip val) (kurecolor-hex-to-hsv hex)
    (kurecolor-rgb-to-hex (kurecolor-hsv-to-rgb hue sat val))))

(defun kurecolor-hex-set-brightness-from (source target)
  "Copy brightness from SOURCE to TARGET."
  (kurecolor-hex-set-brightness target (kurecolor-hex-get-brightness source)))

(defun kurecolor-hex-set-hue-from (source target)
  "Copy brightness from SOURCE to TARGET."
  (kurecolor-hex-set-hue target (kurecolor-hex-get-hue source)))

(defun kurecolor-hex-set-saturation-from (source target)
  "Copy the saturation of SOURCE to TARGET."
  (kurecolor-hex-set-saturation target (kurecolor-hex-get-saturation source)))

(defun kurecolor-interpolate (color1 color2)
  "Interpolate two hex colors COLOR1 and COLOR2, to get their mixed color."
  (let ((cons+ (lambda (c) (+ (car c) (cdr c)))))
    (cl-destructuring-bind (r g b)
       (mapcar #'(lambda (n) (* (/ n 2) 255.0))
                 (-zip-with '+ (kurecolor-hex-to-rgb color1)
                               (kurecolor-hex-to-rgb color2)))
      (format "#%02X%02X%02X" r g b))))

(defun kurecolor-clamp (num min max)
  "Clamp NUM to range of MIN MAX."
  (if (< num min)
      min
   (if (> num max)
       max
     num)))

(defun kurecolor-cssrgb-to-hex (cssrgb &optional hexrgba)
  "Convert a CSSRGB (`rgb()' or `rgba()') color to hex.

When HEXRGBA is non-nil the hex color string will be RGBA.
If css `alpha' value isn\\='t present, it will be set as `1.0'
i.e. no transparency

Valid css `rgb()' `rgba()' values are supported."
  (let ((rgb (cdr (s-match
                    (concat "rgba?(\s*"
                     "\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*"
                     "\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*"
                     "\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,?\\(.*?\\))")
                   cssrgb))))
    (cl-destructuring-bind (r g b a)
        (mapcar 'kurecolor-css-rgb-value-to-number rgb)
      (let ((a (if (string= "" (cadr (cddr rgb)))
                   1.0
                 a)))
        (if hexrgba
            (concat (format "#%02X%02X%02X" r g b)
                    (if a (format "%02X" (kurecolor-to-8bit a))) "")
          (format "#%02X%02X%02X" r g b))))))

(defun kurecolor-css-rgb-value-to-number (value)
  "Convert css `rgb()' or `rgba()': `r', `g', `b' VALUE.

From number `0-255' or percentage, to `0-255'."
  (if (s-ends-with? "%" value)
      (* (/ (string-to-number value) 100.0) 255)
    (string-to-number value)))

(defun kurecolor-to-8bit (n)
  "Convert N (`0.0..1.0') to `0-255'."
  (* n 255.0))

(defun kurecolor-hex-to-cssrgb (hex)
  "Convert a HEX rgb color to css rgb."
  (cl-destructuring-bind (r g b)
      (mapcar 'kurecolor-to-8bit (kurecolor-hex-to-rgb hex))
    (format "rgb(%i, %i, %i)" r g b)))

(defun kurecolor-hex-to-cssrgba (hex)
  "Convert a HEX rgba color to css rgba."
  (cl-destructuring-bind (r g b a)
      (kurecolor-hex-to-rgba hex)
    (format "rgba(%i, %i, %i, %.4f)"
            (kurecolor-to-8bit r)
            (kurecolor-to-8bit g)
            (kurecolor-to-8bit b)
            a)))

(defun kurecolor-xcode-color-literal-to-hex-rgba (color-literal)
  "Convert an XCode COLOR-LITERAL to a hex `rgba' string."
  (cl-destructuring-bind (red green blue alpha)
      (mapcar 'kurecolor-to-8bit
              (mapcar 'string-to-number
                      (cdr
                       (car
                        (s-match-strings-all
                         ;; - NOTE: Malformed colorLiterals will fail .
                                 "#colorLiteral(red: \\(.*\\), green: \\(.*\\), blue: \\(.*\\), alpha: \\(.*\\))"
                                 color-literal)))))
    (format "#%02X%02X%02X%02X" red green blue alpha)))

(defun kurecolor-hex-rgba-to-xcode-color-literal (rgba)
  "Convert a hex RGBA string to an XCode `colorLiteral'."
  (cl-destructuring-bind (r g b a)
      (kurecolor-hex-to-rgba rgba)
    (format
     "#colorLiteral(red: %.10f, green: %.10f, blue: %.10f, alpha: %.10f)"
     r g b a)))

(defun kurecolor-xcode-color-literal-to-hex-rgb (color-literal)
  "Convert an XCode COLOR-LITERAL to a hex `rgb' string."
  (substring
   (kurecolor-xcode-color-literal-to-hex-rgba color-literal) 0 7))

;;; Interactive functions

;;;###autoload
(defun kurecolor-hex-set-saturation-in-region (saturation)
  "Set the SATURATION of all hex colors found in region.
When region not active, act on the whole buffer."
  (interactive "nSet saturation (0.0..1.0): ")
  (kurecolor--all-hex-colors-in-region-apply 'kurecolor-hex-set-saturation saturation))

;;;###autoload
(defun kurecolor-hex-set-brightness-in-region (brightness)
  "Set the BRIGHTNESS of all hex colors found in region.
When region not active, act on the whole buffer."
  (interactive "nSet brightness (0.0..1.0): ")
  (kurecolor--all-hex-colors-in-region-apply 'kurecolor-hex-set-brightness brightness))

;;;###autoload
(defun kurecolor-hex-set-hue-in-region (hue)
  "Set the HUE of all hex colors found in region (BEGIN END).
When region not active, act on the whole buffer."
  (interactive "nSet hue for all colors (0°..360°): ")
  (let ((hue (/ hue 360.0)))
    (kurecolor--all-hex-colors-in-region-apply 'kurecolor-hex-set-hue hue)))

;;;###autoload
(defun kurecolor-hex-adjust-saturation-in-region (saturation)
  "Adjust the SATURATION on all hex colors found in region.
When region not active, act on the whole buffer."
  (interactive "nAdjust saturation (-1.0..1.0): ")
  (kurecolor--all-hex-colors-in-region-apply 'kurecolor-adjust-saturation saturation))

;;;###autoload
(defun kurecolor-hex-adjust-brightness-in-region (brightness)
  "Set the BRIGHTNESS of all hex colors found in region.
When region not active, act on the whole buffer."
  (interactive "nAdjust brightness (-1.0..1.0): ")
  (kurecolor--all-hex-colors-in-region-apply 'kurecolor-adjust-brightness brightness)

;;;###autoload
;;;(defun kurecolor-hex-adjust-in-region (hue)
  "Set the HUE of all hex colors found in region (BEGIN END).
When region not active, act on the whole buffer."
  (interactive "nAdjust hue for all colors (-360°..+360°): ")
  (kurecolor--all-hex-colors-in-region-apply 'kurecolor-adjust-hue hue))

;;;###autoload
(defun kurecolor-increase-brightness-by-step (x)
  "Increase brightness on hex color at point (or in region) by step.
Accepts universal argument X."
  (interactive "p")
  (kurecolor-replace-current
   'kurecolor-adjust-brightness
   (/ (* x kurecolor-color-adjust-brightness-step) 100.0)))

;;;###autoload
(defun kurecolor-decrease-brightness-by-step (x)
  "Decrease brightness on hex color at point (or in region) by step.
Accepts universal argument X."
  (interactive "p")
  (kurecolor-replace-current
   'kurecolor-adjust-brightness
   (/ (* (* -1 x) kurecolor-color-adjust-brightness-step) 100.0)))

;;;###autoload
(defun kurecolor-increase-saturation-by-step (x)
  "Increase saturation on hex color at point (or in region) by step.
Accepts universal argument X."
  (interactive "p")
  (kurecolor-replace-current
   'kurecolor-adjust-saturation
   (/ (* x kurecolor-color-adjust-saturation-step) 100.0)))

;;;###autoload
(defun kurecolor-decrease-saturation-by-step (x)
  "Decrease saturation on hex color at point (or in region) by step.
Accepts universal argument X."
  (interactive "p")
  (kurecolor-replace-current
   'kurecolor-adjust-saturation
   (/ (*  (* -1 x) kurecolor-color-adjust-saturation-step) 100.0)))

;;;###autoload
(defun kurecolor-increase-hue-by-step (x)
  "Increase hue on hex color at point (or in region) by step.
Accepts universal argument X."
  (interactive "p")
  (kurecolor-replace-current
   'kurecolor-adjust-hue
   (/ (* x kurecolor-color-adjust-hue-step) 360.0)))

;;;###autoload
(defun kurecolor-decrease-hue-by-step (x)
  "Decrease hue on hex color at point (or in region) by step.
Accepts universal argument X."
  (interactive "p")
  (kurecolor-replace-current
   'kurecolor-adjust-hue
   (/ (* (* -1 x) kurecolor-color-adjust-hue-step) 360.0)))

;;;###autoload
(defun kurecolor-set-brightness (color brightness)
  "Interactively change a COLOR's BRIGHTNESS."
  (interactive (list
                (if mark-active
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-from-minibuffer "Hex Color (#000000 - #FFFFFF): "))
                (/ (string-to-number (read-from-minibuffer "Set Brightness (0% - 100%): ")) 100.0)))
  (when mark-active (kill-region (region-beginning) (region-end)))
  (insert (kurecolor-hex-set-brightness color brightness)))

;;;###autoload
(defun kurecolor-set-saturation (color saturation)
  "Interactively change a COLOR's SATURATION."
  (interactive (list
                (if mark-active
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-from-minibuffer "Hex Color (#000000 - #FFFFFF): "))
                (/ (string-to-number (read-from-minibuffer "Set Saturation (0% - 100%): ")) 100.0)))
  (when mark-active (kill-region (region-beginning) (region-end)))
  (insert (kurecolor-hex-set-saturation color saturation)))

;;;###autoload
(defun kurecolor-set-hue (color hue)
  "Interactively change a COLOR's HUE."
  (interactive (list
                (if mark-active
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-from-minibuffer "Hex Color (#000000 - #FFFFFF): "))
                (/ (string-to-number (read-from-minibuffer "Set Hue (0° - 360°): ")) 360.0)))
  (when mark-active (kill-region (region-beginning) (region-end)))
  (insert (kurecolor-hex-set-hue color hue)))

;;;###autoload
(defun kurecolor-hex-hue-group (hex)
  "Given a HEX color.
Insert a list of hexcolors of different hue."
  (interactive "sHex color: ")
  (cl-loop for n from 9 downto 1 do
        (insert
         (format (or kurecolor-color-group-format "\n%s")
                 (kurecolor-hex-set-hue hex (* n 0.1))))))

;;;###autoload
(defun kurecolor-hex-sat-group (hex)
  "Given a HEX color.
Insert a list of hexcolors of different saturation (sat)."
  (interactive "sHex color: ")
  (cl-loop for n from 9 downto 1 do
        (insert
         (format (or kurecolor-color-group-format "\n%s")
                 (kurecolor-hex-set-saturation hex (* n 0.1))))))

;;;###autoload
(defun kurecolor-hex-val-group (hex)
  "Given a HEX color.
Insert a list of hexcolors of different brightness (val)."
  (interactive "sHex color: ")
  (cl-loop for n from 9 downto 1 do
        (insert
         (format (or kurecolor-color-group-format "\n%s")
                 (kurecolor-hex-set-brightness hex (* n 0.1))))))

;;;###autoload
(defun kurecolor-cssrgb-at-point-or-region-to-hex ()
  "CSS `rgb' color at point or region to hex `rgb'."
  (interactive)
  (kurecolor-replace-current 'kurecolor-cssrgb-to-hex))

;;;###autoload
(defun kurecolor-hexcolor-at-point-or-region-to-css-rgb ()
  "Hex `rgb' color at point or region to css `rgb' color."
  (interactive)
  (kurecolor-replace-current 'kurecolor-hex-to-cssrgb))

;;;###autoload
(defun kurecolor-hexcolor-at-point-or-region-to-css-rgba ()
  "Hex `rgb' color at point or region to css `rgba'.
Opacity is always set to `1.0'."
  (interactive)
  (kurecolor-replace-current 'kurecolor-hex-to-cssrgba))

;;;###autoload
(defun kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgba ()
  "XCode `colorLiteral' at point to hex `rgba'."
  (interactive)
  (kurecolor-replace-current 'kurecolor-xcode-color-literal-to-hex-rgba))

;;;###autoload
(defun kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgb ()
  "XCode `colorLiteral' at point to hex `rgb'."
  (interactive)
  (kurecolor-replace-current 'kurecolor-xcode-color-literal-to-hex-rgb))

;;;###autoload
(defun kurecolor-hex-rgb-at-point-or-region-to-xcode-color-literal ()
  "Hex `rgb' to XCode `colorLiteral'."
  (interactive)
  (kurecolor-replace-current 'kurecolor-hex-rgb-to-xcode-color-literal))

;;;###autoload
(defun kurecolor-hex-rgba-at-point-or-region-to-xcode-color-literal ()
  "Hex `rgba' to XCode `colorLiteral'."
  (interactive)
  (kurecolor-replace-current 'kurecolor-hex-rgba-to-xcode-color-literal))

(provide 'kurecolor)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; no-byte-compile: t
;; End:

;;; kurecolor.el ends here
