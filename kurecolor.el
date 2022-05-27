;;; kurecolor.el --- color editing goodies

;;; Author: Jason Milkins <jasonm23@gmail.com>

;;; Version: 1.2.9

;;; Commentary:
;;
;; A collection of color tools aimed at those working with (normal 6
;; digit) hex color codes, useful for CSS, Emacs themes, etc.
;;
;; View the presentation at https://github.com/emacsfodder/kurecolor
;;
;; ![](kurecolor.gif)
;;
;; Features include interactive step modification of hue, sat, val on
;; hex colors.  Color conversion algorithms, for 6 digit hex colors,
;; hsv, rgb, cssrgb.  Get/set h s v values from/for a color.
;;
;; It's recommend you use this in conjunction with rainbow-mode, for
;; instant feedback on color changes.
;;
;; ### Doing cool things
;;
;; You can do funky things like adjust the saturation of all the
;; colors in a selection, using macros.
;;
;; The presentation touches on this, and you'll need to get your
;; keyboard macro skills out to really shine, but it's relatively
;; simple.
;;
;; ### Step by step: Mass saturation decrease
;; As per the animated presentation, these steps will demonstrate how to
;; use Kurecolor interactive hsv adjusters with keyboard macros, to mass
;; adjust colors.

;; Add a few hex colors to your active buffer, or load up a css, code or
;; simple text buffer, which already has some colors you'd like to tweak.
;; Activate rainbow-mode.  (If you need to install rainbow-mode, grab it
;; from ELPA.)

;; After you have some colors ready to tweak, we're ready to try the
;; following steps:
;;
;; 1. Select the region you want to modify
;; 2. Narrow the buffer `M-x narrow-to-region`
;; 3. Go to the top `M-<` of the narrowed section
;; 4. Start recording a macro `f3`
;;   1. Regexp i-search `C-M-s` for `#[0-9A-Fa-f]\{6\}` and `ENTER` on the first match
;;   2. Your cursor point will be at the end of the first color (unless the Regexp was in-adequate :( )
;;   3. `M-5` (to do 5 steps) `M-x kurecolor-decrease-saturation-by-step` (shortcut `M-x kure-d-sat` **TAB**)
;; 5. Stop recording the macro `f4`
;; 6. Run the macro again with `f4`, repeat until you are finished, or use `M-0 f4` to run the macro until it hits the end of the narrowed region (or hits an error).
;; 8. When you're done, `M-x widen` to exit narrowing.
;;
;; ## Installing
;;
;; Installing kurecolor is recommended to be done via MELPA.
;;
;;     M-x package-install kurecolor
;;
;; If you wish to install it manually, you already have your big boy
;; pants on and need no further help from me.
;;
;; Enjoy!
;;
;; ### Tests
;;
;; This package has a suite of unit tests.  To run them load both
;; kurecolor and kurecolor-test, and then do `M-x ert` (accept
;; `default`).
;;
;; ### Ephemera
;;
;; For those interested in such things, the name Kurecolor is
;; unashamedly nicked from a high end marker pen company.  Hopefully
;; this outrage will fall silently under their radar, and I won't have
;; to change it due to some frivilous and paranoid law
;; suit. (seriously guys, this is just free advertising.)
;;
;; I have not been pressured into saying this, however, Kurecolor
;; markers and art supplies are best best!  Buy some NOW (Like REALLY
;; Immediately!!) for you, your mum and your pet chinchilla Frank.
;;
;; Since the question comes up occassionally, the mode-line hack used
;; in the presentation is based on original work by Armit Patel. I
;; gisted this a while back, you can get it from.
;; https://gist.github.com/jasonm23/8554119
;;
;; The theme is Gruvbox, although you can't see much of it. Anyway,
;; it's a great theme, you should go install it now. (from MELPA)

;;; Package-Requires: ((emacs "24.1") (s "1.0"))

;;; Licence: MIT


;;; Code:

(require 's)
(eval-when-compile
  (require 'cl-macs))

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24.1 or later"))

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
  "Convert a 6 digit HEX color to r g b."
  (setq hex (replace-regexp-in-string "#" "" hex))
  (mapcar #'(lambda (s) (/ (string-to-number s 16) 255.0))
          (list (substring hex 0 2)
                (substring hex 2 4)
                (substring hex 4 6))))

(defun kurecolor-hex-to-hsv (hex)
  "Convert a 6 digit HEX color to h s v."
  (kurecolor-rgb-to-hsv (kurecolor-hex-to-rgb hex)))

(defun kurecolor-hsv-to-hex (h s v)
  "Convert H S V to a 6 digit HEX color."
  (kurecolor-rgb-to-hex (kurecolor-hsv-to-rgb h s v)))

(defun kurecolor-rgb-to-hex (rgb)
  "Replacement simple RGB to hex."
  (cl-destructuring-bind
      (red green blue)
      (mapcar 'kurecolor-to-8bit   rgb)
    (format "#%02X%02X%02X" red green blue)))

(defun kurecolor-rgb-to-hsv (rgb)
  "Convert RGB, a list of (r g b) to list (h s v).
For this module, h is returned as [0-1] instead of [0-360]."
  (cl-destructuring-bind
      (red green blue) rgb
    (let*
        ((val (max red green blue))
         (delta (- val (min red green blue)))
         (sat (if (cl-plusp val)
                  (/ delta val)
                0))
         (normalize #'(lambda
                        (constant right left)
                        (let ((hue (+ constant (/ (* 60 (- right left)) delta))))
                          (if (cl-minusp hue)
                              (+ hue 360)
                            hue)))))
      (list (/ (cond
                ((zerop sat) 0)
                ((= red val) (funcall normalize 0 green blue)) ; dominant red
                ((= green val) (funcall normalize 120 blue red)) ; dominant green
                (t (funcall normalize 240 red green)))
               360.0)
            sat
            val))))

(defun kurecolor-hsv-to-rgb (h s v)
  "Convert hsv (H S V) to red green blue.
Note: args H S V are expected to be a values from 0..1"
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

(defun kurecolor-adjust-brightness (hex amount)
  "Adjust the HEX color brightness by AMOUNT 0.0-0.1."
  (cl-destructuring-bind (hue sat val) (kurecolor-hex-to-hsv hex)
    (setq val (min 1.0 (+ amount val)))
    (kurecolor-rgb-to-hex
     (kurecolor-hsv-to-rgb hue sat val))))

(defun kurecolor-adjust-saturation (hex amount)
  "Adjust the HEX color saturation by AMOUNT 0.0-0.1."
  (cl-destructuring-bind (hue sat val) (kurecolor-hex-to-hsv hex)
    (setq sat (min 1.0 (+ sat amount)))
    (kurecolor-rgb-to-hex
     (kurecolor-hsv-to-rgb hue sat val))))

(defun kurecolor-adjust-hue (hex amount)
  "Adjust the HEX color hue by AMOUNT 0.0-0.1."
  (cl-destructuring-bind (hue sat val) (kurecolor-hex-to-hsv hex)
    (setq hue (mod (+ hue amount) 1.0))
    (kurecolor-rgb-to-hex
     (kurecolor-hsv-to-rgb hue sat val))))

(defun kurecolor-hex-set-hue (hex hue)
  "Change a HEX color's HUE, amount values from 0-1.
returns a 6 digit hex color."
  (cl-destructuring-bind (skip sat val) (kurecolor-hex-to-hsv hex)
    (kurecolor-rgb-to-hex (kurecolor-hsv-to-rgb hue sat val))))

(defun kurecolor-hex-get-hue (hex)
  "Get the hue of HEX color."
  (car (kurecolor-hex-to-hsv hex)))

(defun kurecolor-hex-get-saturation (hex)
  "Get the saturation of HEX color."
  (cadr (kurecolor-hex-to-hsv hex)))

(defun kurecolor-hex-get-brightness (hex)
  "Get the brightness of HEX color."
  (caddr (kurecolor-hex-to-hsv hex)))

(defun kurecolor-hex-set-brightness (hex val)
  "Change a HEX color's brightness VAL, amount values from 0.0-1.0.
returns a 6 digit hex color."
  (cl-destructuring-bind (hue sat skip) (kurecolor-hex-to-hsv hex)
    (kurecolor-rgb-to-hex (kurecolor-hsv-to-rgb hue sat val))))

(defun kurecolor-hex-set-saturation (hex sat)
  "Change a HEX color's saturation SAT, amount values from 0-1.
returns a 6 digit hex color."
  (cl-destructuring-bind (hue skip val) (kurecolor-hex-to-hsv hex)
    (kurecolor-rgb-to-hex (kurecolor-hsv-to-rgb hue sat val))))

(defun kurecolor-interpolate (color1 color2)
  "Interpolate two hex colors COLOR1 and COLOR2, to get their mixed color."
  (cl-destructuring-bind (r g b)
      (mapcar #'(lambda (n) (* (/ n 2) 255.0))
              (cl-mapcar '+ (kurecolor-hex-to-rgb color1)
                         (kurecolor-hex-to-rgb color2)))
    (format "#%02X%02X%02X" r g b)))

(defun kurecolor-cssrgb-to-hex (cssrgb)
  "Convert a CSSRGB (or rgba) color to hex (alpha value is ignored)."
  (let ((rgb (cdr
              (s-match
               (concat "rgba?(\s*"
                       "\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*"
                       "\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*"
                       "\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,?[^)]*)")
               ;; For now we discard the alpha but we need to be aware
               ;; of its presence for the Rx to match rgba() colors.
               cssrgb))))
    (cl-destructuring-bind (r g b) (mapcar 'string-to-number rgb)
      (format "#%02X%02X%02X" r g b))))

(defun kurecolor-to-8bit (n)
  "Convert N (0.0-1.0) to 0-255."
  (* n 255.0))

(defun kurecolor-hex-to-cssrgb (hex)
  "Convert a HEX rgb color to cssrgb."
  (cl-destructuring-bind (r g b)
      (mapcar 'kurecolor-to-8bit (kurecolor-hex-to-rgb hex))
    (format "rgb(%i, %i, %i)" r g b)))

(defun kurecolor-hex-to-cssrgba (hex)
  "Convert a HEX rgb color to css rgba (only with 1.0 alpha)."
  (cl-destructuring-bind (r g b)
      (mapcar 'kurecolor-to-8bit (kurecolor-hex-to-rgb hex))
    (format "rgba(%i, %i, %i, 1.0)" r g b)))

(defun kurecolor-xcode-color-literal-to-hex-rgba(color-literal)
  "Convert an XCode COLOR-LITERAL to a hex rgba string."
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

(defun kurecolor-xcode-color-literal-to-hex-rgb(color-literal)
  "Convert an XCode COLOR-LITERAL to a hex rgb string."
  (substring
   (kurecolor-xcode-color-literal-to-hex-rgba color-literal) 0 7))

;;; Interactive functions

;;;###autoload
(defun kurecolor-increase-brightness-by-step (x)
  "Increase brightness on hex color at point (or in region) by step.
Accepts universal argument (X)."
  (interactive "P")
  (unless (numberp x) (setq x 1))
  (kurecolor-replace-current
   'kurecolor-adjust-brightness
   (/ (* x kurecolor-color-adjust-brightness-step) 100.0)))

;;;###autoload
(defun kurecolor-decrease-brightness-by-step (x)
  "Decrease brightness on hex color at point (or in region) by step.
Accepts universal argument (X)."
  (interactive "P")
  (unless (numberp x) (setq x 1))
  (kurecolor-replace-current
   'kurecolor-adjust-brightness
   (/ (* (* -1 x) kurecolor-color-adjust-brightness-step) 100.0)))

;;;###autoload
(defun kurecolor-increase-saturation-by-step (x)
  "Increase saturation on hex color at point (or in region) by step.
Accepts universal argument (X)."
  (interactive "P")
  (unless (numberp x) (setq x 1))
  (kurecolor-replace-current
   'kurecolor-adjust-saturation
   (/ (* x kurecolor-color-adjust-saturation-step) 100.0)))

;;;###autoload
(defun kurecolor-decrease-saturation-by-step (x)
  "Decrease saturation on hex color at point (or in region) by step.
Accepts universal argument (X)."
  (interactive "P")
  (unless (numberp x) (setq x 1))
  (kurecolor-replace-current
   'kurecolor-adjust-saturation
   (/ (*  (* -1 x) kurecolor-color-adjust-saturation-step) 100.0)))

;;;###autoload
(defun kurecolor-increase-hue-by-step (x)
  "Increase hue on hex color at point (or in region) by step.
Accepts universal argument (X)."
  (interactive "P")
  (unless (numberp x) (setq x 1))
  (kurecolor-replace-current
   'kurecolor-adjust-hue
   (/ (* x kurecolor-color-adjust-hue-step) 360.0)))

;;;###autoload
(defun kurecolor-decrease-hue-by-step (x)
  "Decrease hue on hex color at point (or in region) by step.
Accepts universal argument (X)."
  (interactive "P")
  (unless (numberp x) (setq x 1))
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
  "CSS rgb color at point or region to hex rgb."
  (interactive)
  (kurecolor-replace-current 'kurecolor-cssrgb-to-hex))

;;;###autoload
(defun kurecolor-hexcolor-at-point-or-region-to-css-rgb ()
  "Hex rgb color at point or region to css rgb color."
  (interactive)
  (kurecolor-replace-current 'kurecolor-hex-to-cssrgb))

;;;###autoload
(defun kurecolor-hexcolor-at-point-or-region-to-css-rgba ()
  "Hex rgb color at point or region to css rgba.
Opacity is always set to 1.0."
  (interactive)
  (kurecolor-replace-current 'kurecolor-hex-to-cssrgba))

;;;###autoload
(defun kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgba ()
  "XCode color literal at point to hex rgba."
  (interactive)
  (kurecolor-replace-current 'kurecolor-xcode-color-literal-to-hex-rgba))

;;;###autoload
(defun kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgb ()
  "XCode color literal at point to hex rgb."
  (interactive)
  (kurecolor-replace-current 'kurecolor-xcode-color-literal-to-hex-rgb))

(provide 'kurecolor)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; no-byte-compile: t
;; End:

;;; kurecolor.el ends here
