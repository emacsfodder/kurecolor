;;; kurecolor-examples --- tests for Kurecolor
;; -*- lexical-binding: t;-*-
;;
;; Version: 1.6.2
;;
;;; Commentary:
;; Uses a prototype ert test wrapper `./etd.el'.
;;
;;; Code:
;;
;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1)) (font-lock-add-keywords nil '(("(etd-examples\\|(etd-group\\| => \\| ~> " (0 'font-lock-keyword-face))))
;; no-byte-compile: t
;; End:

;; Dev dependencies

(require 'etd)
(require 'dash)
(require 'kurecolor)

(etd-group "Color/hue range grouping"
 (etd-examples kurecolor-hue-group
  (kurecolor-hue-group
    "#00FF00"
    '((group-a (340 . 120))
      (group-b (120 . 240))
      (group-c (240 . 340))))
  => 'group-b

  (kurecolor-hue-group "#FFFF00" kurecolor-hue-groups) => 'yellow-green
  (kurecolor-hue-group "#FF0000") => 'red
  (kurecolor-hue-group "#00FF00") => 'green
  (kurecolor-hue-group "#FF00FF") => 'magenta
  (kurecolor-hue-group "#00FFFF") => 'cyan
  (kurecolor-hue-group "#0000FF") => 'blue-magenta
  (kurecolor-hue-group "#FFFF00") => 'yellow-green
  (kurecolor-hue-group "#FFFF00" kurecolor-simple-hue-groups) => 'green))

(etd-group "Utility functions"
 (etd-examples kurecolor-clamp
   (kurecolor-clamp 1 -1.0 1.0)    => 1
   (kurecolor-clamp 2 -1.0 1.0)    => 1.0
   (kurecolor-clamp -2 -1.0 1.0)   => -1.0
   (kurecolor-clamp 0.1 -1.0 1.0)  => 0.1
   (kurecolor-clamp -0.1 -1.0 1.0) => -0.1)

 (etd-examples kurecolor-to-8bit
   (kurecolor-to-8bit 0)      => 0.0
   (kurecolor-to-8bit 1)      => 255.0
   (kurecolor-to-8bit 0.3231) ~> 82.39
   (kurecolor-to-8bit 0.7)    => 178.5
   (kurecolor-to-8bit 0.25)   => 63.75)

 (etd-examples kurecolor-interpolate
   (kurecolor-interpolate "#FFFFFF" "#000000") => "#7F7F7F"
   (kurecolor-interpolate "#0077FF" "#111111") => "#084488"
   (kurecolor-interpolate "#FF7700" "#111111") => "#884408"
   (kurecolor-interpolate "#7F7F7F" "#7F7F7F") => "#7F7F7F"
   (kurecolor-interpolate "#000000" "#000000") => "#000000"))

(etd-group "CSS helpers"
 (etd-examples kurecolor-hex-to-cssrgb
   (kurecolor-hex-to-cssrgb "#347291") => "rgb(52, 114, 145)"
   (kurecolor-hex-to-cssrgb "#000000") => "rgb(0, 0, 0)"
   (kurecolor-hex-to-cssrgb "#888888") => "rgb(136, 136, 136)"
   (kurecolor-hex-to-cssrgb "#FFFFFF") => "rgb(255, 255, 255)")

 (etd-examples kurecolor-hex-to-cssrgba
   (kurecolor-hex-to-cssrgba "#347291FF") => "rgba(52, 114, 145, 1.0000)"
   (kurecolor-hex-to-cssrgba "#34729100") => "rgba(52, 114, 145, 0.0000)"
   (kurecolor-hex-to-cssrgba "#34729180") => "rgba(52, 114, 145, 0.5020)")

 (etd-examples kurecolor-cssrgb-to-hex
   (kurecolor-cssrgb-to-hex "rgb(10%, 20%, 90%)")          => "#1933E5"
   (kurecolor-cssrgb-to-hex "rgba(100%, 100%, 100%, 1.0)") => "#FFFFFF"
   (kurecolor-cssrgb-to-hex "rgb(52, 114, 145)")           => "#347291"
   (kurecolor-cssrgb-to-hex "rgba(52, 114, 145, 1.0)")     => "#347291"
   (kurecolor-cssrgb-to-hex "rgba(52, 114, 145, 1.0)")     => "#347291"
   (kurecolor-cssrgb-to-hex "rgb(52, 114, 145)" t)         => "#347291FF"
   (kurecolor-cssrgb-to-hex "rgba(52, 114, 145, 0.5)" t)   => "#3472917F")

 (etd-examples kurecolor-css-rgb-value-to-number
   (kurecolor-css-rgb-value-to-number "10") => 10
   (kurecolor-css-rgb-value-to-number "10%") => 25.5
   (kurecolor-css-rgb-value-to-number "100%") => 255.0))

(etd-group "Absolute set hue, saturation, brightness"
 (etd-examples kurecolor-hex-set-hue
   (kurecolor-hex-set-hue "#FF7700" 0.5) => "#00FFFF"
   (kurecolor-hex-set-hue "#FF00FF" 0.5) => "#00FFFF"
   (kurecolor-hex-set-hue "#FFFF00" 0.5) => "#00FFFF")

 (etd-examples kurecolor-hex-set-saturation
   (kurecolor-hex-set-saturation "#FF7700" 0.5) => "#FFBB7F"
   (kurecolor-hex-set-saturation "#007700" 0.5) => "#3B773B"
   (kurecolor-hex-set-saturation "#FF0000" 0.5) => "#FF7F7F")

 (etd-examples kurecolor-hex-set-brightness
   (kurecolor-hex-set-brightness "#FF7700" 0.5) => "#7F3B00"
   (kurecolor-hex-set-brightness "#FF0000" 0.5) => "#7F0000"
   (kurecolor-hex-set-brightness "#FF0077" 0.5) => "#7F003B"))

(etd-group "Set hue, saturation, brightness from another hex color"
 (etd-examples kurecolor-hex-set-brightness-from
   (kurecolor-hex-set-brightness-from "#008800" "#000000") => "#888888"
   (kurecolor-hex-set-brightness-from "#00FF00" "#000000") => "#FFFFFF"
   (kurecolor-hex-set-brightness-from "#000000" "#00FF00") => "#000000")

 (etd-examples kurecolor-hex-set-hue-from
   (kurecolor-hex-set-hue-from "#FF0000" "#00FF00") => "#FF0000"
   (kurecolor-hex-set-hue-from "#00FF00" "#FF0000") => "#00FF00"
   (kurecolor-hex-set-hue-from "#0088FF" "#880000") => "#004888")

 (etd-examples kurecolor-hex-set-saturation-from
   (kurecolor-hex-set-saturation-from "#000000" "#00FF00") => "#FFFFFF"
   (kurecolor-hex-set-saturation-from "#004400" "#00FF00") => "#00FF00"
   (kurecolor-hex-set-saturation-from "#FFBB7F" "#FF7700") => "#FFBA7F"))

(etd-group "RGB Hex to rgb"
 (etd-examples kurecolor-hex-to-rgb
   (kurecolor-hex-to-rgb "#347291") ~> '(0.2039 0.4471 0.5686)
   (kurecolor-hex-to-rgb "#72FF91") ~> '(0.4471 1.0 0.5686)
   (kurecolor-hex-to-rgb "#720091") ~> '(0.4471 0.0 0.5686)))

(etd-group "RGBA Hex to rgba"
 (etd-examples kurecolor-hex-to-rgba
   (kurecolor-hex-to-rgba "#34729100") ~> '(0.2039 0.4471 0.5686 0.0)
   (kurecolor-hex-to-rgba "#FFFFFFFF") => '(1.0 1.0 1.0 1.0)
   (kurecolor-hex-to-rgba "#72009172") ~> '(0.44715 0.0 0.5686 0.4471)))

(etd-group "HSV/RGB/HEX (RGB) conversion"
  (etd-examples kurecolor-hex-to-hsv
    (kurecolor-hex-to-hsv "#347291") ~> '(0.5556 0.6414 0.5686)
    (kurecolor-hex-to-hsv "#729134") ~> '(0.2222 0.6414 0.5686)
    (kurecolor-hex-to-hsv "#913472") ~> '(0.8889 0.6414 0.5686))

  (etd-examples kurecolor-hsv-to-hex
    (kurecolor-hsv-to-hex 0.556 0.65 0.5687) => "#327191"
    (kurecolor-hsv-to-hex 1.0 0.7 1.0) => "#FF4C4C"
    (kurecolor-hsv-to-hex 0.5 0.5 0.6) => "#4C9999")

  (etd-examples kurecolor-hsv-to-rgb
    (kurecolor-hsv-to-rgb 0.5555 0.6413 0.5686) ~> '(0.2039 0.4472 0.5686))

  (etd-examples kurecolor-rgb-to-hex
    (kurecolor-rgb-to-hex '(0.204 0.44706 0.5687)) => "#347291"))

(etd-group "Get hue, saturation, brightness"
 (etd-examples kurecolor-hex-get-brightness
   (kurecolor-hex-get-brightness "#FFFFFF") => 1.0
   (kurecolor-hex-get-brightness "#808080") ~> 0.5020
   (kurecolor-hex-get-brightness "#000000") => 0.0)

 (etd-examples kurecolor-hex-get-saturation
    (kurecolor-hex-get-saturation "#00FF00") => 1.0
    (kurecolor-hex-get-saturation "#7FFF7F") ~> 0.5020
    (kurecolor-hex-get-saturation "#000000") => 0.0)

 (etd-examples kurecolor-hex-get-hue
   (kurecolor-hex-get-hue "#FF0000") => 0.0
   (kurecolor-hex-get-hue "#00FF00") ~> 0.3333
   (kurecolor-hex-get-hue "#0000FF") ~> 0.6667))

(etd-group "Color adjust"
 (etd-examples kurecolor-adjust-saturation
   (kurecolor-adjust-saturation "#006091" -0.1) => "#0E6491"
   (kurecolor-adjust-saturation "#006091" -0.2) => "#1C6991"
   (kurecolor-adjust-saturation "#006091" -0.5) => "#487891"
   (kurecolor-adjust-saturation "#006091" -0.6) => "#567D91"
   (kurecolor-adjust-saturation "#006091" -0.7) => "#658291"
   (kurecolor-adjust-saturation "#006091" -0.8) => "#748791"
   (kurecolor-adjust-saturation "#006091" -0.9) => "#828C91"
   (kurecolor-adjust-saturation "#006091" -1.0) => "#919191"
   (kurecolor-adjust-saturation "#347291"  0.1) => "#256D91"
   (kurecolor-adjust-saturation "#347291"  0.2) => "#176891"
   (kurecolor-adjust-saturation "#256D91"  0.1) => "#166891"
   (kurecolor-adjust-saturation "#166891"  1.0) => "#006091"
   (kurecolor-adjust-saturation "#FF0000" -2.0) => "#FFFFFF")

 (etd-examples kurecolor-adjust-brightness
   (kurecolor-adjust-brightness "#FFFFFF" -0.1) => "#E5E5E5"
   (kurecolor-adjust-brightness "#FFFFFF" -0.2) => "#CCCCCC"
   (kurecolor-adjust-brightness "#FFFFFF" -0.5) => "#7F7F7F"
   (kurecolor-adjust-brightness "#FFFFFF" -0.6) => "#666666"
   (kurecolor-adjust-brightness "#FFFFFF" -0.7) => "#4C4C4C"
   (kurecolor-adjust-brightness "#FFFFFF" -0.8) => "#323232"
   (kurecolor-adjust-brightness "#FFFFFF" -0.9) => "#191919"
   (kurecolor-adjust-brightness "#FFFFFF" -1.0) => "#000000"
   (kurecolor-adjust-brightness "#000000"  0.1) => "#191919"
   (kurecolor-adjust-brightness "#000000"  0.2) => "#333333"
   (kurecolor-adjust-brightness "#AA6600"  0.1) => "#C37500"
   (kurecolor-adjust-brightness "#329847" -2.5) => "#000000")

 (etd-examples kurecolor-adjust-hue
   (kurecolor-adjust-hue "#FF0000" -0.1) => "#FF0098"
   (kurecolor-adjust-hue "#FF7700" -0.2) => "#FF00BB"
   (kurecolor-adjust-hue "#FFFF00" -0.5) => "#0000FF"
   (kurecolor-adjust-hue "#FF00FF" -0.6) => "#98FF00"
   (kurecolor-adjust-hue "#00FFFF" -0.7) => "#CC00FF"
   (kurecolor-adjust-hue "#0000FF" -0.8) => "#FF00CC"
   (kurecolor-adjust-hue "#0077FF" -0.3) => "#43FF00"
   (kurecolor-adjust-hue "#224477" -0.4) => "#667722"
   (kurecolor-adjust-hue "#543322"  0.1) => "#545122"
   (kurecolor-adjust-hue "#0474F9"  0.2) => "#B904F9"
   (kurecolor-adjust-hue "#AA6600"  0.1) => "#87AA00"
   (kurecolor-adjust-hue "#329847"  0.5) => "#983183"))

(etd-group "XCode color literal helpers"
  (etd-examples kurecolor-hex-rgba-to-xcode-color-literal
    (kurecolor-hex-rgba-to-xcode-color-literal "#0E1B21FF")
    => "#colorLiteral(red: 0.0549019608, green: 0.1058823529, blue: 0.1294117647, alpha: 1.0000000000)"
    (kurecolor-hex-rgba-to-xcode-color-literal "#ECF3F600")
    => "#colorLiteral(red: 0.9254901961, green: 0.9529411765, blue: 0.9647058824, alpha: 0.0000000000)"
    (kurecolor-hex-rgba-to-xcode-color-literal "#ADC3CC80")
    => "#colorLiteral(red: 0.6784313725, green: 0.7647058824, blue: 0.8000000000, alpha: 0.5019607843)")

  (etd-examples kurecolor-xcode-literal-to-hex-rgba
    (kurecolor-xcode-color-literal-to-hex-rgba
     "#colorLiteral(red: 0.0864074271, green: 0.1963072013, blue: 0.2599330357, alpha: 1)")
    => "#163242FF"
    (kurecolor-xcode-color-literal-to-hex-rgba
     "#colorLiteral(red: 0.0585, green: 0.10855, blue: 0.13, alpha: 1)")
    => "#0E1B21FF"
    (kurecolor-xcode-color-literal-to-hex-rgba
     "#colorLiteral(red: 0.9280523557, green: 0.9549868208, blue: 0.9678013393, alpha: 1)")
    => "#ECF3F6FF"
    (kurecolor-xcode-color-literal-to-hex-rgba
     "#colorLiteral(red: 0.6817694399, green: 0.7659880177, blue: 0.802081694, alpha: 1)")
    => "#ADC3CCFF")

  (etd-examples kurecolor-xcode-literal-to-hex-rgb
    (kurecolor-xcode-color-literal-to-hex-rgb
     "#colorLiteral(red: 0.05882352941, green: 0.1098039216, blue: 0.0, alpha: 1)")
    => "#0E1C00"
    (kurecolor-xcode-color-literal-to-hex-rgb
     "#colorLiteral(red: 0.0585, green: 0.10855, blue: 0.13, alpha: 1)")
    => "#0E1B21"
    (kurecolor-xcode-color-literal-to-hex-rgb
     "#colorLiteral(red: 0.9280523557, green: 0.9549868208, blue: 0.9678013393, alpha: 1)")
    => "#ECF3F6"
    (kurecolor-xcode-color-literal-to-hex-rgb
     "#colorLiteral(red: 0.6817694399, green: 0.7659880177, blue: 0.802081694, alpha: 1)")
    => "#ADC3CC"))

(provide 'kurecolor-examples)

;;; kurecolor-examples.el ends here
