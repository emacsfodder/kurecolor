;; Kurecolor ERT Tests...

(require 'kurecolor)

(ert-deftest test-to-8bit ()
  "Test conversion of 0.0-0.1 to 0-255."
  (skip-unless (featurep 'kurecolor))
  (should (equal (to-8bit 0)      0.0))
  (should (equal (to-8bit 1)      255.0))
  (should (equal (to-8bit 0.3231) 82.3905))
  (should (equal (to-8bit 0.7)    178.5))
  (should (equal (to-8bit 0.25)   63.75)))

(ert-deftest test-kurecolor-interpolate ()
  "Test color interpolation."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-interpolate "#FFFFFF" "#000000") "#7F7F7F"))
  (should (equal (kurecolor-interpolate "#0077FF" "#111111") "#084488"))
  (should (equal (kurecolor-interpolate "#FF7700" "#111111") "#884408"))
  (should (equal (kurecolor-interpolate "#7F7F7F" "#7F7F7F") "#7F7F7F"))
  (should (equal (kurecolor-interpolate "#000000" "#000000") "#000000")))

(ert-deftest test-kurecolor-hex-to-cssrgb ()
  "Test conversion of hex to rgb css."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-hex-to-cssrgb "#347291") "rgb(52, 114, 145)")))

(ert-deftest test-kurecolor-hex-to-cssrgba ()
  "Test conversion of hex to rgb css."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-hex-to-cssrgba "#347291") "rgba(52, 114, 145, 1.0)")))

(ert-deftest test-kurecolor-cssrgb-to-hex ()
  "Test conversion of css rgb to hex."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-cssrgb-to-hex "rgb(52, 114, 145)") "#347291"))
  (should (equal (kurecolor-cssrgb-to-hex "rgb(10%, 20%, 90%)") "#0A145A"))
  (should (equal (kurecolor-cssrgb-to-hex "rgba(52, 114, 145, 1.0)") "#347291"))
  (should (equal (kurecolor-cssrgb-to-hex "rgba(10%, 20%, 90%, 1.0)") "#0A145A")))

(ert-deftest test-kurecolor-cssrgba-to-hex ()
  "Test conversion of css rgba to hex."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-cssrgb-to-hex "rgba(52, 114, 145, 1.0)") "#347291"))
  (should (equal (kurecolor-cssrgb-to-hex "rgba(10%, 20%, 90%, 1.0)") "#0A145A")))

(ert-deftest test-kurecolor-hex-set-hue ()
  "Test setting brightness of hex."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-hex-set-hue "#FF7700" 0.5) "#00FFFF")))

(ert-deftest test-kurecolor-hex-set-sat ()
  "Test setting sat of hex."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-hex-set-saturation "#FF7700" 0.5) "#FFBB7F")))

(ert-deftest test-kurecolor-hex-set-brightness ()
  "Test setting hue of hex."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-hex-set-brightness "#FF7700" 0.5) "#7F3B00")))

(ert-deftest test-kurecolor-hex-to-rgb ()
  "Test conversion of hex to rgb."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-hex-to-rgb "#347291")
                 (list
                  0.20392156862745098
                  0.4470588235294118
                  0.5686274509803921))))

(ert-deftest test-kurecolor-hex-to-hsv ()
  "Test conversion of hex to hsv."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-hex-to-hsv "#347291")
                 (list
                  0.5555555555555556
                  0.6413793103448275
                  0.5686274509803921))))

(ert-deftest test-kurecolor-hsv-to-hex ()
  "Test conversion of hsv hex."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-hsv-to-hex
                  0.5555555555555556
                  0.6413793103448275
                  0.5686274509803921)
                 "#347191")))

(ert-deftest test-kurecolor-rgb-to-hex ()
  "Test conversion of rgb to hex."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-rgb-to-hex
                  (list 0.20392156862745098
                        0.4470588235294118
                        0.5686274509803921))
                 "#347291"
                 )))

(ert-deftest test-kurecolor-rgb-to-hsv ()
  "Test conversion of rgb to hsv."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-hex-to-hsv "#347291")
                 (list 0.5555555555555556
                       0.6413793103448275
                       0.5686274509803921))))

(ert-deftest test-kurecolor-hsv-to-rgb ()
  "Test conversion of hsv to rgb."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-hsv-to-rgb
                  0.5555555555555556 0.6413793103448275 0.5686274509803921)
                 (list 0.203921568627451 0.4470588235294117 0.5686274509803921))))

(ert-deftest test-kurecolor-hex-get-brightness ()
  "Test getting brightness from hex."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-hex-get-brightness "#FFFFFF") 1.0))
  (should (equal (kurecolor-hex-get-brightness "#808080") (/ 128.0 255.0)))
  (should (equal (kurecolor-hex-get-brightness "#000000") 0.0)))

(ert-deftest test-kurecolor-hex-get-saturation ()
  "Test getting saturation from hex."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-hex-get-saturation "#00FF00") 1.0))
  (should (equal (kurecolor-hex-get-saturation "#7FFF7F") (/ 128.0 255.0)))
  (should (equal (kurecolor-hex-get-saturation "#000000") 0)))

(ert-deftest test-kurecolor-hex-get-hue ()
  "Test getting hue from hex."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-hex-get-hue "#FF0000") (/ 0 360.0)))
  (should (equal (kurecolor-hex-get-hue "#00FF00") (/ 120.0 360.0)))
  (should (equal (kurecolor-hex-get-hue "#0000FF") (/ 240.0 360.0))))

(ert-deftest test-kurecolor-adjust-sat ()
  "Test adjustment of sat (saturation)."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-adjust-saturation "#006091" -0.1) "#0E6491"))
  (should (equal (kurecolor-adjust-saturation "#006091" -0.2) "#1C6991"))
  (should (equal (kurecolor-adjust-saturation "#006091" -0.5) "#487891"))
  (should (equal (kurecolor-adjust-saturation "#006091" -0.6) "#567D91"))
  (should (equal (kurecolor-adjust-saturation "#006091" -0.7) "#658291"))
  (should (equal (kurecolor-adjust-saturation "#006091" -0.8) "#748791"))
  (should (equal (kurecolor-adjust-saturation "#006091" -0.9) "#828C91"))
  (should (equal (kurecolor-adjust-saturation "#006091" -1.0) "#919191"))
  (should (equal (kurecolor-adjust-saturation "#347291"  0.1) "#256D91"))
  (should (equal (kurecolor-adjust-saturation "#347291"  0.2) "#176891"))
  (should (equal (kurecolor-adjust-saturation "#256D91"  0.1) "#166891"))
  (should (equal (kurecolor-adjust-saturation "#166891"  1.0) "#006091")))

(ert-deftest test-kurecolor-adjust-val ()
  "Test adjustment of val (brightness)."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-adjust-brightness "#FFFFFF" -0.1) "#E5E5E5"))
  (should (equal (kurecolor-adjust-brightness "#FFFFFF" -0.2) "#CCCCCC"))
  (should (equal (kurecolor-adjust-brightness "#FFFFFF" -0.5) "#7F7F7F"))
  (should (equal (kurecolor-adjust-brightness "#FFFFFF" -0.6) "#666666"))
  (should (equal (kurecolor-adjust-brightness "#FFFFFF" -0.7) "#4C4C4C"))
  (should (equal (kurecolor-adjust-brightness "#FFFFFF" -0.8) "#323232"))
  (should (equal (kurecolor-adjust-brightness "#FFFFFF" -0.9) "#191919"))
  (should (equal (kurecolor-adjust-brightness "#FFFFFF" -1.0) "#000000"))
  (should (equal (kurecolor-adjust-brightness "#000000"  0.1) "#191919"))
  (should (equal (kurecolor-adjust-brightness "#000000"  0.2) "#333333"))
  (should (equal (kurecolor-adjust-brightness "#AA6600"  0.1) "#C37500"))
  (should (equal (kurecolor-adjust-brightness "#329847"  0.5) "#53FF77")))

(ert-deftest test-kurecolor-adjust-hue ()
  "Test adjustment of hue."
  (skip-unless (featurep 'kurecolor))
  (should (equal (kurecolor-adjust-hue "#FF0000" -0.1) "#FF0098"))
  (should (equal (kurecolor-adjust-hue "#FF7700" -0.2) "#FF00BB"))
  (should (equal (kurecolor-adjust-hue "#FFFF00" -0.5) "#0000FF"))
  (should (equal (kurecolor-adjust-hue "#FF00FF" -0.6) "#98FF00"))
  (should (equal (kurecolor-adjust-hue "#00FFFF" -0.7) "#CC00FF"))
  (should (equal (kurecolor-adjust-hue "#0000FF" -0.8) "#FF00CC"))
  (should (equal (kurecolor-adjust-hue "#0077FF" -0.3) "#43FF00"))
  (should (equal (kurecolor-adjust-hue "#224477" -0.4) "#667722"))
  (should (equal (kurecolor-adjust-hue "#543322"  0.1) "#545122"))
  (should (equal (kurecolor-adjust-hue "#0474F9"  0.2) "#B904F9"))
  (should (equal (kurecolor-adjust-hue "#AA6600"  0.1) "#87AA00"))
  (should (equal (kurecolor-adjust-hue "#329847"  0.5) "#983183")))

(ert-deftest test-kurecolor-xcode-literal-to-hex-rgba ()
  "Test conversion of XCode color literal to hex rgba string."
  (skip-unless (featurep 'kurecolor))
  (should (equal
           (kurecolor-xcode-color-literal-to-hex-rgba
            "#colorLiteral(red: 0.0864074271, green: 0.1963072013, blue: 0.2599330357, alpha: 1)")
           "#163242FF")))

(ert-deftest test-kurecolor-xcode-literal-to-hex-rgb ()
  "Test conversion of XCode color literal to hex rgb string."
  (skip-unless (featurep 'kurecolor))
  (should (equal
           (kurecolor-xcode-color-literal-to-hex-rgb
            "#colorLiteral(red: 0.0864074271, green: 0.1963072013, blue: 0.2599330357, alpha: 1)")
           "#163242")))

(ert-deftest test-kurecolor-xcode-literal-to-hex-rgb-zero-padding ()
  "Test conversion of XCode color literal to hex rgba string."
  (skip-unless (featurep 'kurecolor))
  (should (equal
           (kurecolor-xcode-color-literal-to-hex-rgb
            "#colorLiteral(red: 0.05882352941, green: 0.1098039216, blue: 0.1294117647, alpha: 1)")
           "#0E1C20")))

(ert-deftest test-kurecolor-multiple-xcode-literal-to-hex-rgb ()
  "Test conversion of XCode color literal to hex rgba string."
  (skip-unless (featurep 'kurecolor))
  (should (equal

           (list "#0E1B21"
                 "#ECF3F6"
                 "#ADC3CC")

           (list  (kurecolor-xcode-color-literal-to-hex-rgb
                   "#colorLiteral(red: 0.0585, green: 0.10855, blue: 0.13, alpha: 1)")

                  (kurecolor-xcode-color-literal-to-hex-rgb
                   "#colorLiteral(red: 0.9280523557, green: 0.9549868208, blue: 0.9678013393, alpha: 1)")

                  (kurecolor-xcode-color-literal-to-hex-rgb
                   "#colorLiteral(red: 0.6817694399, green: 0.7659880177, blue: 0.802081694, alpha: 1)")))))

(provide 'kurecolor-test)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; no-byte-compile: t
;; End:
