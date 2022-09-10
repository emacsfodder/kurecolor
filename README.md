# Kurecolor

A collection of tools aimed at those working with color, useful for CSS,
Emacs themes, etc.

[![](https://github.com/emacsfodder/kurecolor/actions/workflows/kurecolor-tests.yml/badge.svg)](https://github.com/emacsfodder/kurecolor/actions/workflows/kurecolor-tests.yml)

[![MELPA](https://stable.melpa.org/packages/kurecolor-badge.svg)](https://stable.melpa.org/#/kurecolor)
[![MELPA](https://melpa.org/packages/kurecolor-badge.svg)](https://melpa.org/#/kurecolor)

Use rainbow-mode when using kurecolor commands, for instant feedback on color changes.

## Installing

Kurecolor is on MELPA, you can install using `package.el`

    M-x package-install kurecolor

- - -

### Color/hue range grouping

* [kurecolor-hue-group](#kurecolor-hue-group-color-optional-hue-groups) `(color &optional hue-groups)`

### Utility functions

* [kurecolor-clamp](#kurecolor-clamp-num-min-max) `(num min max)`
* [kurecolor-to-8bit](#kurecolor-to-8bit-n) `(n)`
* [kurecolor-interpolate](#kurecolor-interpolate-color1-color2) `(color1 color2)`

### CSS helpers

* [kurecolor-hex-to-cssrgb](#kurecolor-hex-to-cssrgb-hex) `(hex)`
* [kurecolor-hex-to-cssrgba](#kurecolor-hex-to-cssrgba-hex) `(hex)`
* [kurecolor-cssrgb-to-hex](#kurecolor-cssrgb-to-hex-cssrgb-optional-hexrgba) `(cssrgb &optional hexrgba)`
* [kurecolor-css-rgb-value-to-number](#kurecolor-css-rgb-value-to-number-value) `(value)`

### Absolute set hue, saturation, brightness

* [kurecolor-hex-set-hue](#kurecolor-hex-set-hue-hex-hue) `(hex hue)`
* [kurecolor-hex-set-saturation](#kurecolor-hex-set-saturation-hex-sat) `(hex sat)`
* [kurecolor-hex-set-brightness](#kurecolor-hex-set-brightness-hex-val) `(hex val)`

### Set hue, saturation, brightness from another hex color

* [kurecolor-hex-set-brightness-from](#kurecolor-hex-set-brightness-from-source-target) `(source target)`
* [kurecolor-hex-set-hue-from](#kurecolor-hex-set-hue-from-source-target) `(source target)`
* [kurecolor-hex-set-saturation-from](#kurecolor-hex-set-saturation-from-source-target) `(source target)`

### RGB Hex to rgb

* [kurecolor-hex-to-rgb](#kurecolor-hex-to-rgb-hex) `(hex)`

### RGBA Hex to rgba

* [kurecolor-hex-to-rgba](#kurecolor-hex-to-rgba-hex) `(hex)`

### HSV/RGB/HEX (RGB) conversion

* [kurecolor-hex-to-hsv](#kurecolor-hex-to-hsv-hex) `(hex)`
* [kurecolor-hsv-to-hex](#kurecolor-hsv-to-hex-h-s-v) `(h s v)`
* [kurecolor-hsv-to-rgb](#kurecolor-hsv-to-rgb-h-s-v) `(h s v)`
* [kurecolor-rgb-to-hex](#kurecolor-rgb-to-hex-rgb) `(rgb)`

### Get hue, saturation, brightness

* [kurecolor-hex-get-brightness](#kurecolor-hex-get-brightness-hex) `(hex)`
* [kurecolor-hex-get-saturation](#kurecolor-hex-get-saturation-hex) `(hex)`
* [kurecolor-hex-get-hue](#kurecolor-hex-get-hue-hex) `(hex)`

### Color adjust

* [kurecolor-adjust-saturation](#kurecolor-adjust-saturation-hex-amount) `(hex amount)`
* [kurecolor-adjust-brightness](#kurecolor-adjust-brightness-hex-amount) `(hex amount)`
* [kurecolor-adjust-hue](#kurecolor-adjust-hue-hex-amount) `(hex amount)`

### XCode color literal helpers

* [kurecolor-hex-rgba-to-xcode-color-literal](#kurecolor-hex-rgba-to-xcode-color-literal-rgba) `(rgba)`
* [kurecolor-xcode-literal-to-hex-rgba](#kurecolor-xcode-literal-to-hex-rgba) 
* [kurecolor-xcode-literal-to-hex-rgb](#kurecolor-xcode-literal-to-hex-rgb) 


### kurecolor-hue-group `(color &optional hue-groups)`

Return the color hue group for `color`.

Optionally provide a list of `hue-groups`, (default uses `kurecolor-hue-groups`.)

Also available is `kurecolor-simple-hue-groups`,
both are customizable, or define your own.

This facilitates hue grouping & sorting by a secondary axis.

For example sort a list of colors by some axis (brightness or
saturation). Then group by hue groups, and sort the groups.

The format of each group in the list is:

    (group-name (n1 . n2))

Where `group-name` is a symbol to name the group,
`(n1 . n2)` is a hue range specifier (in degrees)
low `n1` to high `n2`.

`a` hue range which crosses the apex (i.e. `360°..0°`) is permitted.

```lisp
(kurecolor-hue-group "#00FF00" '((group-a (340 . 120)) (group-b (120 . 240)) (group-c (240 . 340))))
 ⇒ 'group-b
(kurecolor-hue-group "#FFFF00" kurecolor-hue-ranges)
 ⇒ 'yellow-green
(kurecolor-hue-group "#FF0000")
 ⇒ 'red
```


### kurecolor-clamp `(num min max)`

Clamp `num` to range of `min` `max`.

```lisp
(kurecolor-clamp 1 -1.0 1.0)
 ⇒ 1
(kurecolor-clamp 2 -1.0 1.0)
 ⇒ 1.0
(kurecolor-clamp -2 -1.0 1.0)
 ⇒ -1.0
```

### kurecolor-to-8bit `(n)`

Convert `n` (`0.0..1.0`) to `0-255`.

```lisp
(kurecolor-to-8bit 0)
 ⇒ 0.0
(kurecolor-to-8bit 1)
 ⇒ 255.0
(kurecolor-to-8bit 0.3231)
 ⇒ 82.3905
```

### kurecolor-interpolate `(color1 color2)`

Interpolate two hex colors `color1` and `color2`, to get their mixed color.

```lisp
(kurecolor-interpolate "#FFFFFF" "#000000")
 ⇒ "#7F7F7F"
(kurecolor-interpolate "#0077FF" "#111111")
 ⇒ "#084488"
(kurecolor-interpolate "#FF7700" "#111111")
 ⇒ "#884408"
```


### kurecolor-hex-to-cssrgb `(hex)`

Convert a `hex` rgb color to css rgb.

```lisp
(kurecolor-hex-to-cssrgb "#347291")
 ⇒ "rgb(52, 114, 145)"
(kurecolor-hex-to-cssrgb "#000000")
 ⇒ "rgb(0, 0, 0)"
(kurecolor-hex-to-cssrgb "#888888")
 ⇒ "rgb(136, 136, 136)"
```

### kurecolor-hex-to-cssrgba `(hex)`

Convert a `hex` rgba color to css rgba.

```lisp
(kurecolor-hex-to-cssrgba "#347291FF")
 ⇒ "rgba(52, 114, 145, 1.0000)"
(kurecolor-hex-to-cssrgba "#34729100")
 ⇒ "rgba(52, 114, 145, 0.0000)"
(kurecolor-hex-to-cssrgba "#34729180")
 ⇒ "rgba(52, 114, 145, 0.5020)"
```

### kurecolor-cssrgb-to-hex `(cssrgb &optional hexrgba)`

Convert a `cssrgb` (`rgb()` or `rgba()`) color to hex.

When `hexrgba` is non-nil the hex color string will be `rgba`.
If css `alpha` value isn't present, it will be set as `1.0`
i.e. no transparency

Valid css `rgb()` `rgba()` values are supported.

```lisp
(kurecolor-cssrgb-to-hex "rgb(10%, 20%, 90%)")
 ⇒ "#1933E5"
(kurecolor-cssrgb-to-hex "rgba(100%, 100%, 100%, 1.0)")
 ⇒ "#FFFFFF"
(kurecolor-cssrgb-to-hex "rgb(52, 114, 145)")
 ⇒ "#347291"
```

### kurecolor-css-rgb-value-to-number `(value)`

Convert css `rgb()` or `rgba()`: `r`, `g`, `b` `value`.

From number `0-255` or percentage, to `0-255`.

```lisp
(kurecolor-css-rgb-value-to-number "10")
 ⇒ 10
(kurecolor-css-rgb-value-to-number "10%")
 ⇒ 25.5
(kurecolor-css-rgb-value-to-number "100%")
 ⇒ 255.0
```


### kurecolor-hex-set-hue `(hex hue)`

Change a `hex` color's `hue`, amount values from `0.0..1.0`.
returns a `rgb` hex color.

```lisp
(kurecolor-hex-set-hue "#FF7700" 0.5)
 ⇒ "#00FFFF"
(kurecolor-hex-set-hue "#FF00FF" 0.5)
 ⇒ "#00FFFF"
(kurecolor-hex-set-hue "#FFFF00" 0.5)
 ⇒ "#00FFFF"
```

### kurecolor-hex-set-saturation `(hex sat)`

Change a `hex` color's saturation `sat`, amount values from `0.0..1.0`.
returns a `rgb` hex color.

```lisp
(kurecolor-hex-set-saturation "#FF7700" 0.5)
 ⇒ "#FFBB7F"
(kurecolor-hex-set-saturation "#007700" 0.5)
 ⇒ "#3B773B"
(kurecolor-hex-set-saturation "#FF0000" 0.5)
 ⇒ "#FF7F7F"
```

### kurecolor-hex-set-brightness `(hex val)`

Change a `hex` color's brightness `val`, amount values from `0.0..1.0`.
returns a `rgb` hex color.

```lisp
(kurecolor-hex-set-brightness "#FF7700" 0.5)
 ⇒ "#7F3B00"
(kurecolor-hex-set-brightness "#FF0000" 0.5)
 ⇒ "#7F0000"
(kurecolor-hex-set-brightness "#FF0077" 0.5)
 ⇒ "#7F003B"
```


### kurecolor-hex-set-brightness-from `(source target)`

Copy brightness from `source` to `target`.

```lisp
(kurecolor-hex-set-brightness-from "#008800" "#000000")
 ⇒ "#888888"
(kurecolor-hex-set-brightness-from "#00FF00" "#000000")
 ⇒ "#FFFFFF"
(kurecolor-hex-set-brightness-from "#000000" "#00FF00")
 ⇒ "#000000"
```

### kurecolor-hex-set-hue-from `(source target)`

Copy brightness from `source` to `target`.

```lisp
(kurecolor-hex-set-hue-from "#FF0000" "#00FF00")
 ⇒ "#FF0000"
(kurecolor-hex-set-hue-from "#00FF00" "#FF0000")
 ⇒ "#00FF00"
(kurecolor-hex-set-hue-from "#0088FF" "#880000")
 ⇒ "#004888"
```

### kurecolor-hex-set-saturation-from `(source target)`

Copy the saturation of `source` to `target`.

```lisp
(kurecolor-hex-set-saturation-from "#000000" "#00FF00")
 ⇒ "#FFFFFF"
(kurecolor-hex-set-saturation-from "#004400" "#00FF00")
 ⇒ "#00FF00"
(kurecolor-hex-set-saturation-from "#FFBB7F" "#FF7700")
 ⇒ "#FFBA7F"
```


### kurecolor-hex-to-rgb `(hex)`

Convert a `rgb` `hex` color to a list `(r g b)`.

The `r`,`g`,`b` values range between `0.0..1.0`.

```lisp
(kurecolor-hex-to-rgb "#347291")
 ⇒ '(0.20392156862745098 0.4470588235294118 0.5686274509803921)
(kurecolor-hex-to-rgb "#72FF91")
 ⇒ '(0.4470588235294118 1.0 0.5686274509803921)
(kurecolor-hex-to-rgb "#720091")
 ⇒ '(0.4470588235294118 0.0 0.5686274509803921)
```


### kurecolor-hex-to-rgba `(hex)`

Convert a `rgba` `hex` color to a list `(r g b a)`.

`rgba` `hex` colors = `#`rrggbbaa`` (i.e. `css` hex rgba)

e.g. `#`ffffff00`` white with no opacity.
     `#000000FF` black with no transparency.

The returned list `(rgba)` values will range between `0.0..1.0`.

```lisp
(kurecolor-hex-to-rgba "#34729100")
 ⇒ '(0.20392156862745098 0.4470588235294118 0.5686274509803921 0.0)
(kurecolor-hex-to-rgba "#FFFFFFFF")
 ⇒ '(1.0 1.0 1.0 1.0)
(kurecolor-hex-to-rgba "#72009172")
 ⇒ '(0.4470588235294118 0.0 0.5686274509803921 0.4470588235294118)
```


### kurecolor-hex-to-hsv `(hex)`

Convert a `rgb` `hex` color to `h` `s` `v`.

```lisp
(kurecolor-hex-to-hsv "#347291")
 ⇒ '(0.5555555555555556 0.6413793103448275 0.5686274509803921)
(kurecolor-hex-to-hsv "#729134")
 ⇒ '(0.2222222222222222 0.6413793103448275 0.5686274509803921)
(kurecolor-hex-to-hsv "#913472")
 ⇒ '(0.8888888888888888 0.6413793103448275 0.5686274509803921)
```

### kurecolor-hsv-to-hex `(h s v)`

Convert `h` `s` `v` to a `rgb` hex color.

```lisp
(kurecolor-hsv-to-hex 0.5555555555555556 0.65 0.5686274509803921)
 ⇒ "#327191"
(kurecolor-hsv-to-hex 1.0 0.7 1.0)
 ⇒ "#FF4C4C"
(kurecolor-hsv-to-hex 0.5 0.5 0.6)
 ⇒ "#4C9999"
```

### kurecolor-hsv-to-rgb `(h s v)`

Convert hsv (`h` `s` `v`) to red green blue.
Note: args `h` `s` `v` are expected to be a values from `0.0..1.0`

```lisp
(kurecolor-hsv-to-rgb 0.5555555555555556 0.6413793103448275 0.5686274509803921)
 ⇒ '(0.203921568627451 0.4470588235294117 0.5686274509803921)
```

### kurecolor-rgb-to-hex `(rgb)`

`rgb` as a list `(r g b)` to `rgb` hex color.

The `r`,`g`,`b` values can range between `0.0..1.0`.

```lisp
(kurecolor-rgb-to-hex '(0.20392156862745098 0.4470588235294118 0.5686274509803921))
 ⇒ "#347291"
```


### kurecolor-hex-get-brightness `(hex)`

Get the brightness of `hex` color.

```lisp
(kurecolor-hex-get-brightness "#FFFFFF")
 ⇒ 1.0
(kurecolor-hex-get-brightness "#808080")
 ⇒ 0.5019607843137255
(kurecolor-hex-get-brightness "#000000")
 ⇒ 0.0
```

### kurecolor-hex-get-saturation `(hex)`

Get the saturation of `hex` color.

```lisp
(kurecolor-hex-get-saturation "#00FF00")
 ⇒ 1.0
(kurecolor-hex-get-saturation "#7FFF7F")
 ⇒ 0.5019607843137255
(kurecolor-hex-get-saturation "#000000")
 ⇒ 0.0
```

### kurecolor-hex-get-hue `(hex)`

Get the hue of `hex` color.

```lisp
(kurecolor-hex-get-hue "#FF0000")
 ⇒ 0.0
(kurecolor-hex-get-hue "#00FF00")
 ⇒ 0.3333333333333333
(kurecolor-hex-get-hue "#0000FF")
 ⇒ 0.6666666666666666
```


### kurecolor-adjust-saturation `(hex amount)`

Adjust the `hex` color saturation by `amount` `-1.0..1.0`.

```lisp
(kurecolor-adjust-saturation "#006091" -0.1)
 ⇒ "#0E6491"
(kurecolor-adjust-saturation "#006091" -0.2)
 ⇒ "#1C6991"
(kurecolor-adjust-saturation "#006091" -0.5)
 ⇒ "#487891"
```

### kurecolor-adjust-brightness `(hex amount)`

Adjust the `hex` color brightness by `amount` `-1.0..1.0`.

```lisp
(kurecolor-adjust-brightness "#FFFFFF" -0.1)
 ⇒ "#E5E5E5"
(kurecolor-adjust-brightness "#FFFFFF" -0.2)
 ⇒ "#CCCCCC"
(kurecolor-adjust-brightness "#FFFFFF" -0.5)
 ⇒ "#7F7F7F"
```

### kurecolor-adjust-hue `(hex amount)`

Adjust the `hex` color hue by `amount` `0.0..1.0`.

```lisp
(kurecolor-adjust-hue "#FF0000" -0.1)
 ⇒ "#FF0098"
(kurecolor-adjust-hue "#FF7700" -0.2)
 ⇒ "#FF00BB"
(kurecolor-adjust-hue "#FFFF00" -0.5)
 ⇒ "#0000FF"
```


### kurecolor-hex-rgba-to-xcode-color-literal `(rgba)`

Convert a hex `rgba` string to an XCode `colorLiteral`.

```lisp
(kurecolor-hex-rgba-to-xcode-color-literal "#0E1B21FF")
 ⇒ "#colorLiteral(red: 0.0549019608, green: 0.1058823529, blue: 0.1294117647, alpha: 1.0000000000)"
(kurecolor-hex-rgba-to-xcode-color-literal "#ECF3F600")
 ⇒ "#colorLiteral(red: 0.9254901961, green: 0.9529411765, blue: 0.9647058824, alpha: 0.0000000000)"
(kurecolor-hex-rgba-to-xcode-color-literal "#ADC3CC80")
 ⇒ "#colorLiteral(red: 0.6784313725, green: 0.7647058824, blue: 0.8000000000, alpha: 0.5019607843)"
```

### kurecolor-xcode-literal-to-hex-rgba 



```lisp
(kurecolor-xcode-color-literal-to-hex-rgba "#colorLiteral(red: 0.0864074271, green: 0.1963072013, blue: 0.2599330357, alpha: 1)")
 ⇒ "#163242FF"
(kurecolor-xcode-color-literal-to-hex-rgba "#colorLiteral(red: 0.0585, green: 0.10855, blue: 0.13, alpha: 1)")
 ⇒ "#0E1B21FF"
(kurecolor-xcode-color-literal-to-hex-rgba "#colorLiteral(red: 0.9280523557, green: 0.9549868208, blue: 0.9678013393, alpha: 1)")
 ⇒ "#ECF3F6FF"
```

### kurecolor-xcode-literal-to-hex-rgb 



```lisp
(kurecolor-xcode-color-literal-to-hex-rgb "#colorLiteral(red: 0.05882352941, green: 0.1098039216, blue: 0.0, alpha: 1)")
 ⇒ "#0E1C00"
(kurecolor-xcode-color-literal-to-hex-rgb "#colorLiteral(red: 0.0585, green: 0.10855, blue: 0.13, alpha: 1)")
 ⇒ "#0E1B21"
(kurecolor-xcode-color-literal-to-hex-rgb "#colorLiteral(red: 0.9280523557, green: 0.9549868208, blue: 0.9678013393, alpha: 1)")
 ⇒ "#ECF3F6"
```


### Test/Examples

The examples documented run as  ERT tests, using the framework [ETD](//github.com/emacsfodder/etd)  (in `kurecolor-examples.el`). You can run them using `bin/test` from the package folder.`

## Ephemera

For those interested in such things, the name Kurecolor is
unashamedly nicked from a high end marker pen company.  Hopefully
this outrage will fall silently under their radar, and I won't have
to change it due to some frivilous and paranoid law
suit. (seriously guys, this is just free advertising.)

I have not been pressured into saying this, however, Kurecolor
markers and art supplies are the very best!  Buy some (many!) NOW (Like REALLY
Immediately!!) for you, your mum and your pet chinchilla Frank.

Since the question comes up occassionally, the mode-line hack used
in the presentation is based on original work by Armit Patel. I
gisted this a while back, you can get it from.
https://gist.github.com/jasonm23/8554119
