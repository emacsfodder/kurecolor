# Kurecolor
A collection of tools aimed at those working with color, useful for CSS,
Emacs themes, etc.

[![MELPA](https://stable.melpa.org/packages/kurecolor-badge.svg)](https://stable.melpa.org/#/kurecolor)
[![MELPA](https://melpa.org/packages/kurecolor-badge.svg)](https://melpa.org/#/kurecolor)

When using kurecolor commands, we suggest using rainbow-mode for instant feedback on color changes.

## Installing

Kurecolor is on MELPA, you can install using `package.el`

    M-x package-install kurecolor

 - - -
## Functions

### kurecolor--all-hex-colors-in-region-apply

Use `func` and `args` to modify all hex colors found in region.
When region is not set, act on the whole buffer.

For example, to set the brightness on all colors in region to 50%.

```
(kurecolor--all-hex-colors-in-region-apply kurecolor-hex-set-brightness 0.5)
```

<sup>function signature</sup>
```lisp
(kurecolor--all-hex-colors-in-region-apply (func &rest args))
```

- - -

### kurecolor-adjust-brightness

Adjust the `hex` color brightness by `amount` 0.0-0.1.

<sup>function signature</sup>
```lisp
(kurecolor-adjust-brightness (hex amount))
```

- - -

### kurecolor-adjust-hue

Adjust the `hex` color hue by `amount` 0.0-0.1.

<sup>function signature</sup>
```lisp
(kurecolor-adjust-hue (hex amount))
```

- - -

### kurecolor-adjust-saturation

Adjust the `hex` color saturation by `amount` 0.0-0.1.

<sup>function signature</sup>
```lisp
(kurecolor-adjust-saturation (hex amount))
```

- - -

### kurecolor-clamp

Clamp `num` to range of `min` `max`.

<sup>function signature</sup>
```lisp
(kurecolor-clamp (num min max))
```

- - -

### kurecolor-cssrgb-at-point-or-region-to-hex [command]

CSS rgb color at point or region to hex rgb.

<sup>function signature</sup>
```lisp
(kurecolor-cssrgb-at-point-or-region-to-hex)
```

- - -

### kurecolor-cssrgb-to-hex

Convert a `cssrgb` (or rgba) color to hex (alpha value is ignored).

<sup>function signature</sup>
```lisp
(kurecolor-cssrgb-to-hex (cssrgb))
```

- - -

### kurecolor-decrease-brightness-by-step [command]

Decrease brightness on hex color at point (or in region) by step.
Accepts universal argument (`x`).

<sup>function signature</sup>
```lisp
(kurecolor-decrease-brightness-by-step (x))
```

- - -

### kurecolor-decrease-hue-by-step [command]

Decrease hue on hex color at point (or in region) by step.
Accepts universal argument (`x`).

<sup>function signature</sup>
```lisp
(kurecolor-decrease-hue-by-step (x))
```

- - -

### kurecolor-decrease-saturation-by-step [command]

Decrease saturation on hex color at point (or in region) by step.
Accepts universal argument (`x`).

<sup>function signature</sup>
```lisp
(kurecolor-decrease-saturation-by-step (x))
```

- - -

### kurecolor-hex-adjust-brightness-in-region [command]

Set the `brightness` of all hex colors found in region.
When region not active, act on the whole buffer.

<sup>function signature</sup>
```lisp
(kurecolor-hex-adjust-brightness-in-region (brightness))
```

- - -

### kurecolor-hex-adjust-hue-in-region [command]

Set the `hue` of all hex colors found in region (BEGIN END).
When region not active, act on the whole buffer.

<sup>function signature</sup>
```lisp
(kurecolor-hex-adjust-hue-in-region (hue))
```

- - -

### kurecolor-hex-adjust-saturation-in-region [command]

Adjust the `saturation` on all hex colors found in region.
When region not active, act on the whole buffer.

<sup>function signature</sup>
```lisp
(kurecolor-hex-adjust-saturation-in-region (saturation))
```

- - -

### kurecolor-hex-get-brightness

Get the brightness of `hex` color.

<sup>function signature</sup>
```lisp
(kurecolor-hex-get-brightness (hex))
```

- - -

### kurecolor-hex-get-hue

Get the hue of `hex` color.

<sup>function signature</sup>
```lisp
(kurecolor-hex-get-hue (hex))
```

- - -

### kurecolor-hex-get-saturation

Get the saturation of `hex` color.

<sup>function signature</sup>
```lisp
(kurecolor-hex-get-saturation (hex))
```

- - -

### kurecolor-hex-hue-group [command]

Given a `hex` color.
Insert a list of hexcolors of different hue.

<sup>function signature</sup>
```lisp
(kurecolor-hex-hue-group (hex))
```

- - -

### kurecolor-hex-rgb-at-point-or-region-to-xcode-color-literal [command]

Hex rgb to XCode rgba color literal.

<sup>function signature</sup>
```lisp
(kurecolor-hex-rgb-at-point-or-region-to-xcode-color-literal)
```

- - -

### kurecolor-hex-rgba-at-point-or-region-to-xcode-color-literal [command]

Hex rgba to XCode rgba color literal.

<sup>function signature</sup>
```lisp
(kurecolor-hex-rgba-at-point-or-region-to-xcode-color-literal)
```

- - -

### kurecolor-hex-rgba-to-xcode-color-literal

Convert a hex `rgba` string to an XCode color-literal.

<sup>function signature</sup>
```lisp
(kurecolor-hex-rgba-to-xcode-color-literal (rgba))
```

- - -

### kurecolor-hex-sat-group [command]

Given a `hex` color.
Insert a list of hexcolors of different saturation (sat).

<sup>function signature</sup>
```lisp
(kurecolor-hex-sat-group (hex))
```

- - -

### kurecolor-hex-set-brightness

Change a `hex` color's brightness `val`, amount values from 0.0-1.0.
returns a 6 digit hex color.

<sup>function signature</sup>
```lisp
(kurecolor-hex-set-brightness (hex val))
```

- - -

### kurecolor-hex-set-brightness-from

Copy brightness from `source` to `target`.

<sup>function signature</sup>
```lisp
(kurecolor-hex-set-brightness-from (source target))
```

- - -

### kurecolor-hex-set-brightness-in-region [command]

Set the `brightness` of all hex colors found in region.
When region not active, act on the whole buffer.

<sup>function signature</sup>
```lisp
(kurecolor-hex-set-brightness-in-region (brightness))
```

- - -

### kurecolor-hex-set-hue

Change a `hex` color's `hue`, amount values from 0-1.
returns a 6 digit hex color.

<sup>function signature</sup>
```lisp
(kurecolor-hex-set-hue (hex hue))
```

- - -

### kurecolor-hex-set-hue-from

Copy brightness from `source` to `target`.

<sup>function signature</sup>
```lisp
(kurecolor-hex-set-hue-from (source target))
```

- - -

### kurecolor-hex-set-hue-in-region [command]

Set the `hue` of all hex colors found in region (BEGIN END).
When region not active, act on the whole buffer.

<sup>function signature</sup>
```lisp
(kurecolor-hex-set-hue-in-region (hue))
```

- - -

### kurecolor-hex-set-saturation

Change a `hex` color's saturation `sat`, amount values from 0-1.
returns a 6 digit hex color.

<sup>function signature</sup>
```lisp
(kurecolor-hex-set-saturation (hex sat))
```

- - -

### kurecolor-hex-set-saturation-from

Copy the saturation of `source` to `target`.

<sup>function signature</sup>
```lisp
(kurecolor-hex-set-saturation-from (source target))
```

- - -

### kurecolor-hex-set-saturation-in-region [command]

Set the `saturation` of all hex colors found in region.
When region not active, act on the whole buffer.

<sup>function signature</sup>
```lisp
(kurecolor-hex-set-saturation-in-region (saturation))
```

- - -

### kurecolor-hex-to-cssrgb

Convert a `hex` rgb color to cssrgb.

<sup>function signature</sup>
```lisp
(kurecolor-hex-to-cssrgb (hex))
```

- - -

### kurecolor-hex-to-cssrgba

Convert a `hex` rgba color to css rgba.

<sup>function signature</sup>
```lisp
(kurecolor-hex-to-cssrgba (hex))
```

- - -

### kurecolor-hex-to-hsv

Convert a 6 digit `hex` color to h s v.

<sup>function signature</sup>
```lisp
(kurecolor-hex-to-hsv (hex))
```

- - -

### kurecolor-hex-to-rgb

Convert a 6 digit `hex` color to r g b.

<sup>function signature</sup>
```lisp
(kurecolor-hex-to-rgb (hex))
```

- - -

### kurecolor-hex-to-rgba

Convert a 8 digit `hex` color to r g b a.

<sup>function signature</sup>
```lisp
(kurecolor-hex-to-rgba (hex))
```

- - -

### kurecolor-hex-val-group [command]

Given a `hex` color.
Insert a list of hexcolors of different brightness (val).

<sup>function signature</sup>
```lisp
(kurecolor-hex-val-group (hex))
```

- - -

### kurecolor-hexcolor-at-point-or-region-to-css-rgb [command]

Hex rgb color at point or region to css rgb color.

<sup>function signature</sup>
```lisp
(kurecolor-hexcolor-at-point-or-region-to-css-rgb)
```

- - -

### kurecolor-hexcolor-at-point-or-region-to-css-rgba [command]

Hex rgb color at point or region to css rgba.
Opacity is always set to 1.0.

<sup>function signature</sup>
```lisp
(kurecolor-hexcolor-at-point-or-region-to-css-rgba)
```

- - -

### kurecolor-hsv-to-hex

Convert `h` `s` `v` to a 6 digit hex color.

<sup>function signature</sup>
```lisp
(kurecolor-hsv-to-hex (h s v))
```

- - -

### kurecolor-hsv-to-rgb

Convert hue `h`, saturation `s`, value `v` to `(red green blue)`.

`h` `s` `v` will be clamped to values from 0.0..1.0

<sup>function signature</sup>
```lisp
(kurecolor-hsv-to-rgb (h s v))
```

- - -

### kurecolor-increase-brightness-by-step [command]

Increase brightness on hex color at point (or in region) by step.
Accepts universal argument (`x`).

<sup>function signature</sup>
```lisp
(kurecolor-increase-brightness-by-step (x))
```

- - -

### kurecolor-increase-hue-by-step [command]

Increase hue on hex color at point (or in region) by step.
Accepts universal argument (`x`).

<sup>function signature</sup>
```lisp
(kurecolor-increase-hue-by-step (x))
```

- - -

### kurecolor-increase-saturation-by-step [command]

Increase saturation on hex color at point (or in region) by step.
Accepts universal argument (`x`).

<sup>function signature</sup>
```lisp
(kurecolor-increase-saturation-by-step (x))
```

- - -

### kurecolor-interpolate

Interpolate two hex colors `color1` and `color2`, to get their mixed color.

<sup>function signature</sup>
```lisp
(kurecolor-interpolate (color1 color2))
```

- - -

### kurecolor-replace-current

Get the current unspaced string at point.
Replace with the return value of the function `fn` with `args`

<sup>function signature</sup>
```lisp
(kurecolor-replace-current (fn &rest args))
```

- - -

### kurecolor-rgb-to-hex

Replacement simple `rgb` to hex.

<sup>function signature</sup>
```lisp
(kurecolor-rgb-to-hex (rgb))
```

- - -

### kurecolor-rgb-to-hsv

Convert `rgb`, a list of (r g b) to list (h s v).
For this module, h is returned as [0-1] instead of [0-360].

<sup>function signature</sup>
```lisp
(kurecolor-rgb-to-hsv (rgb))
```

- - -

### kurecolor-set-brightness [command]

Interactively change a `color`'s `brightness`.

<sup>function signature</sup>
```lisp
(kurecolor-set-brightness (color brightness))
```

- - -

### kurecolor-set-hue [command]

Interactively change a `color`'s `hue`.

<sup>function signature</sup>
```lisp
(kurecolor-set-hue (color hue))
```

- - -

### kurecolor-set-saturation [command]

Interactively change a `color`'s `saturation`.

<sup>function signature</sup>
```lisp
(kurecolor-set-saturation (color saturation))
```

- - -

### kurecolor-to-8bit

Convert `n` (0.0-1.0) to 0-255.

<sup>function signature</sup>
```lisp
(kurecolor-to-8bit (n))
```

- - -

### kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgb [command]

XCode color literal at point to hex rgb.

<sup>function signature</sup>
```lisp
(kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgb)
```

- - -

### kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgba [command]

XCode color literal at point to hex rgba.

<sup>function signature</sup>
```lisp
(kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgba)
```

- - -

### kurecolor-xcode-color-literal-to-hex-rgb

Convert an XCode `color-literal` to a hex rgb string.

<sup>function signature</sup>
```lisp
(kurecolor-xcode-color-literal-to-hex-rgb (color-literal))
```

- - -

### kurecolor-xcode-color-literal-to-hex-rgba

Convert an XCode `color-literal` to a hex rgba string.

<sup>function signature</sup>
```lisp
(kurecolor-xcode-color-literal-to-hex-rgba (color-literal))
```

- - -

### Tests

This package has a suite of unit tests.  To run them load both
kurecolor and kurecolor-test, and then do `M-x ert` (accept
`default`).

## Ephemera

For those interested in such things, the name Kurecolor is
unashamedly nicked from a high end marker pen company.  Hopefully
this outrage will fall silently under their radar, and I won't have
to change it due to some frivilous and paranoid law
suit. (seriously guys, this is just free advertising.)

I have not been pressured into saying this, however, Kurecolor
markers and art supplies are best best!  Buy some NOW (Like REALLY
Immediately!!) for you, your mum and your pet chinchilla Frank.

Since the question comes up occassionally, the mode-line hack used
in the presentation is based on original work by Armit Patel. I
gisted this a while back, you can get it from.
https://gist.github.com/jasonm23/8554119

