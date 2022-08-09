# Kurecolor
A collection of tools aimed at those working with color, useful for CSS,
Emacs themes, etc.

[![MELPA](http://stable.melpa.org/packages/kurecolor-badge.svg)](http://melpa-stable.milkbox.net/#/kurecolor)
[![MELPA](https://melpa.org/packages/kurecolor-badge.svg)](https://melpa.org/#/kurecolor)

It's recommend you use kurekolor commands in conjunction with rainbow-mode, for
instant feedback on color changes.

## Installing

Kurecolor is on MELPA, you can install using `package.el`

    M-x package-install kurecolor

## Functions

### kurecolor--all-colors-in-region-apply

Use `func` and `arg` to modify all hex colors found in region.
When region is not set, act on the whole buffer.

For example, to set the brightness on all colors in region to 50%.

```
(kurecolor--all-colors-in-region-apply kurecolor-hex-set-brightness 0.5)
```

```lisp
(kurecolor--all-colors-in-region-apply (func arg))
```
<sup>function signature</sup>
- - -

### kurecolor-adjust-brightness

Adjust the `hex` color brightness by `amount` 0.0-0.1.

```lisp
(kurecolor-adjust-brightness (hex amount))
```
<sup>function signature</sup>
- - -

### kurecolor-adjust-hue

Adjust the `hex` color hue by `amount` 0.0-0.1.

```lisp
(kurecolor-adjust-hue (hex amount))
```
<sup>function signature</sup>
- - -

### kurecolor-adjust-saturation

Adjust the `hex` color saturation by `amount` 0.0-0.1.

```lisp
(kurecolor-adjust-saturation (hex amount))
```
<sup>function signature</sup>
- - -

### kurecolor-clamp

Clamp `num` to range of `min` `max`.

```lisp
(kurecolor-clamp (num min max))
```
<sup>function signature</sup>
- - -

### kurecolor-cssrgb-at-point-or-region-to-hex

CSS rgb color at point or region to hex rgb.

```lisp
(kurecolor-cssrgb-at-point-or-region-to-hex)
```
<sup>function signature</sup>
- - -

### kurecolor-cssrgb-to-hex

Convert a `cssrgb` (or rgba) color to hex (alpha value is ignored).

```lisp
(kurecolor-cssrgb-to-hex (cssrgb))
```
<sup>function signature</sup>
- - -

### kurecolor-decrease-brightness-by-step

Decrease brightness on hex color at point (or in region) by step.
Accepts universal argument (`x`).

```lisp
(kurecolor-decrease-brightness-by-step (x))
```
<sup>function signature</sup>
- - -

### kurecolor-decrease-hue-by-step

Decrease hue on hex color at point (or in region) by step.
Accepts universal argument (`x`).

```lisp
(kurecolor-decrease-hue-by-step (x))
```
<sup>function signature</sup>
- - -

### kurecolor-decrease-saturation-by-step

Decrease saturation on hex color at point (or in region) by step.
Accepts universal argument (`x`).

```lisp
(kurecolor-decrease-saturation-by-step (x))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-adjust-brightness-in-region

Set the `brightness` of all hex colors found in region.
When region not active, act on the whole buffer.

```lisp
(kurecolor-hex-adjust-brightness-in-region (brightness))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-adjust-hue-in-region

Set the `hue` of all hex colors found in region (BEGIN `end`).
When region not active, act on the whole buffer.

```lisp
(kurecolor-hex-adjust-hue-in-region (hue))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-adjust-saturation-in-region

Adjust the `saturation` on all hex colors found in region.
When region not active, act on the whole buffer.

```lisp
(kurecolor-hex-adjust-saturation-in-region (saturation))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-get-brightness

Get the brightness of `hex` color.

```lisp
(kurecolor-hex-get-brightness (hex))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-get-hue

Get the hue of `hex` color.

```lisp
(kurecolor-hex-get-hue (hex))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-get-saturation

Get the saturation of `hex` color.

```lisp
(kurecolor-hex-get-saturation (hex))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-hue-group

Given a `hex` color.
Insert a list of hexcolors of different hue.

```lisp
(kurecolor-hex-hue-group (hex))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-rgb-at-point-or-region-to-xcode-color-literal

Hex rgb to xcode rgba color literal.

```lisp
(kurecolor-hex-rgb-at-point-or-region-to-xcode-color-literal)
```
<sup>function signature</sup>
- - -

### kurecolor-hex-rgba-at-point-or-region-to-xcode-color-literal

Hex rgba to xcode rgba color literal.

```lisp
(kurecolor-hex-rgba-at-point-or-region-to-xcode-color-literal)
```
<sup>function signature</sup>
- - -

### kurecolor-hex-rgba-to-xcode-color-literal

Convert a hex `rgba` string to an xcode color-literal.

```lisp
(kurecolor-hex-rgba-to-xcode-color-literal (rgba))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-sat-group

Given a `hex` color.
Insert a list of hexcolors of different saturation (sat).

```lisp
(kurecolor-hex-sat-group (hex))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-set-brightness

Change a `hex` color's brightness `val`, amount values from 0.0-1.0.
returns a 6 digit hex color.

```lisp
(kurecolor-hex-set-brightness (hex val))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-set-brightness-from

Copy brightness from `source` to `target`.

```lisp
(kurecolor-hex-set-brightness-from (source target))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-set-brightness-in-region

Set the `brightness` of all hex colors found in region.
When region not active, act on the whole buffer.

```lisp
(kurecolor-hex-set-brightness-in-region (brightness))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-set-hue

Change a `hex` color's `hue`, amount values from 0-1.
returns a 6 digit hex color.

```lisp
(kurecolor-hex-set-hue (hex hue))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-set-hue-from

Copy brightness from `source` to `target`.

```lisp
(kurecolor-hex-set-hue-from (source target))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-set-hue-in-region

Set the `hue` of all hex colors found in region (BEGIN `end`).
When region not active, act on the whole buffer.

```lisp
(kurecolor-hex-set-hue-in-region (hue))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-set-saturation

Change a `hex` color's saturation `sat`, amount values from 0-1.
returns a 6 digit hex color.

```lisp
(kurecolor-hex-set-saturation (hex sat))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-set-saturation-from

Copy the saturation of `source` to `target`.

```lisp
(kurecolor-hex-set-saturation-from (source target))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-set-saturation-in-region

Set the `saturation` of all hex colors found in region.
When region not active, act on the whole buffer.

```lisp
(kurecolor-hex-set-saturation-in-region (saturation))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-to-cssrgb

Convert a `hex` rgb color to cssrgb.

```lisp
(kurecolor-hex-to-cssrgb (hex))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-to-cssrgba

Convert a `hex` rgba color to css rgba.

```lisp
(kurecolor-hex-to-cssrgba (hex))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-to-hsv

Convert a 6 digit `hex` color to h s v.

```lisp
(kurecolor-hex-to-hsv (hex))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-to-rgb

Convert a 6 digit `hex` color to r g b.

```lisp
(kurecolor-hex-to-rgb (hex))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-to-rgba

Convert a 8 digit `hex` color to r g b a.

```lisp
(kurecolor-hex-to-rgba (hex))
```
<sup>function signature</sup>
- - -

### kurecolor-hex-val-group

Given a `hex` color.
Insert a list of hexcolors of different brightness (val).

```lisp
(kurecolor-hex-val-group (hex))
```
<sup>function signature</sup>
- - -

### kurecolor-hexcolor-at-point-or-region-to-css-rgb

Hex rgb color at point or region to css rgb color.

```lisp
(kurecolor-hexcolor-at-point-or-region-to-css-rgb)
```
<sup>function signature</sup>
- - -

### kurecolor-hexcolor-at-point-or-region-to-css-rgba

Hex rgb color at point or region to css rgba.
Opacity is always set to 1.0.

```lisp
(kurecolor-hexcolor-at-point-or-region-to-css-rgba)
```
<sup>function signature</sup>
- - -

### kurecolor-hsv-to-hex

Convert H S V to a 6 digit `hex` color.

```lisp
(kurecolor-hsv-to-hex (h s v))
```
<sup>function signature</sup>
- - -

### kurecolor-hsv-to-rgb

Convert hsv (`h` `s` `v`) to red green blue.
Note: args `h` `s` `v` are floats from 0.0..1.0

```lisp
(kurecolor-hsv-to-rgb (h s v))
```
<sup>function signature</sup>
- - -

### kurecolor-increase-brightness-by-step

Increase brightness on hex color at point (or in region) by step.
Accepts universal argument (`x`).

```lisp
(kurecolor-increase-brightness-by-step (x))
```
<sup>function signature</sup>
- - -

### kurecolor-increase-hue-by-step

Increase hue on hex color at point (or in region) by step.
Accepts universal argument (`x`).

```lisp
(kurecolor-increase-hue-by-step (x))
```
<sup>function signature</sup>
- - -

### kurecolor-increase-saturation-by-step

Increase saturation on hex color at point (or in region) by step.
Accepts universal argument (`x`).

```lisp
(kurecolor-increase-saturation-by-step (x))
```
<sup>function signature</sup>
- - -

### kurecolor-interpolate

Interpolate two hex colors `color1` and `color2`, to get their mixed color.

```lisp
(kurecolor-interpolate (color1 color2))
```
<sup>function signature</sup>
- - -

### kurecolor-replace-current

Get the current unspaced string at point.
Replace with the return value of the function `fn` with `args`

```lisp
(kurecolor-replace-current (fn &rest args))
```
<sup>function signature</sup>
- - -

### kurecolor-rgb-to-hex

Replacement simple `rgb` to hex.

```lisp
(kurecolor-rgb-to-hex (rgb))
```
<sup>function signature</sup>
- - -

### kurecolor-rgb-to-hsv

Convert `rgb`, a list of (r g b) to list (h s v).
For this module, h is returned as [0-1] instead of [0-360].

```lisp
(kurecolor-rgb-to-hsv (rgb))
```
<sup>function signature</sup>
- - -

### kurecolor-set-brightness

Interactively change a `color``s `brightness`.

```lisp
(kurecolor-set-brightness (color brightness))
```
<sup>function signature</sup>
- - -

### kurecolor-set-hue

Interactively change a `color``s `hue`.

```lisp
(kurecolor-set-hue (color hue))
```
<sup>function signature</sup>
- - -

### kurecolor-set-saturation

Interactively change a `color``s `saturation`.

```lisp
(kurecolor-set-saturation (color saturation))
```
<sup>function signature</sup>
- - -

### kurecolor-to-8bit

Convert N (0.0-1.0) to 0-255.

```lisp
(kurecolor-to-8bit (n))
```
<sup>function signature</sup>
- - -

### kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgb

XCode color literal at point to hex rgb.

```lisp
(kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgb)
```
<sup>function signature</sup>
- - -

### kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgba

XCode color literal at point to hex rgba.

```lisp
(kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgba)
```
<sup>function signature</sup>
- - -

### kurecolor-xcode-color-literal-to-hex-rgb

Convert an xcode `color-literal` to a hex rgb string.

```lisp
(kurecolor-xcode-color-literal-to-hex-rgb (color-literal))
```
<sup>function signature</sup>
- - -

### kurecolor-xcode-color-literal-to-hex-rgba

Convert an xcode `color-literal` to a hex rgba string.

```lisp
(kurecolor-xcode-color-literal-to-hex-rgba (color-literal))
```
<sup>function signature</sup>
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

