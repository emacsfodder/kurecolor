*Author:* Jason Milkins<br>
*Version:* 1.0<br>

# Kurecolor

A collection of color tools aimed at those working with (normal 6
digit) hex color codes, useful for CSS, Emacs themes, etc. etc.

View the presentation at https://github.com/emacsfodder/kurecolor

![](kurecolor.gif)

Features include interactive step modification of hue, sat, val on
hex colors.  Color conversion algorithms, for 6 digit hex colors,
hsv, rgb, cssrgb.  Get/set h s v values from/for a color.

It's recommend you use this in conjunction with rainbow-mode, for
instant feedback on color changes.

### Doing cool things

You can do funky things like adjust the saturation of all the
colors in a selection, using macros.

The presentation touches on this, and you'll need to get your
keyboard macro skills out to really shine, but it's relatively simple.

### Step by step

Add a few colors to a buffer, or load up a css, code or simple text
buffer, which already has some colors you'd like to tweak.

1. Select the region you want to modify
2. Narrow the buffer `M-x narrow-to-region`
3. Go to the top `M-<` of the narrowed section
4. Start recording a macro `f3`
  1. Regexp i-search `C-M-s` for `#[0-9A-Fa-f]\{6\}` and `ENTER` on the first match
  2. Your cursor point will be at the end of the first color (unless the Regexp was in-adequate :( )
  3. `M-5` (to do 5 steps) `M-x kurecolor-decrease-saturation-by-step` (shortcut `M-x kure-d-sat` **TAB**)
5. Stop recording the macro `f4`
6. Run the macro again with `f4`, repeat until you are finished, or use `M-0 f4` to run the macro until it hits the end of the narrowed region (or hits an error).
8. When you're done, `M-x widen` to exit narrowing.

### Uninteresting notes...

For those interested in such things, the name Kurecolor is
perloined from a high end marker pen company.  Hopefully it will
fall silently under their radar, and we won't have to change it
when they enter the Emacs extensions market.  Hopefully this is
just sarcasm, as opposed to inadvertent prophesy.

Since the question comes up occassionally, the mode-line hack used
in the presentation is based on original work by Armit Patel. I
gisted this a while back, you can get it from.
https://gist.github.com/jasonm23/8554119

The theme is Gruvbox, although you can't see much of it. Anyway,
it's a great theme, you should go install it now. (from MELPA)

## Installing

Installing kurecolor is recommended to be done via MELPA.

    M-x package-install kurecolor

If you wish to install it manually, you already have your big boy
pants on and need no further help from me.

Enjoy!

Package-Requires: ((emacs "24.0") (s "1.0"))

Licence:
 GNU / GPL2


---
Converted from `kurecolor.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
