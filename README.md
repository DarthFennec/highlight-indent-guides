[![MELPA](https://melpa.org/packages/highlight-indent-guides-badge.svg)](https://melpa.org/#/highlight-indent-guides)

highlight-indent-guides.el
==========================

This minor mode highlights indentation levels via `font-lock`. Indent widths are
dynamically discovered, which means this correctly highlights in any mode,
regardless of indent width, even in languages with non-uniform indentation such
as Haskell. By default, this mode also inspects your theme dynamically, and
automatically chooses appropriate colors for highlighting. This mode works
properly around hard tabs and mixed indentation, and it behaves well in large
buffers.

Screenshots
-----------

![responsive gif](https://i.imgur.com/hIUIKgs.gif)

``` emacs-lisp
(setq highlight-indent-guides-method ...)
```

`'fill` | `'column` | `'character`
--------|-----------|-------------
![fill method screenshot][img1] | ![column method screenshot][img2] | ![character method screenshot][img3]

[img1]: https://i.imgur.com/SSlo0So.png
[img2]: https://i.imgur.com/drZ1We2.png
[img3]: https://i.imgur.com/GgjFtYH.png

Installation
------------

To install from [Melpa](http://melpa.org/#/getting-started), use <kbd>M-x
package-install RET highlight-indent-guides RET</kbd>. Otherwise, download
`highlight-indent-guides.el` and put it in your load path.

Usage
-----

Once the mode is installed, do <kbd>M-x highlight-indent-guides-mode</kbd> to
enable it. To enable it automatically in most programming modes, use the
following:

``` emacs-lisp
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
```

Configuration
-------------

This mode supports three display methods. To change the display method,
customize `highlight-indent-guides-method`, and set it to one of the following:
- `fill`: The default method. All whitespace used for indentation is
  highlighted. The color of each level of indentation alternates between
  `highlight-indent-guides-odd-face` and `highlight-indent-guides-even-face`.
- `column`: Like `fill`, but only the first column of each level of indentation
  is highlighted.
- `character`: The first column of each level of indentation is drawn using a
  column of characters. The character to draw with is specified by
  `highlight-indent-guides-character`, and it is drawn using the face
  `highlight-indent-guides-character-face`.

For example:

``` emacs-lisp
(setq highlight-indent-guides-method 'character)
```

To change the character used for drawing guide lines with the `character`
display method, customize `highlight-indent-guides-character`.

For example:

``` emacs-lisp
(setq highlight-indent-guides-character ?\|)
```

Highlight Colors
----------------

By default, this mode dynamically chooses colors that look acceptable with the
loaded theme. It does this by altering the luminosity of the theme's background
color by a given percentage. These percentages can be tweaked, to make the
colors more intense or subtle.

For example:

``` emacs-lisp
(setq highlight-indent-guides-auto-odd-face-perc 15)
(setq highlight-indent-guides-auto-even-face-perc 15)
(setq highlight-indent-guides-auto-character-face-perc 20)
```

To set the colors manually, disable this feature and customize the faces
directly.

For example:

``` emacs-lisp
(setq highlight-indent-guides-auto-enabled nil)

(set-face-background 'highlight-indent-guides-odd-face "darkgray")
(set-face-background 'highlight-indent-guides-even-face "dimgray")
(set-face-foreground 'highlight-indent-guides-character-face "dimgray")
```

Responsive Guides
-----------------

Responsive guides allow you to visualize not only the indentation itself, but
your place in it. To enable this feature, customize
`highlight-indent-guides-responsive`, and set it to one of the following:
- `nil`: The default. Responsive guides are disabled.
- `top`: Use a different color to highlight the "current" guide (the indentation
  block of the line that the cursor is on). This changes as the cursor moves.
- `stack`: Like `top`, but also use a third color for all "ancestor" guides of
  the current guide. Again, this will change as the cursor moves around.

By default, responsive guides are not updated immediately every time the cursor
moves. Instead, guides only update after the cursor stops moving for a certain
period of time (one tenth of a second, by default). If you would like to change
this behavior, customize `highlight-indent-guides-delay`, and set it to the
number of seconds to wait. For example, to disable the delay entirely:

``` emacs-lisp
(setq highlight-indent-guides-delay 0)
```

Enabling this feature provides more highlight faces, as well as more color
modifiers for the dynamic colors feature. These are specified in the following
table:

Type | Level | Method    | Variable
-----|-------|-----------|------------------------------------------------------
face | nil   | odd       | `highlight-indent-guides-odd-face`
face | nil   | even      | `highlight-indent-guides-even-face`
face | nil   | character | `highlight-indent-guides-character-face`
face | top   | odd       | `highlight-indent-guides-top-odd-face`
face | top   | even      | `highlight-indent-guides-top-even-face`
face | top   | character | `highlight-indent-guides-top-character-face`
face | stack | odd       | `highlight-indent-guides-stack-odd-face`
face | stack | even      | `highlight-indent-guides-stack-even-face`
face | stack | character | `highlight-indent-guides-stack-character-face`
perc | nil   | odd       | `highlight-indent-guides-auto-odd-face-perc`
perc | nil   | even      | `highlight-indent-guides-auto-even-face-perc`
perc | nil   | character | `highlight-indent-guides-auto-character-face-perc`
perc | top   | odd       | `highlight-indent-guides-auto-top-odd-face-perc`
perc | top   | even      | `highlight-indent-guides-auto-top-even-face-perc`
perc | top   | character | `highlight-indent-guides-auto-top-character-face-perc`
perc | stack | odd       | `highlight-indent-guides-auto-stack-odd-face-perc`
perc | stack | even      | `highlight-indent-guides-auto-stack-even-face-perc`
perc | stack | character | `highlight-indent-guides-auto-stack-character-face-perc`

Custom Highlighter Function
---------------------------

The highlighter function is the function that calculates which faces to use to
display each guide character. If the default highlighter function isn't doing it
for you, you can write your own by customizing
`highlight-indent-guides-highlighter-function`. A custom highlighter takes three
parameters:

- `level`: The indent level this guide character exists at, starting at `0`.
- `responsive`: The responsive class of this guide character. This can be `nil`,
  `top`, or `stack`.
- `display`: The display method setting. One of `fill`, `column`, or
  `character`.

A custom highlighter should return the face to use to color the given guide
character. Alternatively, it may return `nil` to specify that the guide should
not be displayed at all.

The highlighter function is called once for each indentation character, each
time a section of the buffer is re-highlighted. To speed things up a little, the
results of the highlighter function are memoized per-character, and are reused
when possible. Because of this, a custom highlighter should run quickly, and
should not have side-effects (i.e. it should not depend on or change external
values that might differ from one call to the next). A custom highlighter can
return custom faces, but those faces will not be recognized by the dynamic color
feature, and will need to be defined and colored manually.

The following example highlighter will highlight normally, except that it will
not highlight the first two levels of indentation:

``` emacs-lisp
(defun my-highlighter (level responsive display)
  (if (> 2 level)
      nil
    (highlight-indent-guides--highlighter-default level responsive display)))

(setq highlight-indent-guides-highlighter-function 'my-highlighter)
```

Limitations
-----------

To display the `character` method guides, and to highlight tab characters
correctly, this mode controls the `display` text property of some characters via
`font-lock`. Therefore, this mode may or may not play well with other modes that
use the `display` text property. This mode may also interfere with modes that
use a display table to modify how whitespace is drawn, e.g., the `whitespace`
minor mode.

Currently, with the way this mode is designed, there is no good way to display
indent guides on empty lines.

Alternatives
------------

Package Name                    | Widths  | Hard Tabs   | Other Notes
--------------------------------|---------|-------------|---------------------
[highlight-indentation.el][1]   | Fixed   | Unsupported | Very popular
[indent-guide.el][2]            | Dynamic | Supported   | Fairly slow, jittery
[hl-indent.el][3]               | Dynamic | Unsupported | Slow for large files
[visual-indentation-mode.el][4] | Fixed   | Unsupported | Fast and slim

[1]: https://github.com/antonj/Highlight-Indentation-for-Emacs
[2]: https://github.com/zk-phi/indent-guide
[3]: https://github.com/ikirill/hl-indent
[4]: https://github.com/skeeto/visual-indentation-mode
