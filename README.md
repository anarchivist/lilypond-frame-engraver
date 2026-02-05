# frame-engraver: Engrave boxes around notes in Lilypond

This package provides an engraver that draws boxes around groups of notes. The code herein was written by [David Nalesnik](https://github.com/davidnalesnik) and repackaged into a [lyp](https://github.com/noteflakes/lyp) package. For more context read the messages on lilypond-user:

- https://lists.gnu.org/archive/html/lilypond-user/2012-03/msg00357.html
- https://lists.gnu.org/archive/html/lilypond-user/2017-01/msg00200.html
- https://lists.gnu.org/archive/html/lilypond-user/2025-02/msg00268.html

## Installation

```bash
$ lyp install frame-engraver
```

## Usage

```lilypond
\require "frame-engraver"

...
```

For more information see the included [example](test/frame-engraver-test.ly).

