# Lysys

Wigh-resolution Lindenmayer systems with Common Lisp and Asymptote. 

## Setup

Lysys is a standard Quicklisp project. Clone the repo into your Quicklisp ```local-projects``` directory. Then do 

```
CL-USER> (ql:quickload :lysys)
CL-USER> (in-package :lysys)
LYSYS>
```

in the Common Lisp listener. Lysys doesn't export any symbols, so it is easiest to work inside the ```:lysys``` package.

## Usage

Browse the L-systems in ```database.lisp```. Pick one or write your own. For example, the symbol ```'pentadendrite``` contains the rules, axiom and angle to make the Pentadendrite fractal. Now compute a list of vertices (saved to file) to depth 4 by doing

```
LYSYS> (compute-l-system 'pentadendrite 4)
```

Then invoke Asymptote with

```
$ asy -V -fpdf render-path.asy
```

which creates a PDF image of the L-system using 32k cycled colors.

## Examples

![pentadendrite](https://github.com/tedszy/lysys/blob/main/gallery/pentadendrite.jpeg)


