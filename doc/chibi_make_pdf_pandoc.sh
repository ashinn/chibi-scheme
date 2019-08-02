#!/bin/sh
pandoc -s -f html -t latex -o chibi.pdf chibi.html lib/chibi/*.html lib/chibi/show/base.html
