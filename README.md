# Minilisp

This project provides a very simple Lisp implementation written in the Haskell language. The original idea behind this project was to implement as an embeddable lisp repl environment, and quite unintentionnally as an attempt to show how easy it is to design a fully functional minimal Lisp interpreter by using functional programming. 

# Features

Minilisp is based on the well-known paper by John McCarty (http://www-formal.stanford.edu/jmc/recursive.pdf) with minimal modifications. One of the most remarkable features of minilisp is dynamic scope (as in Emacs Lisp). This may sound cumbersome for a general purpose programming language, but it's really handy when you want a repl which operates on a common environment (http://www.gnu.org/software/emacs/emacs-paper.html#SEC17), which is what minilisp wants to be.

