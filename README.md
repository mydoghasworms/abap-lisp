# ABAP Lisp

A Lisp Interpreter written in ABAP.

This is a very basic Lisp interpreter along the lines of http://norvig.com/lispy.html and http://howtowriteaprogram.blogspot.com/2010/11/lisp-interpreter-in-90-lines-of-c.html.
The idea is eventually to make it able to consume business functions of an ABAP application server by consuming classes and function modules.

## What works now

* Passes the first set of tests at http://norvig.com/lispytest.py
* A lot of other stuff (check commit history)

## What is being worked on

* Integration with ABAP data and function modules
* Development Environment with a REPL

For the story, read http://scn.sap.com/community/abap/blog/2015/06/24/a-lisp-interpreter-in-abap

## Installation

* Source listings starting with 'ZLIB' to be created as type 'I' (include) programs
* Source listings starting with 'ZUSR' to be creates as type '1' (report) programs

Give the source modules the same names as the files, but without the .abap extension.