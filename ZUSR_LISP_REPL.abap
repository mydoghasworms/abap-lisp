*&---------------------------------------------------------------------*
*& Report  ZUSR_LISP_REPL
*& https://github.com/mydoghasworms/abap-lisp
*& Simple REPL for Lisp Interpreter written in ABAP
*& Martin Ceronio, June 2015
*& martin.ceronio@infosize.co.za
*&---------------------------------------------------------------------*

report  zusr_lisp_repl line-size 999.

include zlib_lisp.

data: lr_int type ref to lcl_lisp_interpreter. "The Lisp interpreter

parameters: input type string lower case.
parameters: output type string lower case.

at selection-screen output.

* Make result field output-only
  loop at screen.
    if screen-name = 'OUTPUT'.
      screen-input = 0.
      modify screen.
    endif.
  endloop.

at selection-screen.
* Initialize interpreter if not done yet
  if lr_int is not bound.
    create object lr_int.
  endif.

* Evaluate given code
  output = lr_int->eval_source( input ).
  clear input.

load-of-program.
* Hitting execute gets us back to this event and initializes the interpreter,
* so we preferably want to avoid that happening inadvertently:
  perform insert_into_excl(rsdbrunt) using: 'ONLI', 'SPOS', 'PRIN', 'SJOB'.