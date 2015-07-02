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

data: rt_begin type i.
data: rt_end   type i.

parameters: input type string lower case.
parameters: output type string lower case.
parameters: runtime type string lower case.

at selection-screen output.

* Make result field output-only
  loop at screen.
    if screen-name = 'OUTPUT' or screen-name = 'RUNTIME'.
      screen-input = 0.
      if screen-name = 'RUNTIME'.
        screen-display_3d = 0.
      endif.
      modify screen.
    endif.
  endloop.

at selection-screen.
* Initialize interpreter if not done yet
  if lr_int is not bound.
    create object lr_int.
  endif.

* Evaluate given code
  get RUN TIME FIELD rt_begin.
  output = lr_int->eval_source( input ).
  get RUN TIME FIELD rt_end.
  clear input.
  runtime = |{ rt_end - rt_begin } microseconds|.

load-of-program.
* Hitting execute gets us back to this event and initializes the interpreter,
* so we preferably want to avoid that happening inadvertently:
  perform insert_into_excl(rsdbrunt) using: 'ONLI', 'SPOS', 'PRIN', 'SJOB'.