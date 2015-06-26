*&---------------------------------------------------------------------*
*& Report  ZUSR_LISP_TESTS
*& https://github.com/mydoghasworms/abap-lisp
*& Tests for the Lisp interpreter written in ABAP
*& Copy and paste this code into a type 1 (report) program, making sure
*& the necessary dependencies are met
*&---------------------------------------------------------------------*
*& Martin Ceronio, martin.ceronio@infosize.co.za
*& June 2015
*& MIT License (see below)
*&---------------------------------------------------------------------*
*  The MIT License (MIT)
*
*  Copyright (c) 2015 Martin Ceronio
*
*  Permission is hereby granted, free of charge, to any person obtaining a copy
*  of this software and associated documentation files (the "Software"), to deal
*  in the Software without restriction, including without limitation the rights
*  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*  copies of the Software, and to permit persons to whom the Software is
*  furnished to do so, subject to the following conditions:
*
*  The above copyright notice and this permission notice shall be included in
*  all copies or substantial portions of the Software.
*
*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
*  THE SOFTWARE.

report  zusr_lisp_tests line-size 999.

include zlib_lisp.

data: lr_int type ref to lcl_lisp_interpreter.

data: print_offset type i.

parameter: p_parse as checkbox default space. "Parse tests
parameter: p_basic as checkbox default space. "Basic tests
parameter: p_maths as checkbox default space. "Math tests
parameter: p_compa as checkbox default space. "Comparison tests
parameter: p_listp as checkbox default space. "List tests
parameter: p_func1 as checkbox default space. "Basic functions
parameter: p_funct as checkbox default space. "Functional tests

*&---------------------------------------------------------------------*
*&      Form  code_test
*&      Conduct a test with given code
*&---------------------------------------------------------------------*
form code_test using code type string.
  data: lv_result type string.
  write: / '<-', code.
  lv_result = lr_int->eval_source( code ).
  write: / '->', lv_result.
  uline.
endform.                    "code_test

*&---------------------------------------------------------------------*
*&      Form  print_element
*&      Write out a given element
*----------------------------------------------------------------------*
form print_element using element type ref to lcl_lisp_element.
  data: lr_elem type ref to lcl_lisp_element.
  case element->type.
    when lcl_lisp_element=>type_conscell.
      new-line.
      write:  at print_offset '('.
      lr_elem = element.
      do.
        add 2 to print_offset.
        perform print_element using lr_elem->car.
        subtract 2 from print_offset.
        if lr_elem->cdr = lr_int->nil.
          exit.
        endif.
        lr_elem = lr_elem->cdr.
      enddo.
      write: ')'.
    when lcl_lisp_element=>type_number or lcl_lisp_element=>type_symbol.
      write element->value.
  endcase.
endform.                    "print_element

*&---------------------------------------------------------------------*
*&      Form  parse_test
*&      Test parsing of a given piece of code and write out result
*----------------------------------------------------------------------*
form parse_test using code type string.
  write: / '<-', code.
  write: / '>-'.
  data: cell type ref to lcl_lisp_element.
  cell = lr_int->parse( code ).
  print_offset = 1.
  perform print_element using cell.
  uline.
endform.                    "parse_test


start-of-selection.

  data: code type string.
* Initialze Lisp interpreter
  create object lr_int.

*--------------------------------------------------------------------*
* BASIC TESTS

  if p_parse = abap_true.

    perform parse_test using '(define a(lambda()20))'.

    code =
      '(define riff-shuffle ' &&
      ' ( lambda (deck) (begin ' &&
      ' (define take ' &&
      ' (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq)))))) ' &&
      ' (define drop ' &&
      ' (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))' &&
      ' (define mid ' &&
      ' (lambda (seq) (/ (length seq) 2)))' &&
      ' ((combine append) (take (mid deck) deck) (drop (mid deck) deck))' &&
      ' )))' .
    perform parse_test using code.

  endif.

  if p_basic = abap_true.

* Test quote
    perform code_test using '(quote 19)'.
    perform code_test using '(quote a)'.
  endif.

  if p_maths = abap_true.

* Test addition
    perform code_test using '(+ 22 24 25)'.

* Test multiplication
    perform code_test using '(* 22)'.
    perform code_test using '(* 11 12)'.
    perform code_test using '(* 11 12 13)'.

* Test subtraction
    perform code_test using '(- 22)'.
    perform code_test using '(- 22 23 24)'.
    perform code_test using '(- (- (- (- (- 5 1) 1) 1) 1) 1)'.

* Test division
    perform code_test using '(/ 2)'.
    perform code_test using '(/ 10)'.
    perform code_test using '(/ 5 10)'.
    perform code_test using '(/ 11 12 13)'.

  endif.

  if p_listp = abap_true.

* Test list
    perform code_test using '(list ())'.
    perform code_test using '(list nil)'.
    perform code_test using '(list 22 23 24)'.
    perform code_test using '(list 22 (list 23 24))'.

* Test append
    perform code_test using '(append (list 22 (list 23 24)) 23)'.
    perform code_test using '(append (append (list 22 (list 23 24)) 23) 28)'. "Should give an error
    perform code_test using '(append (list 1) (list 2))'.

* Test length
    perform code_test using '(length (list 21 22 23 24))'.
    perform code_test using '(length (list 22 (list 23 24)))'.

* CAR & CDR test
    perform code_test using '(car (list 22 (list 23 24)))'.
    perform code_test using '(cdr (list 22 (list 23 24)))'.
    perform code_test using '(car (car (cdr (list 22 (list 23 24)))))'.
    perform code_test using '(car nil)'.
    perform code_test using '(car (list 1))'.

* Test CONS
    perform code_test using '(cons (list 1 2) (list 3 4))'.
    perform code_test using '(cons 1 nil)'.
    perform code_test using '(cons 2 (list 3 4))'.
    perform code_test using '(cons 2 3)'.

  endif.

  if p_compa = abap_true.

* Test GT
    perform code_test using '(> 1 2)'.
    perform code_test using '(> 2 1)'.
    perform code_test using '(> 4 3 2 1)'.
    perform code_test using '(> 4 3 2 2)'.

* Test GTE
    perform code_test using '(>= 2 2)'.
    perform code_test using '(>= 4 3 3 2)'.
    perform code_test using '(>= 1 4)'.

* Test LT
    perform code_test using '(< 1 2 3)'.
    perform code_test using '(< 1 2 2)'.
    perform code_test using '(< 3 1)'.

* Test equal?
    perform code_test using '(equal? 22 23)'.
    perform code_test using '(equal? 22 22)'.
    perform code_test using '(equal? (list 21) (list 21))'.

* Test IF
    perform code_test using '(if 22 23)'.
    perform code_test using '(if (< 2 1) 23)'.
    perform code_test using '(if (< 2 1) 23 24)'.

* Test DEFINE
    perform code_test using '(define 22 23)'. "Err
    perform code_test using '(define a 23)'. "Err
    perform code_test using 'a'. "Err

* Test nil?
    perform code_test using '(nil? ())'.
    perform code_test using '(nil? nil)'.
    perform code_test using '(nil? (cdr (list 1)))'.
    perform code_test using '(nil? (cdr (list 1 2)))'.

  endif.

*--------------------------------------------------------------------*
* BASIC FUNCTIONS
  if p_func1 = abap_true.

* Test LAMBDA
    perform code_test using '(define b (lambda (b) (* 10 b)))'.
    perform code_test using 'b'.
    perform code_test using '(b 20)'.
    perform code_test using '((lambda (a) (+ a 20)) 10 )'.

  endif.

*--------------------------------------------------------------------*
* FUNCTIONAL TESTS
  if p_funct = abap_true.
* COMBINE + ZIP
    perform code_test using '(define combine (lambda (f) (lambda (x y) (if (nil? x) (quote ()) (f (list (car x) (car y)) ((combine f) (cdr x) (cdr y)))))))'.
    perform code_test using '(define zip (combine cons))'.
    perform code_test using 'zip'.
    perform code_test using '(zip (list 1 2 3 4) (list 5 6 7 8))'.
    perform code_test using '(define compose (lambda (f g) (lambda (x) (f (g x)))))'.
    perform code_test using '(define repeat (lambda (f) (compose f f)))'.

* Riffle-shuffle
    code =
      '(define riff-shuffle' &&
      '(lambda (deck) (begin ' &&
      '(define take ' &&
      '(lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq)))))) ' &&
      '(define drop ' &&
      '(lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))' &&
      '(define mid ' &&
      '(lambda (seq) (/ (length seq) 2)))' &&
      '((combine append) (take (mid deck) deck) (drop (mid deck) deck))' &&
      ')))' .
    perform code_test using code.

*    lr_int->debug-active = abap_true.
    perform code_test using '(riff-shuffle (list 1 2 3 4 5 6 7 8))'.
    perform code_test using '((repeat riff-shuffle) (list 1 2 3 4 5 6 7 8))'.
    perform code_test using '(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))'.

  endif.