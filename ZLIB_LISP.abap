*&---------------------------------------------------------------------*
*& Include           ZLIB_LISP
*& Lisp interpreter written in ABAP
*& Copy and paste this code into a type I (include) program
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

* Macro to simplify the definition of a native procedure
   define _proc_meth.
     methods &1
     importing list type ref to lcl_lisp_element
       returning value(result) type ref to lcl_lisp_element
       raising lcx_lisp_eval_err.
   end-of-definition.

* Macro that implements the logic for the comparison native
* procedures, where only the comparison operator differs
   define _comparison.
     data: cell type ref to lcl_lisp_element.
     result = true.
     data: carry type decfloat34.
     if list->car->type ne lcl_lisp_element=>type_number.
       eval_err( |{ list->car->value } is not a number| ).
     endif.
     cell = list->cdr.
     carry = list->car->number.
     while cell ne nil.
       if cell->car->type ne lcl_lisp_element=>type_number.
         eval_err( |{ list->car->value } is not a number| ).
       endif.
       if carry &1 cell->car->number.
         result = nil.
         exit.
       endif.
       carry = cell->car->number.
       cell = cell->cdr.
     endwhile.
   end-of-definition.

*--------------------------------------------------------------------*
* EXCEPTIONS
*--------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcx_lisp_exception DEFINITION
*----------------------------------------------------------------------*
* General Lisp exception
*----------------------------------------------------------------------*
   class lcx_lisp_exception definition inheriting from cx_dynamic_check.
     public section.
       data: message type string read-only.
       methods: constructor importing message type string optional.
   endclass.                    "lcx_lisp_exception DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcx_lisp_exception IMPLEMENTATION
*----------------------------------------------------------------------*
   class lcx_lisp_exception implementation.
     method constructor.
       super->constructor( ).
       me->message = message.
     endmethod.                    "constructor
   endclass.                    "lcx_lisp_exception IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcx_lisp_parse_err DEFINITION
*----------------------------------------------------------------------*
* Parse exception
*----------------------------------------------------------------------*
   class lcx_lisp_parse_err definition inheriting from lcx_lisp_exception.
   endclass.                    "lcx_lisp_parse_err DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcx_lisp_eval_err DEFINITION
*----------------------------------------------------------------------*
* Evaluation exception
*----------------------------------------------------------------------*
   class lcx_lisp_eval_err definition inheriting from lcx_lisp_exception.
   endclass.                    "lcx_lisp_eval_err DEFINITION


   class lcl_lisp_environment definition deferred.

* Single element that will capture cons cells, atoms etc.
*----------------------------------------------------------------------*
*       CLASS lcl_lisp_element DEFINITION
*----------------------------------------------------------------------*
   class lcl_lisp_element definition.
     public section.

* Type definitions for the various elements
       types: tv_type type char1.
       class-data: type_symbol     type tv_type value 'S'.
       class-data: type_number     type tv_type value 'N'.
       class-data: type_conscell   type tv_type value 'C'.
       class-data: type_lambda     type tv_type value 'λ'.
       class-data: type_native     type tv_type value 'P'.

       data: type type char1.
       data: value type string.
       data: number type decfloat34.

* Specifically for cons cells:
       data: car type ref to lcl_lisp_element. "Contents of Address portion of Register
       data: cdr type ref to lcl_lisp_element. "Contents of Decrement portion of Register

* Specifically for lambdas:
       data: environment type ref to lcl_lisp_environment.

       methods: constructor importing type type tv_type.

   endclass.                    "lcl_lisp_element DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_element IMPLEMENTATION
*----------------------------------------------------------------------*
   class lcl_lisp_element implementation.
     method constructor.
       me->type = type.
     endmethod.                    "constructor
   endclass.                    "lcl_lisp_element IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_environment DEFINITION
*----------------------------------------------------------------------*
   class lcl_lisp_environment definition.
     public section.

       types: begin of ts_map,
                symbol type string,
                value type ref to lcl_lisp_element,
              end of ts_map.
       types: tt_map type hashed table of ts_map with unique key symbol.

       data: map type tt_map.
* Reference to outer (parent) environment:
       data: outer type ref to lcl_lisp_environment.

       methods:
* Find a value in the environment
            find importing symbol type any
                 returning value(cell) type ref to lcl_lisp_element
                 raising lcx_lisp_eval_err,
* Add a value to the (local) environment
         define
           importing symbol type string
                     element type ref to lcl_lisp_element,
* Convenience method to add a value and create the cell
            define_value
              importing symbol type string
                        type type lcl_lisp_element=>tv_type
                        value type any optional
              returning value(element) type ref to lcl_lisp_element.
   endclass.                    "lcl_lisp_environment DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_environment IMPLEMENTATION
*----------------------------------------------------------------------*
   class lcl_lisp_environment implementation.
     method find.
       data: ls_map type ts_map.
       read table map into ls_map with key symbol = symbol.
       if sy-subrc = 0.
         cell = ls_map-value.
       else.
* Try locate the symbol in the parent (outer) environment
         if outer is bound.
           cell = outer->find( symbol ).
         endif.
       endif.
       if cell is not bound.
         raise exception type lcx_lisp_eval_err
           exporting
             message = |Symbol { symbol } is undefined|.
       endif.
     endmethod.                    "find

     method define_value.
       data: ls_map type ts_map.
       ls_map-symbol = symbol.
       create object ls_map-value
         exporting
           type = type.
*       ls_map-value->type = type.
       if type = lcl_lisp_element=>type_number.
         ls_map-value->number = value.
       else.
         ls_map-value->value = value.
       endif.
       insert ls_map into table map.
       element = ls_map-value.
     endmethod.                    "define_cell

     method define.
       data: ls_map type ts_map.
       ls_map-symbol = symbol.
       ls_map-value = element.
       insert ls_map into table map.
     endmethod.                    "define

   endclass.                    "lcl_lisp_environment IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_interpreter DEFINITION
*----------------------------------------------------------------------*
   class lcl_lisp_interpreter definition.

     public section.

       data: code type string read-only.
       data: length type i.
       data: index type i.
       data: char type char1.
       data: env type ref to lcl_lisp_environment. "Global environment

       methods:
            constructor,
            next_char raising lcx_lisp_parse_err,
            skip_whitespace raising lcx_lisp_parse_err,
            parse_list
              returning value(list) type ref to lcl_lisp_element
              raising lcx_lisp_parse_err,
            parse_token
              returning value(element) type ref to lcl_lisp_element
              raising lcx_lisp_parse_err,
            parse
              importing code type clike
              returning value(cell) type  ref to lcl_lisp_element
              raising lcx_lisp_parse_err.

* Methods for evaluation
       methods:
                eval
           importing element type ref to lcl_lisp_element
                     environment type ref to lcl_lisp_environment
           returning value(result) type  ref to lcl_lisp_element
           raising lcx_lisp_eval_err,
* To enable a REPL, the following convenience method wraps parsing and evaluating
* and stringifies the response/error
         eval_source
           importing code type clike
           returning value(response) type string
           raising lcx_lisp_eval_err,
         to_string
           importing element type ref to lcl_lisp_element
           returning value(str) type string
           raising lcx_lisp_eval_err.

* Native functions:
       _proc_meth:
       proc_append,   ##called
       proc_car,      ##called
       proc_cdr,      ##called
       proc_cons,     ##called
       proc_length,   ##called
       proc_list,     ##called
       proc_nilp,     ##called
       proc_add,      ##called
       proc_subtract, ##called
       proc_multiply, ##called
       proc_divide,   ##called
       proc_gt,       ##called
       proc_gte,      ##called
       proc_lt,       ##called
       proc_lte,      ##called
* Not in the spec: Just adding it anyway
       proc_equal.    ##called

* true, false and nil
       data: nil type ref to   lcl_lisp_element read-only.
       data: true type ref to  lcl_lisp_element read-only.
*       data: false type ref to lcl_lisp_element read-only.

       data: begin of debug,
               active type boole_d,
               stack_level type i,
               eval type string,
             end of debug.

     protected section.
* TODO: Could incorporate this logic directly before evaluation a proc/lambda
* and do away with this method
       methods: eval_err importing message type string
         raising lcx_lisp_eval_err,
                eval_list_copy importing
                  environment type ref to lcl_lisp_environment
                  element type ref to lcl_lisp_element
                  returning value(evallist) type ref to lcl_lisp_element
                  raising lcx_lisp_eval_err.

   endclass.                    "lcl_lisp_interpreter DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_interpreter IMPLEMENTATION
*----------------------------------------------------------------------*
   class lcl_lisp_interpreter implementation.

     method constructor.
       create object env.

* Create nil, true and false values
       nil  = env->define_value( symbol = 'nil'  type = lcl_lisp_element=>type_symbol value   = 'nil' ).
       true = env->define_value( symbol = 'true' type = lcl_lisp_element=>type_symbol value   = 'true' ).

* Add native functions to environment
       env->define_value( symbol = '+'      type = lcl_lisp_element=>type_native value   = 'PROC_ADD' ).
       env->define_value( symbol = '-'      type = lcl_lisp_element=>type_native value   = 'PROC_SUBTRACT' ).
       env->define_value( symbol = '*'      type = lcl_lisp_element=>type_native value   = 'PROC_MULTIPLY' ).
       env->define_value( symbol = '/'      type = lcl_lisp_element=>type_native value   = 'PROC_DIVIDE' ).
       env->define_value( symbol = 'append' type = lcl_lisp_element=>type_native value   = 'PROC_APPEND' ).
       env->define_value( symbol = 'list'   type = lcl_lisp_element=>type_native value   = 'PROC_LIST' ).
       env->define_value( symbol = 'length' type = lcl_lisp_element=>type_native value   = 'PROC_LENGTH' ).
       env->define_value( symbol = 'car'    type = lcl_lisp_element=>type_native value   = 'PROC_CAR' ).
       env->define_value( symbol = 'cdr'    type = lcl_lisp_element=>type_native value   = 'PROC_CDR' ).
       env->define_value( symbol = 'cons'   type = lcl_lisp_element=>type_native value   = 'PROC_CONS' ).
       env->define_value( symbol = 'nil?'   type = lcl_lisp_element=>type_native value   = 'PROC_NILP' ).
       env->define_value( symbol = 'null?'  type = lcl_lisp_element=>type_native value   = 'PROC_NILP' ).
       env->define_value( symbol = '>'      type = lcl_lisp_element=>type_native value   = 'PROC_GT' ).
       env->define_value( symbol = '>='     type = lcl_lisp_element=>type_native value   = 'PROC_GTE' ).
       env->define_value( symbol = '<'      type = lcl_lisp_element=>type_native value   = 'PROC_LT' ).
       env->define_value( symbol = '<='     type = lcl_lisp_element=>type_native value   = 'PROC_LTE' ).
       env->define_value( symbol = 'equal?' type = lcl_lisp_element=>type_native value   = 'PROC_EQUAL' ).
     endmethod.                    "constructor

*--------------------------------------------------------------------*
* PARSING FUNCTIONS
*--------------------------------------------------------------------*
     method next_char.
       index = index + 1.
       if index < length.
         char = code+index(1).
       elseif index = length.
         char = space.
       elseif index > length.
* Unexpected end reached; exception
         raise exception type lcx_lisp_parse_err.
       endif.
     endmethod.                    "next_char

     method skip_whitespace.
       while char = ' ' or char = cl_abap_char_utilities=>newline or
         char = cl_abap_char_utilities=>cr_lf(1).
         next_char( ).
       endwhile.
     endmethod.                    "skip_whitespace

     method parse_list.
       data: lr_cell type ref to lcl_lisp_element.
       data: lv_empty_list type boole_d value abap_true.

       create object list
         exporting
           type = lcl_lisp_element=>type_conscell.
*       list->type = lcl_lisp_element=>type_conscell.
       lr_cell = list. "Set pointer to start of list
*       list = lr_cell. "Pointer to start of list
       next_char( ). "Skip past opening paren
       while index < length.
         skip_whitespace( ).
         if char = ')'.
           if lv_empty_list = abap_true.
             list = nil. "Result = empty list = NIL
           else.
             lr_cell->cdr = nil. "Terminate list
           endif.
           next_char( ). "Skip past closing paren
           return.
         endif.
         if lv_empty_list = abap_false.
* On at least the second item; add new cell and move pointer
           create object lr_cell->cdr
             exporting
               type = lcl_lisp_element=>type_conscell.
*           list->type = lcl_lisp_element=>type_conscell.
           lr_cell = lr_cell->cdr.
         endif.
         lv_empty_list = abap_false. "Next char was not closing paren
         lr_cell->car = parse_token( ).
       endwhile.
     endmethod.                    "parse_list

     method parse_token.
       skip_whitespace( ).
       "create object cell.
       if char = '('.
         element = parse_list( ).
       else.
         data: sval type string.
* Run to delimiter
         while index < length.
           sval = |{ sval }{ char }|.
           next_char( ).
           if char = ')' or char = ' ' or char = '('
             or char = cl_abap_char_utilities=>newline
             or char = cl_abap_char_utilities=>cr_lf(1).
             exit.
           endif.
         endwhile.
* Return atom value:
* Check whether the token can be converted to a float, to cover all
* manner of number formats, including scientific, otherwise treat it
* as a symbol (but we still store it as a string to preserve the original value
* and let the ABAP kernel do the heavy lifting later on)
         data: lv_num type decfloat34.
         condense sval.
         try.
             lv_num = sval. "If this passes, it's a number
             create object element
               exporting
                 type = lcl_lisp_element=>type_number.
             element->number = sval.
           catch cx_sy_conversion_no_number.
             create object element
               exporting
                 type = lcl_lisp_element=>type_symbol.
             element->value = sval.
         endtry.
       endif.

     endmethod.                    "parse_token

* Entry point for parsing code. This is not thread-safe, but as an ABAP
* process does not have the concept of threads, we are safe :-)
     method parse.
       me->code = code.
       length = strlen( code ).
       if length = 0.
         cell = nil.
         return.
       endif.
       index = 0.
       char = code+index(1). "Kick off things by reading first char
       skip_whitespace( ).
       if char = '('.
         cell = parse_list( ).
       else.
         cell = parse_token( ).
       endif.
     endmethod.                    "parse

**********************************************************************
*
*------------------------------- EVAL( ) ----------------------------
*
**********************************************************************
     method eval.
* Return predefined symbols as themselves to save having to look them
* up in the environment
       if element = nil. result = nil. return. endif.
       if element = true. result = true. return. endif.

       case element->type.
         when lcl_lisp_element=>type_number.
           result = element.  "Number evaluates to itself
         when lcl_lisp_element=>type_symbol. "Symbol
           result = environment->find( element->value ). "Look up symbol in environment

*---### EVAL LIST
         when lcl_lisp_element=>type_conscell. "Cons Cell = List
* To evaluate list, we must first evaluate head value
           data: lr_head type ref to lcl_lisp_element.
           data: lr_tail type ref to lcl_lisp_element.
* Evaluate first element of list to determine if it is a native procedure or lambda
           lr_head = element->car. "Unevaluated value
           lr_tail = element->cdr.

*--- QUOTE
           if lr_head->value = 'quote'. "Return the argument to quote unevaluated
* Ensure QUOTE takes a single argument
             if lr_tail->cdr ne nil.
               eval_err( |QUOTE can only take a single argument| ).
             endif.
             result = lr_tail->car.
*--- IF
           elseif lr_head->value = 'if'.
             if eval( element = lr_tail->car environment = environment  ) ne nil.
               result = eval( element = lr_tail->cdr->car environment = environment  ).
             else.
               if lr_tail->cdr->cdr = nil.
                 result = nil.
               else.
                 result = eval( element = lr_tail->cdr->cdr->car environment = environment ).
               endif.
             endif.

*           elseif lr_head->value = 'set!'.

*--- DEFINE
           elseif lr_head->value = 'define'.
             if lr_tail->car->type ne lcl_lisp_element=>type_symbol.
               eval_err( |non-symbol { lr_tail->car->value } cannot be a variable| ).
             endif.
             environment->define( symbol  = lr_tail->car->value
               element = eval( element = lr_tail->cdr->car environment = environment ) ).
             create object result
               exporting
                 type = lcl_lisp_element=>type_symbol.
             result->value = lr_tail->car->value.

*--- LAMBDA
           elseif lr_head->value = 'lambda'.
* The lambda is a special cell that stores a pointer to a list of parameters and a pointer
* to a list which is the body to be evaluated later on
             create object result
               exporting
                 type = lcl_lisp_element=>type_lambda.
             result->car = lr_tail->car.      "List of parameters
             result->cdr = lr_tail->cdr->car. "Body
* Store the reference to the environment in which the lambda was created
* This is necessary, because if the lambda is created inside another lambda, we want that environment
* to be present when we evaluate the new lambda
             result->environment = environment.

*--- BEGIN
           elseif lr_head->value = 'begin'.
             data: lr_ptr type ref to lcl_lisp_element.
             lr_ptr = lr_tail.
             do.
               result = eval( element = lr_ptr->car environment = environment ).
               if lr_ptr->cdr = nil.
                 exit.
               endif.
               lr_ptr = lr_ptr->cdr.
             enddo.

*--- NATIVE PROCEDURES AND LAMBDAS
           else.

* Other symbols at the start of the list must be evaluated first
* The evaluated head must be either a native procedure or lambda
             lr_head = eval( element = element->car environment = environment ).

* DEBUGGING
             if debug-active = abap_true.
               add 1 to debug-stack_level.
               new-line.
               do debug-stack_level times.
                 write: ' >'.
               enddo.
               debug-eval = to_string( lr_head ).
               write: debug-eval, lr_head->value.
               debug-eval = to_string( lr_tail ).
               write: debug-eval.
             endif.

* Before execution of the procedure or lambda, all parameters must be evaluated
             data: lr_args type ref to lcl_lisp_element. "Argument
             lr_args = eval_list_copy( element = lr_tail environment = environment ).

***--- NATIVE FUNCTION
             if lr_head->type = lcl_lisp_element=>type_native.

* Evaluate native function:
               call method (lr_head->value)
                 exporting
                   list   = lr_args
                 receiving
                   result = result.

***--- LAMBDA FUNCTION
             elseif lr_head->type = lcl_lisp_element=>type_lambda.
* The lambda receives its own local environment in which to execute,
* where parameters become symbols that are mapped to the corresponding arguments
               data: lr_env type ref to lcl_lisp_environment.
               create object lr_env.
               lr_env->outer = lr_head->environment.

* Assign each argument to its corresponding symbol in the newly created environment
               data: lr_par type ref to lcl_lisp_element. "Parameter
               data: lr_arg type ref to lcl_lisp_element. "Argument
               lr_par = lr_head->car. "Pointer to formal parameters
*               lr_arg = lr_tail. "Pointer to arguments
               lr_arg = lr_args. "Pointer to arguments
               if lr_par ne nil. "Nil would mean no parameters to map
                 do.
* NOTE: Each element of the argument list is evaluated before being defined in the environment
                   lr_env->define( symbol = lr_par->car->value element = lr_arg->car ).
                   if lr_par->cdr = nil.
                     exit.
                   endif.
                   lr_par = lr_par->cdr.
                   lr_arg = lr_arg->cdr.
                   if lr_arg = nil. "Premature end of arguments
                     eval_err( |Parameter mismatch| ). "TODO: Details on parameter mismatch
                   endif.
                 enddo.
               endif.
* Evaluate lambda
               result = eval( element = lr_head->cdr environment = lr_env ).

             else.
               eval_err( |Cannot evaluate { to_string( lr_head ) } - not a function| ).
             endif.

* DEBUGGING
             if debug-active = abap_true.
               new-line.
               do debug-stack_level times.
                 write: ' <'.
               enddo.
               debug-eval = to_string( result ).
               write: debug-eval.
               subtract 1 from debug-stack_level.
             endif.

           endif.
       endcase.
       if result is not bound.
* In theory this should never happen!
         eval_err( 'EVAL( ) came up empty-handed' ).
       endif.
     endmethod.                    "eval

     method to_string.
       case element->type.
         when lcl_lisp_element=>type_lambda.
           str = '<lambda>'.
         when lcl_lisp_element=>type_symbol.
           str = element->value.
         when lcl_lisp_element=>type_number.
           str = element->number.
         when lcl_lisp_element=>type_native.
           str = '<native>'.
         when lcl_lisp_element=>type_conscell.
           data: lr_elem type ref to lcl_lisp_element.
           str = '( '. "Opening paren
           lr_elem = element.
           do.
* If the next item is not a cons cell, indicate with dot notation
             if lr_elem->type ne lcl_lisp_element=>type_conscell.
               str = |{ str } . { to_string( lr_elem ) }|.
             else.
               str = |{ str } { to_string( lr_elem->car ) }|.
             endif.
             if lr_elem->cdr is not bound or lr_elem->cdr = nil.
               exit.
             endif.
             lr_elem = lr_elem->cdr. "Next element in list
           enddo.
           str = |{ str } )|. "Closing paren
       endcase.
     endmethod.                    "to_string

     method eval_source.
       data: lr_err type ref to lcx_lisp_exception.
       data: lr_result type ref to lcl_lisp_element.
       data: lx_root type ref to cx_root.
       try.
           lr_result = eval( element = parse( code ) environment = env ).
           response = to_string( lr_result ).
         catch lcx_lisp_parse_err into lr_err.
           response = |Parse: { lr_err->message }|.
         catch lcx_lisp_eval_err into lr_err.
           response = |Eval: { lr_err->message }|.
         catch lcx_lisp_exception into lr_err.
           response = lr_err->message.
           if response is initial.
             response = 'Error in processing'.
           endif.
         catch cx_root into lx_root.
           response = lx_root->get_text( ).
       endtry.
     endmethod.                    "eval_source

     method eval_err.
       raise exception type lcx_lisp_eval_err
         exporting
           message = message.
     endmethod.                    "eval_err

**********************************************************************
* NATIVE PROCEDURES
**********************************************************************
     method proc_append.
* Takes two parameters: the first must be a list, and the second can
* be of any type. Appends the second param to the first. But if the
* last element in the list is not a cons cell, we cannot append
       if list->car->type ne lcl_lisp_element=>type_conscell.
         eval_err( |{ to_string( list->car ) } is not a list| ).
       endif.
* Get to last element in list - this can make APPEND expensive, like LENGTH
       data: lr_cell type ref to lcl_lisp_element.
       lr_cell = list->car.
       while lr_cell->cdr is bound and lr_cell->cdr ne nil.
         lr_cell = lr_cell->cdr.
       endwhile.
* If the last item is not a cons cell, we cannot append
       if lr_cell->type ne lcl_lisp_element=>type_conscell.
         eval_err( |A proper list must not end with { to_string( list->car ) }| ).
       endif.
* Last item is a cons cell; tack on the new value
       lr_cell->cdr = list->cdr->car.
       result = list->car.
     endmethod.                    "proc_append
**********************************************************************
     method proc_car.
       if list->car = nil.
         result = nil. return.
       endif.
       result = list->car->car.
*       result = list->car.
       if result is not bound.
         break-point.
       endif.
     endmethod.                    "proc_car
**********************************************************************
     method proc_cdr.
       if list->cdr = nil and list->car = nil.
         result = nil. return.
       endif.
       result = list->car->cdr.
*       result = list->cdr.
       if result is not bound.
         break-point.
       endif.
     endmethod.                    "proc_cdr
**********************************************************************
     method proc_cons.
* Create new cell and prepend it to second parameter
       create object result
         exporting
           type = lcl_lisp_element=>type_conscell.
       result->car = list->car.
       result->cdr = list->cdr->car.
     endmethod.                    "proc_cons
**********************************************************************
     method proc_length.
       if list->car->type ne lcl_lisp_element=>type_conscell.
         eval_err( |{ to_string( list->car ) } is not a list| ).
       endif.
       if list->cdr ne nil.
         eval_err( 'LIST takes only one argument' ).
       endif.

       create object result
         exporting
           type = lcl_lisp_element=>type_number.
       result->number = 0.
       if list = nil.
         return.
       endif.
* Iterate over list to count the number of items
       result->number = 1.
       data: lr_cell type ref to lcl_lisp_element.
       lr_cell = list->car.
       while lr_cell->cdr is bound and lr_cell->cdr ne nil.
         add 1 to result->number.
         lr_cell = lr_cell->cdr.
       endwhile.
* If the last item is not a cons cell, give an error
       if lr_cell->type ne lcl_lisp_element=>type_conscell.
         eval_err( |A proper list must not end with { to_string( list->car ) }| ).
       endif.
     endmethod.                    "proc_length
**********************************************************************
     method proc_list.
* The items given to us are already in a list and evaluated; we just need to return the head
       result = list.
     endmethod.                    "proc_list
**********************************************************************
     method proc_nilp.
       if list->car = nil. " or
         result = true.
       else.
         result = nil.
       endif.
     endmethod.                    "proc_nullp
**********************************************************************
     method proc_add.
       create object result
         exporting
           type = lcl_lisp_element=>type_number.
       data: cell type ref to lcl_lisp_element.
       result->number = 0.
       cell = list.
       do.
         result->number = result->number + cell->car->number.
         if cell->cdr = nil.
           exit.
         endif.
         cell = cell->cdr.
       enddo.
     endmethod.                    "proc_add
**********************************************************************
     method proc_subtract.
       create object result
         exporting
           type = lcl_lisp_element=>type_number.
       data: cell type ref to lcl_lisp_element.
       result->number = list->car->number.
       if list->cdr = nil.
         result->number = 0 - result->number.
       else.
* Subtract all consecutive numbers from the first
         cell = list->cdr.
         do.
           result->number = result->number - cell->car->number.
           if cell->cdr = nil.
             exit.
           endif.
           cell = cell->cdr.
         enddo.
       endif.
     endmethod.                    "proc_subtract
**********************************************************************
     method proc_multiply.
       create object result
         exporting
           type = lcl_lisp_element=>type_number.
       data: cell type ref to lcl_lisp_element.
       result->number = list->car->number.
       cell = list->cdr.
       if cell = nil.
         return.
       endif.
       do.
         result->number = result->number * cell->car->number.
         if cell->cdr = nil.
           exit.
         endif.
         cell = cell->cdr.
       enddo.
     endmethod.                    "proc_multiply
**********************************************************************
     method proc_divide.
       create object result
         exporting
           type = lcl_lisp_element=>type_number.
       data: cell type ref to lcl_lisp_element.
       result->number = list->car->number.
       cell = list->cdr.
       if cell = nil.
         result->number = 1 / result->number.
         return.
       endif.
       do.
         result->number = result->number / cell->car->number.
         if cell->cdr = nil.
           exit.
         endif.
         cell = cell->cdr.
       enddo.
     endmethod.                    "proc_divide
**********************************************************************
     method proc_gt.
       _comparison <=.
     endmethod.                    "proc_gt
     method proc_gte.
       _comparison <.
     endmethod.                    "proc_gte
     method proc_lt.
       _comparison >=.
     endmethod.                    "proc_lt
     method proc_lte.
       _comparison >.
     endmethod.                    "proc_lte
**********************************************************************
     method proc_equal.
       result = nil.
       if list->car->type = lcl_lisp_element=>type_number.
         if list->cdr->car->number = list->car->number and
            list->cdr->car->type  = list->car->type.
           result = true.
         endif.
       elseif list->car->type = lcl_lisp_element=>type_symbol.
         if list->cdr->car->value = list->car->value and
            list->cdr->car->type  = list->car->type.
           result = true.
         endif.
       else.
         if list->car = list->cdr->car.
           result = true.
         endif.
       endif.
     endmethod.                    "proc_equal

* Convenience method to eval the first level of items in a list
* which makes a copy and leaves the original list untouched
     method eval_list_copy.
       if element->car = nil.
         evallist = nil.
         return.
       endif.
       data: source type ref to lcl_lisp_element.
       data: target type ref to lcl_lisp_element.
       create object evallist
         exporting
           type = lcl_lisp_element=>type_conscell.
       source = element.
       target = evallist.

       do.
         target->car = eval( element = source->car environment = environment ).

         if source->cdr = nil.
           target->cdr = nil.
           exit.
         endif.
         source = source->cdr.
         create object target->cdr
           exporting
             type = lcl_lisp_element=>type_conscell.
         target = target->cdr.
       enddo.
     endmethod.                    "eval_list

   endclass.                    "lcl_lisp_interpreter IMPLEMENTATION