*&---------------------------------------------------------------------*
*& Include           ZLIB_LISP
*& https://github.com/mydoghasworms/abap-lisp
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
       class-data: type_string     type tv_type value '"'.
       class-data: type_conscell   type tv_type value 'C'.
       class-data: type_lambda     type tv_type value 'λ'.
       class-data: type_native     type tv_type value 'P'.
* Types for ABAP integration:
       class-data: type_abap_data     type tv_type value 'D'.
       class-data: type_abap_table    type tv_type value 'T'.
       class-data: type_abap_function type tv_type value 'F'.
       class-data: type_abap_class    type tv_type value 'R'.

       data: type type char1.
       data: value type string.
       data: number type decfloat34.
* For ABAP integration
       data: data type ref to data.

* Specifically for cons cells:
       data: car type ref to lcl_lisp_element. "Contents of Address portion of Register
       data: cdr type ref to lcl_lisp_element. "Contents of Decrement portion of Register

* Specifically for lambdas:
       data: environment type ref to lcl_lisp_environment.

       methods: constructor importing type type tv_type.

   endclass.                    "lcl_lisp_element DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_abapfunction DEFINITION
*----------------------------------------------------------------------*
* Specialized element representing an ABAP function module that can
* be called
*----------------------------------------------------------------------*
   class lcl_lisp_abapfunction definition inheriting from lcl_lisp_element.
     public section.
       data: parameters type abap_func_parmbind_tab.
       data: exceptions type abap_func_excpbind_tab.

       class-methods:
         data_to_element importing value(data) type any
                         returning value(element) type ref to lcl_lisp_element,
         element_to_data importing value(element) type ref to lcl_lisp_element
                         returning value(data) type ref to data.
   endclass.                    "lcl_lisp_abapfunction DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_lisp_abapfunction IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
   class lcl_lisp_abapfunction implementation.
     method data_to_element.
     endmethod.                    "data_to_element
     method element_to_data.
     endmethod.                    "element_to_data
   endclass.                    "lcl_lisp_abapfunction IMPLEMENTATION

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

       types: tt_element type standard table of ref to lcl_lisp_element with default key.

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
              returning value(elements) type tt_element
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
       proc_eql,      ##called
* Not in the spec: Just adding it anyway
       proc_equal.    ##called

* Built-in functions for ABAP integration:
       _proc_meth:
       proc_abap_data,          ##called
       proc_abap_function,      ##called
       proc_abap_function_call. ##called

* true, false and nil
       data: nil type ref to   lcl_lisp_element read-only.
       data: true type ref to  lcl_lisp_element read-only.
*       data: false type ref to lcl_lisp_element read-only.

     protected section.
* TODO: Could incorporate this logic directly before evaluation a proc/lambda
* and do away with this method
       methods: eval_err importing message type string
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
       env->define_value( symbol = '='      type = lcl_lisp_element=>type_native value   = 'PROC_EQL' ). "Math equal
       env->define_value( symbol = 'equal?' type = lcl_lisp_element=>type_native value   = 'PROC_EQUAL' ).

* Native functions for ABAP integration
       env->define_value( symbol = 'abap-data'          type = lcl_lisp_element=>type_native value   = 'PROC_ABAP_DATA' ).
       env->define_value( symbol = 'abap-function'      type = lcl_lisp_element=>type_native value   = 'PROC_ABAP_FUNCTION' ).

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
       data: sval type string.
       "create object cell.
       if char = '('.
         element = parse_list( ).
       elseif char = ''''. "Quoted element
* ' is just a shortcut for QUOTE, so we wrap the consecutive element in a list starting with the quote symbol
* so that when it is evaluated later, it returns the quote elements unmodified
         create object element
           exporting
             type = lcl_lisp_element=>type_conscell.
         create object element->car
           exporting
             type = lcl_lisp_element=>type_symbol.
         element->car->value = 'quote'.
         create object element->cdr
           exporting
             type = lcl_lisp_element=>type_conscell.
         element->cdr->cdr = nil.
         next_char( ). "Skip past single quote
         element->cdr->car = parse_token( ).
       elseif char = '"'.
         data: pchar type char1.
         next_char( ). "Skip past opening quote
         while index < length.
           if char = '"' and pchar ne '\'.
             exit.
           endif.
           concatenate sval char into sval respecting blanks.
*           sval = |{ sval }{ char }|.
           pchar = char.
           next_char( ).
         endwhile.
         next_char( ). "Skip past closing quote
         create object element
           exporting
             type = lcl_lisp_element=>type_string.
         element->value = sval.
         return.
       else.
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
       data: element type ref to lcl_lisp_element.
       me->code = code.
       length = strlen( code ).
       if length = 0.
         append nil to elements.
         return.
       endif.
       index = 0.
       char = code+index(1). "Kick off things by reading first char
       while index < length.
         skip_whitespace( ).
         if char = '('.
           element = parse_list( ).
         else.
           element = parse_token( ).
         endif.
         append element to elements.
       endwhile.
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
         when lcl_lisp_element=>type_number or lcl_lisp_element=>type_string.
           result = element.  "Number or string evaluates to itself
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

*--- DEFINE
           elseif lr_head->value = 'define'.
*             if lr_tail->car->type ne lcl_lisp_element=>type_symbol.
*               eval_err( |non-symbol { lr_tail->car->value } cannot be a variable| ).
*             endif.
* Assign symbol
             if lr_tail->car->type = lcl_lisp_element=>type_symbol.
               environment->define( symbol  = lr_tail->car->value
                 element = eval( element = lr_tail->cdr->car environment = environment ) ).
               create object result
                 exporting
                   type = lcl_lisp_element=>type_symbol.
               result->value = lr_tail->car->value.
* Function shorthand (define (id arg ... ) body ...+)
             elseif lr_tail->car->type = lcl_lisp_element=>type_conscell.
* define's function shorthand allows us to define a function by specifying a list as the
* first argument where the first element is a symbol and consecutive elements are arguments
               create object result
                 exporting
                   type = lcl_lisp_element=>type_lambda.
               result->car = lr_tail->car->cdr. "List of params following function symbol
               result->cdr = lr_tail->cdr->car.
               result->environment = environment.
* Add function to the environment with symbol
               environment->define( symbol  = lr_tail->car->car->value
                 element = result ).
* TODO: Here and above: Scheme does not return a value for define; should we?
               create object result
                 exporting
                   type = lcl_lisp_element=>type_symbol.
               result->value = lr_tail->car->car->value.
             else.
               eval_err( |{ to_string( lr_tail->car ) } cannot be a variable identifier| ).
             endif.

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

* Before execution of the procedure or lambda, all parameters must be evaluated
             data: lr_args type ref to lcl_lisp_element. "Argument
             data: lr_arg_source type ref to lcl_lisp_element.
             data: lr_arg_target type ref to lcl_lisp_element.

* TODO: Need to review whether the way I am evaluating the tail is correct...
             if lr_tail = nil or lr_tail->car = nil.
               lr_args = nil.
             else.
               create object lr_args
                 exporting
                   type = lcl_lisp_element=>type_conscell.
               lr_arg_source = lr_tail.
               lr_arg_target = lr_args.

               do.
                 lr_arg_target->car = eval( element = lr_arg_source->car environment = environment ).

                 if lr_arg_source->cdr = nil.
                   lr_arg_target->cdr = nil.
                   exit.
                 endif.
                 lr_arg_source = lr_arg_source->cdr.
                 create object lr_arg_target->cdr
                   exporting
                     type = lcl_lisp_element=>type_conscell.
                 lr_arg_target = lr_arg_target->cdr.
               enddo.

             endif.

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

* >>> TEST: Support evaluation of ABAP function directly
             elseif lr_head->type = lcl_lisp_element=>type_abap_function.
* Recompose as if calling a PROC (which we are). This is part of the test. If we make an ABAP function
* call first-class, then we would need to revisit evaluating the whole of ELEMENT in one shot
               data: lr_funcinput type ref to lcl_lisp_element.
               create object lr_funcinput
                 exporting
                   type = lcl_lisp_element=>type_conscell.
               lr_funcinput->car = lr_head.
               lr_funcinput->cdr = lr_tail.
               result = proc_abap_function_call( lr_funcinput ).
* <<< TEST
             else.
               eval_err( |Cannot evaluate { to_string( lr_head ) } - not a function| ).
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
* TODO: Other Lisp REPLs give back the string as a quoted string
         when lcl_lisp_element=>type_symbol or lcl_lisp_element=>type_string.
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
*--------------------------------------------------------------------*
* Additions for ABAP Types:
         when lcl_lisp_element=>type_abap_function.
           str = |<ABAP function module { element->value }>|.
       endcase.
     endmethod.                    "to_string

     method eval_source.
       data: lr_err type ref to lcx_lisp_exception.
       data: lr_element type ref to lcl_lisp_element.
       data: lt_element type lcl_lisp_interpreter=>tt_element.
       data: lx_root type ref to cx_root.
       try.
           lt_element = parse( code ).
           loop at lt_element into lr_element.
             lr_element = eval( element = lr_element environment = env ).
             response = |{ response }{ to_string( lr_element ) } |.
           endloop.
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
* TODO: Check for incorrect number of arguments
*       if list->cdr->type ne lcl_lisp_element=>type_conscell.
*         eval_err( |Argument to CDR must be a pair| ).
*       endif.
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
     method proc_eql.
       if list->car->type = lcl_lisp_element=>type_number and
          list->cdr->car->type = lcl_lisp_element=>type_number and
          list->car->number = list->cdr->car->number.
         result = true.
       else.
         result = nil.
       endif.
     endmethod.                    "proc_eql
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

**********************************************************************
*       _                   _           _ _ _        _
*  __ _| |__   __ _ _ __   | |__  _   _(_) | |_     (_)_ __  ___
* / _` | '_ \ / _` | '_ \  | '_ \| | | | | | __|____| | '_ \/ __|
*| (_| | |_) | (_| | |_) | | |_) | |_| | | | ||_____| | | | \__ \
* \__,_|_.__/ \__,_| .__/  |_.__/ \__,_|_|_|\__|    |_|_| |_|___/
*                  |_|
**********************************************************************

     method proc_abap_data.
* First input: name of data type, second input: value
       create object result
         exporting
           type = lcl_lisp_element=>type_abap_data.
       create data result->data type (list->car->value).
* Set value if supplied as second parameter
       field-symbols: <value> type any.
       if list->cdr ne nil.
         assign result->data->* to <value>.
         if list->cdr->car->type = lcl_lisp_element=>type_number.
           <value> = list->cdr->car->number.
         else.
           <value> = list->cdr->car->value.
         endif.
       endif.
     endmethod.                    "proc_abap_data
     method proc_abap_function.

       data: function type ref to lcl_lisp_abapfunction.
       data: function_name type rs38l-name.

       create object function
         exporting
           type = lcl_lisp_element=>type_abap_function.

       function->value = list->car->value. "Name of function module
       function_name = list->car->value.

* Determine the parameters of the function module to populate parameter table
* TODO: At the moment, we do not support reference types in function module interfaces
       data: ls_par type abap_func_parmbind.
       data: ls_exc type abap_func_excpbind.

       data: lt_desc_exc type table of rsexc.
       data: lt_desc_exp type table of rsexp.
       data: lt_desc_imp type table of rsimp.
       data: lt_desc_cha type table of rscha.
       data: lt_desc_tab type table of rstbl.
       data: ls_desc_exc type rsexc.
       data: ls_desc_exp type rsexp.
       data: ls_desc_imp type rsimp.
       data: ls_desc_cha type rscha.
       data: ls_desc_tab type rstbl.

* Read the function module interface
       call function 'FUNCTION_IMPORT_INTERFACE'
         exporting
           funcname           = function_name
         tables
           exception_list     = lt_desc_exc
           export_parameter   = lt_desc_exp
           import_parameter   = lt_desc_imp
           changing_parameter = lt_desc_cha
           tables_parameter   = lt_desc_tab
         exceptions
           error_message      = 1
           function_not_found = 2
           invalid_name       = 3
           others             = 4.
       if sy-subrc <> 0.
         eval_err( |Could not get function interface of { function->value } ({ sy-subrc })| ).
       endif.

* Create structures in parameter tables
* TABLES
       loop at lt_desc_tab into ls_desc_tab.
         clear ls_par.
         ls_par-kind = abap_func_tables.
         ls_par-name = ls_desc_tab-parameter.
         if ls_desc_tab-typ is initial.
           create data ls_par-value type table of (ls_desc_tab-dbstruct).
           create data ls_par-tables_wa type (ls_desc_tab-dbstruct).
         else.
           create data ls_par-value type table of (ls_desc_tab-typ).
           create data ls_par-tables_wa type (ls_desc_tab-typ).
         endif.
         insert ls_par into table function->parameters.
       endloop.
* IMPORT
       loop at lt_desc_imp into ls_desc_imp.
         clear ls_par.
         ls_par-kind = abap_func_exporting.
         ls_par-name = ls_desc_imp-parameter.
         if ls_desc_imp-dbfield is not initial.
           create data ls_par-value type (ls_desc_imp-dbfield).
         elseif ls_desc_imp-typ is not initial.
           create data ls_par-value type (ls_desc_imp-typ).
         else.
           create data ls_par-value type text100. "Fallback for untyped parameters
         endif.
         insert ls_par into table function->parameters.
       endloop.
* EXPORT
       loop at lt_desc_exp into ls_desc_exp.
         clear ls_par.
         ls_par-kind = abap_func_importing.
         ls_par-name = ls_desc_exp-parameter.
         if ls_desc_exp-dbfield is not initial.
           create data ls_par-value type (ls_desc_exp-dbfield).
         elseif ls_desc_exp-typ is not initial.
           create data ls_par-value type (ls_desc_exp-typ).
         else.
           create data ls_par-value type text100. "Fallback for untyped parameters
         endif.
         insert ls_par into table function->parameters.
       endloop.
* CHANGING
       loop at lt_desc_cha into ls_desc_cha.
         clear ls_par.
         ls_par-kind = abap_func_changing.
         ls_par-name = ls_desc_cha-parameter.
         if ls_desc_cha-dbfield is not initial.
           create data ls_par-value type (ls_desc_cha-dbfield).
         elseif ls_desc_cha-typ is not initial.
           create data ls_par-value type (ls_desc_cha-typ).
         else.
           create data ls_par-value type text100. "Fallback for untyped parameters
         endif.
         insert ls_par into table function->parameters.
       endloop.

       result = function.

     endmethod.                    "proc_abap_function
     method proc_abap_function_call.

       data: lr_func type ref to lcl_lisp_abapfunction.

* The first parameter must be a function module instance
       if list->car->type ne lcl_lisp_element=>type_abap_function.
         eval_err( |{ list->car->value } is not a function module reference| ).
       endif.
       lr_func ?= list->car.

* Map given list to parameters of function module

* First parameter: Name of function to call; second parameter: data to pass to interface
       call function list->car->value
         parameter-table lr_func->parameters
         exception-table lr_func->exceptions.

* Map output parameters to new list

       result = list->car. "Function reference is updated with values after call

     endmethod.                    "proc_abap_function_call
   endclass.                    "lcl_lisp_interpreter IMPLEMENTATION