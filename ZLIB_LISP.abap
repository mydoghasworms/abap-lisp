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
       class-data: type_hash       type tv_type value 'H'.
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
*       CLASS lcl_lisp_hash DEFINITION
*----------------------------------------------------------------------*
* Hash is a specialized ABAP Lisp type for quick lookup of elements
* using a symbol or string key (backed by an ABAP hash table)
*----------------------------------------------------------------------*
   class lcl_lisp_hash definition inheriting from lcl_lisp_element.
     public section.
       types: begin of ts_hash,
                key type string,
                element type ref to lcl_lisp_element,
              end of ts_hash.
       types: tt_hash type hashed table of ts_hash with unique key key.
       data: hash type tt_hash.
   endclass.                    "lcl_lisp_abapfunction DEFINITION

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
* List of actual parameters passed
       data: paramact type abap_func_parmbind_tab.
   endclass.                    "lcl_lisp_abapfunction DEFINITION

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
* TODO: reuse DEFINE() here
       insert ls_map into table map.
* To comply with Scheme define, overwrite existing defined values
       if sy-subrc = 4.
         modify table map from ls_map.
       endif.
       element = ls_map-value.
     endmethod.                    "define_cell

     method define.
       data: ls_map type ts_map.
       ls_map-symbol = symbol.
       ls_map-value = element.
       insert ls_map into table map.
* To comply with Scheme define, overwrite existing defined values
       if sy-subrc = 4.
         modify table map from ls_map.
       endif.
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
* Functions for dealing with hashes:
       _proc_meth:
       proc_make_hash,    ##called "Create new hash
       proc_hash_get,     ##called "Get an element from a hash
       proc_hash_insert,  ##called "Insert a new element into a hash
       proc_hash_remove,  ##called "Delete an item from a hash
       proc_hash_keys.    ##called "Delete an item from a hash

* Built-in functions for ABAP integration:
       _proc_meth:
       proc_abap_data,          ##called
       proc_abap_function,      ##called
*       proc_abap_function_param,##called
       proc_abap_table,         ##called
       proc_abap_append_row,    ##called
       proc_abap_delete_row,    ##called
       proc_abap_get_row,       ##called
       proc_abap_get_value,     ##called
       proc_abap_set_value,     ##called
       proc_abap_set,           ##called
       proc_abap_get,           ##called
* Called internally only:
       proc_abap_function_call. ##called

* true, false and nil
       class-data: nil type ref to   lcl_lisp_element read-only.
       class-data: true type ref to  lcl_lisp_element read-only.
*       data: false type ref to lcl_lisp_element read-only.

     protected section.
* TODO: Could incorporate this logic directly before evaluation a proc/lambda
* and do away with this method
       class-methods: eval_err importing message type string
         raising lcx_lisp_eval_err.

*---- ABAP Integration support functions; mapping -----
       class-methods:
* Convert ABAP data to Lisp element
         data_to_element importing value(data) type any
                         returning value(element) type ref to lcl_lisp_element
                         raising lcx_lisp_eval_err,
* Convert Lisp element to ABAP Data
         element_to_data importing value(element) type ref to lcl_lisp_element
                         changing value(data) type any "ref to data
                         raising lcx_lisp_eval_err.
* Determine an ABAP data component from an element and an identifier
       class-methods:
         get_element importing value(element) type ref to lcl_lisp_element
                               value(identifier) type ref to lcl_lisp_element
                     returning value(rdata) type ref to data
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
* Hash-related functions
       env->define_value( symbol = 'make-hash'   type = lcl_lisp_element=>type_native value   = 'PROC_MAKE_HASH' ).
       env->define_value( symbol = 'hash-get'    type = lcl_lisp_element=>type_native value   = 'PROC_HASH_GET' ).
       env->define_value( symbol = 'hash-insert' type = lcl_lisp_element=>type_native value   = 'PROC_HASH_INSERT' ).
       env->define_value( symbol = 'hash-remove' type = lcl_lisp_element=>type_native value   = 'PROC_HASH_REMOVE' ).
       env->define_value( symbol = 'hash-keys'   type = lcl_lisp_element=>type_native value   = 'PROC_HASH_KEYS' ).
* Functions for type:
       env->define_value( symbol = 'string?'   type = lcl_lisp_element=>type_native value   = 'PROC_IS_STRING' ).
       env->define_value( symbol = 'hash?'     type = lcl_lisp_element=>type_native value   = 'PROC_IS_HASH' ).
       env->define_value( symbol = 'number?'   type = lcl_lisp_element=>type_native value   = 'PROC_IS_NUMBER' ).
       env->define_value( symbol = 'symbol?'   type = lcl_lisp_element=>type_native value   = 'PROC_IS_SYMBOL' ).
       env->define_value( symbol = 'type'      type = lcl_lisp_element=>type_native value   = 'PROC_IS_TYPE' ).

* Native functions for ABAP integration
       env->define_value( symbol = 'ab-data'       type = lcl_lisp_element=>type_native value   = 'PROC_ABAP_DATA' ).
       env->define_value( symbol = 'ab-function'   type = lcl_lisp_element=>type_native value   = 'PROC_ABAP_FUNCTION' ).
       env->define_value( symbol = 'ab-func-param' type = lcl_lisp_element=>type_native value   = 'PROC_ABAP_FUNCTION_PARAM' ).
       env->define_value( symbol = 'ab-table'      type = lcl_lisp_element=>type_native value   = 'PROC_ABAP_TABLE' ).
       env->define_value( symbol = 'ab-append-row' type = lcl_lisp_element=>type_native value   = 'PROC_ABAP_APPEND_ROW' ).
       env->define_value( symbol = 'ab-delete-row' type = lcl_lisp_element=>type_native value   = 'PROC_ABAP_DELETE_ROW' ).
       env->define_value( symbol = 'ab-get-row'    type = lcl_lisp_element=>type_native value   = 'PROC_ABAP_GET_ROW' ).
       env->define_value( symbol = 'ab-get-value'  type = lcl_lisp_element=>type_native value   = 'PROC_ABAP_GET_VALUE' ).
       env->define_value( symbol = 'ab-set-value'  type = lcl_lisp_element=>type_native value   = 'PROC_ABAP_SET_VALUE' ).
       env->define_value( symbol = 'ab-get'   type = lcl_lisp_element=>type_native value   = 'PROC_ABAP_GET' ).
       env->define_value( symbol = 'ab-set'   type = lcl_lisp_element=>type_native value   = 'PROC_ABAP_SET' ).
* Define a value in the environment for SYST
       data: lr_sy type ref to lcl_lisp_element.
       create object lr_sy exporting type = lcl_lisp_element=>type_abap_data.
       get reference of syst into lr_sy->data.
       env->define( symbol = 'ab-sy' element = lr_sy ).

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
               result->cdr = lr_tail->cdr.
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
             result->cdr = lr_tail->cdr. "Body
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
               lr_ptr = lr_head->cdr.
               do.
                 result = eval( element = lr_ptr->car environment = lr_env ).
                 if lr_ptr->cdr = nil.
                   exit.
                 endif.
                 lr_ptr = lr_ptr->cdr.
               enddo.

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
         when lcl_lisp_element=>type_hash.
           str = '<hash>'.
*--------------------------------------------------------------------*
* Additions for ABAP Types:
         when lcl_lisp_element=>type_abap_function.
           str = |<ABAP function module { element->value }>|.
         when lcl_lisp_element=>type_abap_data.
           str = |<ABAP Data>|.
         when lcl_lisp_element=>type_abap_table.
           str = |<ABAP Table>|.
         when lcl_lisp_element=>type_abap_class.
           str = |<ABAP Class>|.
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
* FIXME: test (cdr 23); should result in error!
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

*--------------------------------------------------------------------*
* Hash-related functions
     method proc_make_hash.
       data: lr_hash type ref to lcl_lisp_hash.
       data: lr_ptr type ref to lcl_lisp_element.
       data: ls_entry type lcl_lisp_hash=>ts_hash.
       create object lr_hash
         exporting
           type = lcl_lisp_element=>type_hash.
* Can accept a parameter which should be a list of alternating symbols/strings and elements
       if list->car->type = lcl_lisp_element=>type_conscell.
         lr_ptr = list->car.
         do.
           if lr_ptr->car->type ne lcl_lisp_element=>type_symbol and
              lr_ptr->car->type ne lcl_lisp_element=>type_string.
             eval_err( 'MAKE-HASH: Use only symbol or string as a key' ).
           endif.
           ls_entry-key = lr_ptr->car->value.
           if lr_ptr->cdr = nil.
             exit. "Entry is not added
           endif.
           lr_ptr = lr_ptr->cdr. "Move pointer to next cell
           ls_entry-element = lr_ptr->car.
           insert ls_entry into table lr_hash->hash.
           if lr_ptr->cdr = nil.
             exit. "Entry is not added
           endif.
           lr_ptr = lr_ptr->cdr. "Move pointer to next cell
         enddo.
       endif.
       result = lr_hash.
     endmethod.                    "proc_make_hash
* Get an element from a hash
     method proc_hash_get.
       data: lr_hash type ref to lcl_lisp_hash.
       data: ls_entry type lcl_lisp_hash=>ts_hash.
       if list->car->type ne lcl_lisp_element=>type_hash.
         eval_err( 'HASH-GET only works on hashes' ).
       endif.
       lr_hash ?= list->car.
       if list->cdr->car = nil.
         eval_err( 'HASH-GET requires a key to access an element' ).
       endif.
* TODO: Additional check for key type
       read table lr_hash->hash into ls_entry
         with key key = list->cdr->car->value.
       if sy-subrc = 0.
         result = ls_entry-element.
       else.
         result = nil.
       endif.
     endmethod.                    "proc_hash_get
* Insert an element into a hash
     method proc_hash_insert.
       data: lr_hash type ref to lcl_lisp_hash.
       data: ls_entry type lcl_lisp_hash=>ts_hash.
       if list->car->type ne lcl_lisp_element=>type_hash.
         eval_err( 'HASH-INSERT only works on hashes' ).
       endif.
       lr_hash ?= list->car.
* TODO: Check number and type of parameters
       ls_entry-key = list->cdr->car->value.
       ls_entry-element = list->cdr->cdr->car.
       insert ls_entry into table lr_hash->hash.
* TODO: Should we overwrite existing keys?
       result = nil.
     endmethod.                    "proc_hash_insert
* Remove an element from a hash
     method proc_hash_remove.
       data: lr_hash type ref to lcl_lisp_hash.
       if list->car->type ne lcl_lisp_element=>type_hash.
         eval_err( 'HASH-REMOVE only works on hashes' ).
       endif.
       lr_hash ?= list->car.
* TODO: Check number and type of parameters
       delete lr_hash->hash where key = list->cdr->car->value.
       result = nil.
     endmethod.                    "proc_hash_delete
* Return the keys of a hash
     method proc_hash_keys.
       data: lr_hash type ref to lcl_lisp_hash.
       data: ls_entry type lcl_lisp_hash=>ts_hash.
       data: lr_ptr type ref to lcl_lisp_element.
       data: lv_tabix type i value 0.
       if list->car->type ne lcl_lisp_element=>type_hash.
         eval_err( 'HASH-KEYS only works on hashes' ).
       endif.
       lr_hash ?= list->car.
       if lr_hash->hash is initial.
         result = nil. return.
       endif.
       create object result
         exporting
           type = lcl_lisp_element=>type_conscell.
       lr_ptr = result.
       loop at lr_hash->hash into ls_entry.
         add 1 to lv_tabix.
         if lv_tabix > 1.
           create object lr_ptr->cdr
             exporting
               type = lcl_lisp_element=>type_conscell.
           lr_ptr = lr_ptr->cdr.
         endif.
         create object lr_ptr->car
           exporting
             type = lcl_lisp_element=>type_symbol.
         lr_ptr->car->value = ls_entry-key.
       endloop.
       lr_ptr->cdr = nil.
     endmethod.                    "proc_hash_keys

**********************************************************************
*       _                   _           _ _ _        _
*  __ _| |__   __ _ _ __   | |__  _   _(_) | |_     (_)_ __  ___
* / _` | '_ \ / _` | '_ \  | '_ \| | | | | | __|____| | '_ \/ __|
*| (_| | |_) | (_| | |_) | | |_) | |_| | | | ||_____| | | | \__ \
* \__,_|_.__/ \__,_| .__/  |_.__/ \__,_|_|_|\__|    |_|_| |_|___/
*                  |_|
**********************************************************************

     method proc_abap_data.
       data: lr_desc type ref to cl_abap_typedescr.

       if list->car = nil or ( list->car->type ne lcl_lisp_element=>type_string and
                               list->car->type ne lcl_lisp_element=>type_symbol ).
         eval_err( 'AB-DATA: String or symbol required as name of type' ).
       endif.

       lr_desc = cl_abap_typedescr=>describe_by_name( list->car->value ).
       if sy-subrc ne 0. "DESCRIBE_BY_NAME has only one exception
         eval_err( |AB-DATA: Type { list->car->value } not found | ).
       endif.

       if lr_desc->kind = cl_abap_typedescr=>kind_table.
         create object result
           exporting
             type = lcl_lisp_element=>type_abap_table.
       elseif lr_desc->kind = cl_abap_typedescr=>kind_elem or
              lr_desc->kind = cl_abap_typedescr=>kind_struct.
         create object result
           exporting
             type = lcl_lisp_element=>type_abap_data.
       else.
         eval_err( |AB-DATA: Type kind { lr_desc->kind } not supported yet| ).
       endif.
* Create data as given type
       create data result->data type (list->car->value).
* Set value if supplied as second parameter
       if list->cdr ne nil.
         call method element_to_data
           exporting
             element = list->cdr->car
           changing
             data    = result->data.
       endif.
     endmethod.                    "proc_abap_data
**********************************************************************
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

*
       field-symbols: <defval> type any. "Default value for parameter

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
         insert ls_par into table function->paramact.
       endloop.

* Import/Export/Changing have same requirement
       field-symbols: <table> type standard table,
                      <row> type any,
                      <field> type any.

       do 3 times.
         if sy-index = 1.
           assign lt_desc_imp to <table>.
           ls_par-kind = abap_func_exporting.
         elseif sy-index = 2.
           assign lt_desc_exp to <table>.
           ls_par-kind = abap_func_importing.
         elseif sy-index = 3.
           assign lt_desc_cha to <table>.
           ls_par-kind = abap_func_changing.
         endif.
         loop at <table> assigning <row>.
           assign component 'PARAMETER' of structure <row> to <field>.
           ls_par-name = <field>.
           assign component 'DBFIELD' of structure <row> to <field>.
           if <field> is not initial.
             create data ls_par-value type (<field>).
           else.
             assign component 'TYP' of structure <row> to <field>.
             if <field> is not initial.
               create data ls_par-value type (<field>).
             else.
               create data ls_par-value type text100. "Fallback for untyped parameters
             endif.
           endif.
           insert ls_par into table function->parameters.
           insert ls_par into table function->paramact.
         endloop.
       enddo.

       result = function.

     endmethod.                    "proc_abap_function
**********************************************************************
     method proc_abap_table. "Create a table data
* First input: name of data type, second input: value
       create object result
         exporting
           type = lcl_lisp_element=>type_abap_table.
       create data result->data type table of (list->car->value).
* Set value if supplied as second parameter
       if list->cdr ne nil.
         call method element_to_data
           exporting
             element = list->cdr->car
           changing
             data    = result->data.
       endif.
     endmethod.                    "proc_abap_table
**********************************************************************
     method proc_abap_append_row.
     endmethod.                    "proc_abap_append_row
**********************************************************************
     method proc_abap_delete_row.
     endmethod.                    "proc_abap_delete_row
**********************************************************************
     method proc_abap_get_row.

     endmethod.                    "proc_abap_get_row
**********************************************************************
     method proc_abap_get_value. "Convert ABAP to Lisp data
       data: lx_root type ref to cx_root.
       field-symbols: <data> type any.
       if list->car->type ne lcl_lisp_element=>type_abap_data and
          list->car->type ne lcl_lisp_element=>type_abap_table.
         eval_err( |AB-GET-VALUE requires ABAP data or table as parameter| ).
       endif.
       try.
           assign list->car->data->* to <data>.
           result = data_to_element( <data> ).
         catch cx_root into lx_root.
           eval_err( |Mapping error: { lx_root->get_text( ) }| ).
       endtry.
     endmethod.                    "proc_abap_get_value
**********************************************************************
     method proc_abap_set_value. "Convert Lisp to ABAP data
       data: lx_root type ref to cx_root.
       field-symbols: <data> type any.
       if list->car->type ne lcl_lisp_element=>type_abap_data and
          list->car->type ne lcl_lisp_element=>type_abap_table.
         eval_err( |AB-SET-VALUE requires ABAP data or table as first parameter| ).
       endif.
       try.
           assign list->car->data->* to <data>.
           call method element_to_data
             exporting
               element = list->cdr->car
             changing
               data    = <data>.
         catch cx_root into lx_root.
           eval_err( |Mapping error: { lx_root->get_text( ) }| ).
       endtry.
       result = nil. "TODO: What should we return here?
     endmethod.                    "proc_abap_set_value
**********************************************************************
     method proc_abap_function_call. "Called internally only for execution of function module

       data: lr_func type ref to lcl_lisp_abapfunction.

* The first parameter must be a function module instance
       if list->car->type ne lcl_lisp_element=>type_abap_function.
         eval_err( |{ list->car->value } is not a function module reference| ).
       endif.
       lr_func ?= list->car.

* TODO: Map given list to parameters of function module

* First parameter: Name of function to call; second parameter: data to pass to interface
       call function list->car->value
         parameter-table lr_func->paramact
         exception-table lr_func->exceptions.

* Map output parameters to new list
       result = list->car. "Function reference is updated with values after call

     endmethod.                    "proc_abap_function_call

     method proc_abap_get.
       data: lr_data type ref to data.
       data: lr_ddesc type ref to cl_abap_typedescr.
       field-symbols <value> type any.

* Ensure a valid first parameter is passed
       if list->car->type ne lcl_lisp_element=>type_abap_data and
          list->car->type ne lcl_lisp_element=>type_abap_function and
          list->car->type ne lcl_lisp_element=>type_abap_table.
         eval_err( 'AB-GET: First parameter must be ABAP data or table or a function' ).
       endif.

* Determine whether the data is elementary or not to decide if we need to get the element by identifier
       if list->car->data is not initial and cl_abap_typedescr=>describe_by_data_ref( list->car->data )->kind = cl_abap_typedescr=>kind_elem.
* Elementary type; can return the value without mapping
         lr_data = list->car->data.
* Could short-cut here and provide the value right away
       else.
         if list->cdr = nil.
           eval_err( 'AB-GET: Complex type requires identifier for lookup' ).
         else.
           lr_data = get_element( element = list->car identifier = list->cdr->car ).
         endif.
       endif.

* Perform RTTI on determined data and generate appropriate response
       assign lr_data->* to <value>.
       lr_ddesc = cl_abap_typedescr=>describe_by_data( <value> ).
       case lr_ddesc->kind.
         when cl_abap_typedescr=>kind_table.
           create object result
             exporting
               type = lcl_lisp_element=>type_abap_table.
           result->data = lr_data.
         when cl_abap_typedescr=>kind_struct.
           create object result
             exporting
               type = lcl_lisp_element=>type_abap_data.
           result->data = lr_data.
         when cl_abap_typedescr=>kind_elem.
* Give back immediate value
           result = data_to_element( <value> ).
         when others.
           eval_err( |AB-GET: Type kind { lr_ddesc->kind } not supported yet| ). "Can do AB-TAB-WHERE some other time
       endcase.

     endmethod.                    "proc_abap_get

     method proc_abap_set.

       data: lr_tdata type ref to data. "Target data
       data: lr_selem type ref to lcl_lisp_element. "Source element
       data: lr_ddesc type ref to cl_abap_typedescr.
       field-symbols <target> type any.
       field-symbols <source> type any.

* Ensure a valid first parameter is passed
       if list->car->type ne lcl_lisp_element=>type_abap_data and
          list->car->type ne lcl_lisp_element=>type_abap_function and
          list->car->type ne lcl_lisp_element=>type_abap_table.
         eval_err( 'AB-SET: First parameter must be ABAP data or table or a function' ).
       endif.

* Determine whether the data is elementary or not to decide if we need to get the element by identifier
       if list->car->data is not initial and cl_abap_typedescr=>describe_by_data_ref( list->car->data )->kind = cl_abap_typedescr=>kind_elem.
* Elementary type; can return the value without mapping
         lr_tdata = list->car->data.
         lr_selem = list->cdr->car.
*         lr_sdata = list->cdr->car->data.       "Value to set is second argument
       else.
         if list->cdr = nil.
           eval_err( 'AB-GET: Complex type requires identifier for lookup' ).
         else.
           lr_tdata = get_element( element = list->car identifier = list->cdr->car ).
*           lr_sdata = list->cdr->cdr->car->data. "Value to set is third argument
           lr_selem = list->cdr->cdr->car.
         endif.
       endif.

* Do we just assign the reference now? Probably should dereference source value
* and copy the value...
* Perform RTTI on determined data and generate appropriate response
       assign lr_tdata->* to <target>.

       lr_ddesc = cl_abap_typedescr=>describe_by_data( <target> ).
* For elementary types, set value from second parameter, otherwise third
       if lr_ddesc->kind = cl_abap_typedescr=>kind_elem.
* For now, we will support setting data from a number, string or symbol
         case lr_selem->type.
           when lcl_lisp_element=>type_string or lcl_lisp_element=>type_string.
             <target> = lr_selem->value.
           when lcl_lisp_element=>type_number.
             <target> = lr_selem->number.
         endcase.
       else.
* Complex types will just copy the whole value across
         assign lr_selem->data->* to <source>.
         <target> = <source>. "Set the value
       endif.

       result = nil.

     endmethod.                    "proc_abap_set

*--------------------------------------------------------------------*
* Map ABAP Data to Lisp element
     method data_to_element.
* RTTI-relevant:
       data: lr_ddesc type ref to cl_abap_typedescr.

* ABAP-side (source) mapping:
       field-symbols: <field> type any.
       field-symbols: <line> type any.
       field-symbols: <table> type any table.
       field-symbols: <sotab> type sorted table.
       field-symbols: <sttab> type standard table.
       data: field type ref to data.
       data: line type ref to data.
       data: table type ref to data.
* Lisp-side (target) mapping:
       data: lr_conscell type ref to lcl_lisp_element.

* Determine type of the ABAP value
       lr_ddesc = cl_abap_typedescr=>describe_by_data( data ).
       case lr_ddesc->kind.

* Table type
         when cl_abap_typedescr=>kind_table.
           assign data to <table>.
           create data line like line of <table>.
           assign line->* to <line>.

           if <table> is initial.
             element = nil.
           else.
* Create list with cell for each row
             create object element
               exporting
                 type = lcl_lisp_element=>type_conscell.
             lr_conscell = element. "Set pointer to start of list
             loop at <table> into <line>.
               if sy-tabix > 1. "Move pointer only from second line onward
                 create object lr_conscell->cdr
                   exporting
                     type = lcl_lisp_element=>type_conscell.
                 lr_conscell = lr_conscell->cdr.
               endif.
               lr_conscell->car = data_to_element( <line> ).
             endloop.
             lr_conscell->cdr = nil. "Terminate list
           endif.

* Structure
         when cl_abap_typedescr=>kind_struct.
           create object element
             exporting
               type = lcl_lisp_element=>type_conscell.
           lr_conscell = element.
           do.
             assign component sy-index of structure data to <field>.
             if sy-subrc ne 0.
               lr_conscell->cdr = nil. "Terminate list
               exit.
             endif.
             if sy-index > 1. "Move pointer only from second field onward
               create object lr_conscell->cdr
                 exporting
                   type = lcl_lisp_element=>type_conscell.
               lr_conscell = lr_conscell->cdr.
             endif.
             lr_conscell->car = data_to_element( <field> ).
           enddo.

* Elementary type
         when cl_abap_typedescr=>kind_elem.
           if lr_ddesc->type_kind = cl_abap_typedescr=>typekind_numeric or
              lr_ddesc->type_kind = cl_abap_typedescr=>typekind_num.
             create object element
               exporting
                 type = lcl_lisp_element=>type_number.
             element->number = data.
           else.
             create object element
               exporting
                 type = lcl_lisp_element=>type_string.
             element->value = data.
           endif.
       endcase.
     endmethod.                    "data_to_element
*--------------------------------------------------------------------*
* Map Lisp element to ABAP Data
     method element_to_data.
* RTTI-relevant:
       data: lr_ddesc type ref to cl_abap_typedescr.
       data: lr_tdesc type ref to cl_abap_tabledescr.
* ABAP-side (target) mapping:
       field-symbols: <field> type any.
       field-symbols: <line> type any.
       field-symbols: <table> type any table.
       field-symbols: <sotab> type sorted table.
       field-symbols: <sttab> type standard table.
       data: field type ref to data.
       data: line type ref to data.
       data: table type ref to data.
* Lisp-side (source) mapping:
       data: lr_conscell type ref to lcl_lisp_element.

* Determine type of the ABAP value
       lr_ddesc = cl_abap_typedescr=>describe_by_data( data ).
       case lr_ddesc->kind.

* Table type
         when cl_abap_typedescr=>kind_table.
* For this mapping to happen, the element must be a cons cell
           if element->type ne lcl_lisp_element=>type_conscell.
             eval_err( 'Mapping failed: Non-cell to table' ).
           endif.
* Provide reference to table and line
           lr_tdesc ?= lr_ddesc.
           get reference of data into table.
           assign table->* to <table>.
           if lr_tdesc->table_kind = cl_abap_tabledescr=>tablekind_sorted or
              lr_tdesc->table_kind = cl_abap_tabledescr=>tablekind_hashed.
             assign table->* to <sotab>. "Sorted table type
             create data line like line of <sotab>.
             assign line->* to <line>.
           else.
             assign table->* to <sttab>. "Standard table type
             create data line like line of <sttab>.
             assign line->* to <line>.
           endif.

           lr_conscell = element. "Set pointer to start of list
           do.
             call method element_to_data
               exporting
                 element = lr_conscell->car
               changing
                 data    = <line>.
* Append or insert, depending on table type (what is assigned)
             if <sotab> is assigned.
               insert <line> into table <sotab>.
             else.
               append <line> to <sttab>.
             endif.
             clear <line>.
             lr_conscell = lr_conscell->cdr.
             if lr_conscell = nil.
               exit.
             endif.
           enddo.

* Structure
         when cl_abap_typedescr=>kind_struct.
           if element->type ne lcl_lisp_element=>type_conscell.
             eval_err( 'Mapping failed: Non-cell to structure' ).
           endif.

           lr_conscell = element. "Set pointer to start of list
           assign data to <line>.
           do.
             assign component sy-index of structure <line> to <field>.
             if sy-subrc ne 0.
               exit.
             endif.

             if sy-index > 1. "Move cons cell pointer only from second element on
               lr_conscell = lr_conscell->cdr.
             endif.
* Don't map nil values
             if lr_conscell->car = nil.
               continue.
             endif.

             call method element_to_data
               exporting
                 element = lr_conscell->car
               changing
                 data    = <field>.
           enddo.

* Elementary type
         when cl_abap_typedescr=>kind_elem.
           assign data to <field>.
           if element->type = lcl_lisp_element=>type_number.
             <field> = element->number.
           else.
             <field> = element->value.
           endif.
         when others.
* Not supported yet
           eval_err( |Mapping failed: unsupported type| ).
       endcase.
     endmethod.                    "element_to_data

*---- METHOD GET_ELEMENT
* ELEMENT    -> Lisp element containing an ABAP value (data, table or function)
* IDENTIFIER -> Lisp element, string or symbol or index, to identify subcomponent of value
* RDATA      <- Data reference to value pointed to
     method get_element.
       data: lr_ddesc type ref to cl_abap_typedescr.
       data: lr_tdesc type ref to cl_abap_tabledescr.
       data: lr_func  type ref to lcl_lisp_abapfunction.
       field-symbols: <sttab> type standard table.
       field-symbols: <sotab> type sorted table.
       field-symbols: <data> type ref to data.
       field-symbols: <value> type any.

* Get function parameter by name
       if element->type = lcl_lisp_element=>type_abap_function.
         if identifier = nil or ( identifier->type ne lcl_lisp_element=>type_string and
                                 identifier->type ne lcl_lisp_element=>type_symbol ).
           eval_err( 'AB-GET: String or symbol required to access function parameter' ).
         endif.
         data: ls_param type abap_func_parmbind.
         lr_func ?= element.
         read table lr_func->paramact into ls_param with key name = identifier->value.
         if sy-subrc ne 0.
* Return blank parameter if parameter has not been set
           read table lr_func->parameters into ls_param with key name = identifier->value.
           if sy-subrc ne 0.
             eval_err( |AB-GET: No parameter { identifier->value } in function| ).
           endif.
         endif.
         rdata = ls_param-value.

       else.
* First parameter is not function, but table or other data; examine the data
         assign element->data->* to <value>.
         lr_ddesc = cl_abap_typedescr=>describe_by_data( <value> ).

* Structure: Use second parameter as field name
         if lr_ddesc->kind = cl_abap_typedescr=>kind_struct.
           if identifier = nil or ( identifier->type ne lcl_lisp_element=>type_string and
                                    identifier->type ne lcl_lisp_element=>type_symbol ).
             eval_err( 'AB-GET: String or symbol required to access structure field' ).
           endif.
           assign component identifier->value of structure <value> to <value>.
           if sy-subrc ne 0.
             eval_err( |AB-GET: Structure has no component { identifier->value }| ).
           endif.
           get reference of <value> into rdata.

* Elementary data: No qualifier / second parameter required
         elseif lr_ddesc->kind = cl_abap_typedescr=>kind_elem.
           rdata = element->data.

* Table: Second parameter is index (std table) or key (sorted table)
         elseif lr_ddesc->kind = cl_abap_typedescr=>kind_table.
           lr_tdesc ?= lr_ddesc. "cl_abap_typedescr=>describe_by_data( element->data ).
           if lr_tdesc->table_kind = cl_abap_tabledescr=>tablekind_sorted or
              lr_tdesc->table_kind = cl_abap_tabledescr=>tablekind_hashed.
* TODO: Read with key, which is a bit more effort
           elseif lr_tdesc->table_kind = cl_abap_tabledescr=>tablekind_std.
* Second input for reading a standard table must be a number (row index)
             if identifier = nil or identifier->type ne lcl_lisp_element=>type_number.
               eval_err( 'AB-GET: Numeric index required to read standard table' ). "Can do AB-TAB-WHERE some other time
             endif.
             assign element->data->* to <sttab>.
             read table <sttab> reference into rdata index identifier->number.
             if sy-subrc ne 0.
               eval_err( |AB-GET: No entry at index { identifier->number }| ). "Can do AB-TAB-WHERE some other time
             endif.
           endif.

         endif.

       endif.
     endmethod.                    "get_element

   endclass.                    "lcl_lisp_interpreter IMPLEMENTATION