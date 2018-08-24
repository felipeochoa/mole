;;; mole-js --- js2 parser implementation using mole  -*- lexical-binding: t -*-

;;; Commentary:

;;; This example file implements most of the js2 parser, excluding
;;; support for older language versions, E4X, and spurious Rhino
;;; extensions

;;; Code:

(require 'mole)

(defvar mole-js-errors)

(defvar mole-js-grammar
  (mole-create-grammar
   ;; helper productions
   (init :lexical t :fuse t (extern (lambda () (setq mole-js-errors nil) (mole-node 'literal nil nil))))
   (error :params (msg) (extern (lambda (msg)
                                  (push msg mole-js-errors)
                                  (mole-node 'literal nil nil (point) (point)))
                                msg))
   (must-not-match :params (prod msg) (or (! prod) (error msg)))
   (must-match :pass-thru t :params (prod msg) (or prod (error msg)))
   ;; lexical productions
   :lexical t :fuse t
   (eof (extern (lambda () (if (eobp) (mole-node 'literal nil t) 'fail))))
   (any (extern (lambda ()
                  (if (eobp) 'fail
                    (forward-char)
                    (mole-node 'literal nil t (1- (point)) (point))))))
   (unicode-category :params (categories)
                     (extern (lambda (categories)
                               (cond
                                ((eobp) 'fail)
                                ((memq (get-char-code-property (char-after) 'general-category) categories)
                                 (forward-char)
                                 (mole-node 'literal nil nil (1- (point)) (point)))
                                (t 'fail)))
                             categories))
   (assign-operator (or "=" "|=" "^=" "&=" "<<=" ">>=" ">>>=" "+=" "-=" "*=" "/=" "%=" "**="))
   (reserved (or `class `enum `export `extends `import `static `super))
   (keyword (or `break
                `case `catch `class `const `continue
                `debugger `default `delete `do
                `else `extends `export
                `false `finally `for `function
                `if `in `instanceof `import
                `let
                `new `null
                `return
                `super `switch
                `this `throw `true `try `typeof
                `var `void
                `while `with
                `yield))
   (comment (extern (lambda ()
                      (let ((beg (point)))
                        (if (forward-comment 1)
                            (mole-node 'literal nil t beg)
                          (mole-update-highwater-mark (point))
                          (goto-char beg)
                          'fail)))))
   (line-terminators (char "\r\n\u2028\u2029"))
   (whitespace-no-eol (or (char " \t\v\f\xa0") (unicode-category '(Zs)) comment))
   (whitespace (* (+ whitespace-no-eol) (\? line-terminators)))

   (hex-escape-sequence "\\x" (2 (char hex-digit)))
   (unicode-escape-sequence "\\u" (or (4 (char hex-digit))
                                      (: "{" (+ (char hex-digit)) "}")
                                      (: (0 4 any) (error "msg.invalid.escape"))))
   (identifier-start (or (char "$_") unicode-escape-sequence
                         ;; Letters:
                         (unicode-category '(Lu Ll Lt Lm Lo Nl))))
   ;; TODO: What's the difference between an identifier and a name?
   (identifier-part (or (char "$_\u200c\u200d") unicode-escape-sequence
                        ;; Letters, combining marks, digits, or connector punctuation:
                        (unicode-category '(Lu Ll Lt Lm Lo Nl Mn Mc Nd Pc))
                        (: "\\" (error "msg.illegal.character"))))
   (identifier-name identifier-start (* identifier-part))
   (name (! (or keyword reserved)) identifier-name) ; Identifier in the ES 262 grammar
   (number (or decimal-literal binary-integer-literal octal-integer-literal
               hex-integer-literal)
           (\? (or identifier-start (char "0-9")) (error "msg.invalid.numeric.literal")))
   (decimal-literal (or (: "." (+ (char "0-9")) (\? exponent-part))
                        (: (or "0" (: (char "1-9") (* (char "0-9"))))
                           (\? (or (: "." (* (char "0-9")) (\? exponent-part))
                                   exponent-part)))))
   (exponent-part (\? (char "eE") (\? (char "+-")) (+ (char "0-9"))))
   (binary-integer-literal "0" (char "bB") (+ (char "01")))
   (octal-integer-literal "0" (char "oO") (+ (char "0-7")))
   (hex-integer-literal "0" (char "hH") (+ (char "0-9a-fA-F")))
   (string (or (: "'" single-string-characters "'")
               (: "\"" double-string-characters "\"")))
   (single-string-characters (* (or (char-not "\'\\")
                                    unicode-escape-sequence
                                    hex-escape-sequence
                                    (: "\\" any))))
   (double-string-characters (* (or (char-not "\"\\")
                                    unicode-escape-sequence
                                    hex-escape-sequence
                                    (: "\\" any))))
   (regexp "/"
           (or (char-not "*\\/\[\n\r\u2028\u2029")
               regular-expr-backslash-sequence
               regular-expr-class)
           (* (or (char-not "\\/\[\n\r\u2028\u2029")
                  regular-expr-backslash-sequence
                  regular-expr-class))
           "/"
           (* (error unicode-escape-sequence "XXX") identifier-part))
   (regular-expr-backslash-sequence "\\" (char-not "\n\r\u2028\u2029"))
   (regular-expr-class "[" (* (or (char-not "\]\\")
                                  regular-expr-backslash-sequence))
                       "]")
   (template-literal "`" (* template-body-unit)
                     (* "$\{" (or (must-not-match "\}" "msg.XXX")
                                  (: expr (must-match "}" "msg.XXX")))
                        (* template-body-unit))
                     "`")
   (template-body-unit (or (char-not "$`\\")
                           (: (char ?\\) any)
                           (: (char "$") (or (char-not "\{\\`") (: (char ?\\) any)))))

   :lexical nil :fuse nil
   ;; Main parse productions
   (program init (or strict-program (* (! eof) statement)))
   (strict-program use-strict-directive (with-context (use-strict t) (* (! eof) statement)))
   (statement
    (or async-function-stmt
        (: (! `async)
           (or (: (or break const-var let-expr continue debugger import labeled-stmt return
                      yield-expr throw expr)
                  auto-semi-insert)
               ;; These are the ones with no automatic semi insertion:
               ";"
               (with-context (not-top-level t)
                             if switch while do for try with block
                             class-stmt function-stmt export)))
        (: (error "msg.syntax") any)))
   ;; Level 1 productions
   (use-strict-directive (or "'use strict';" "\"use strict\";"))
   (break `break (\? name))
   (const-var (or `const `var) (must-match variables "msg.invalid.variables"))
   (let-expr `let (must-match variables "msg.invalid.variables"))
   (continue `continue (\? name))
   (debugger `debugger)
   (import `import
           (\? (if-context (not-top-level t) (error "msg.mod.import.decl.at.top.level")))
           (or string (: import-clause from-clause)))
   (labeled-stmt (+ name ":") statement)
   (return `return (\? (if-context (in-function nil) (error "msg.bad.return")))
           expr)
   (yield-expr `yield (\? "*") (\? (if-context (in-function nil) (error "msg.bad.yield")))
               (or (if-context (star-p nil) (error "msg.syntax"))
                   assign-expr))
   ;; TODO: next line needs to be lexical but expr needs to be syntactic
   (throw (lexical `throw (* whitespace-no-eol) expr))
   (expr assign-expr (* "," assign-expr))
   (if `if condition statement (\? `else statement))
   (switch `switch
           (must-match "(" "msg.no.paren.switch") expr (must-match ")" "msg.no.paren.after.switch")
           (must-match "{" "msg.no.brace.switch")
           (* case-node))               ; TODO: error if multiple defaults
   (while `while condition statement)
   (do `do statement (must-match `while "msg.no.while.do") condition)
   (for (must-match "(" "msg.no.paren.for")
        (or (: (or `var `let `const) variables) (= ";") expr)
        (or (: (or `in `of) expr)     ; TODO: handle 'in obj' getting eaten by expr above.
                                        ; TODO: check for msg.mult.index error
            (: (must-match ";" "msg.no.semi.for")
               (\? expr)
               (must-match ";" "msg.no.semi.for.cond")
               (\? expr)))
        (must-match ")" "msg.no.paren.for.ctrl")
        statement)
   (try `try (or (= "{") (error "msg.no.brace.try"))
        statement
        (or finally (: catch (\? finally)) (error "msg.try.no.catchfinally")))
   (with `with
         (or (if-context (use-strict t) "msg.no.with.strict")
             (: (must-match "(" "msg.no.paren.with") expr (must-match ")" "msg.no.paren.after.with")
                statement)))
   (block "{" (* statement) (must-match "}" "msg.no.brace.block"))
   (class-stmt `class (must-match name "msg.unnamed.class.stmt") class)
   (function-stmt `function
                  (or (: "*" (must-match name "msg.unnamed.function.stmt")
                         (must-match "(" "msg.no.paren.params")
                         (with-context (star-p t) function))
                      (: (must-match name "msg.unnamed.function.stmt")
                         (must-match "(" "msg.no.paren.params")
                         (with-context (star-p nil) function))))
   (async-function-stmt `async `function
                        (or (: "*" (must-match name "msg.unnamed.function.stmt")
                               (must-match "(" "msg.no.paren.params")
                               (with-context (async-p t) (with-context (star-p t) function)))
                            (: (must-match name "msg.unnamed.function.stmt")
                               (must-match "(" "msg.no.paren.params")
                               (with-context (async-p t) (with-context (star-p nil) function)))))
   (export `export
           (\? (if-context (not-top-level t) (error "msg.mod.export.decl.at.top.level")))
           (or class-stmt async-function-stmt function-stmt
               (: (or (: "*" from-clause)
                      (: "{" export-bindings (\? from-clause))
                      (: `default (or class-stmt class-expr async-function-stmt async-function-expr
                                       function-stmt function-expr
                                       (: (or name expr) auto-semi-insert)))
                      (: (or `var `const `let) variables)
                      name expr)
                  auto-semi-insert)))
   ;; level 2
   (auto-semi-insert (\? ";"))
   (variables variable (* "," variable))
   (import-clause (or (: "*" namespace-import)
                      (: "{" export-bindings)
                      (: export-binding (\? "," (or (: "*" namespace-import)
                                                    (: "{" export-bindings)
                                                    (error "msg.syntax"))))
                      (error "msg.mode.declaration.after.import")))
   (from-clause `from (or string (: `this `module)))
   (assign-expr (or yield-expr async-arrow arrow
                    (: cond-expr (\? assign-operator assign-expr))))
   (condition (must-match "(" "msg.no.paren.cond") expr (must-match ")" "msg.no.paren.after.cond"))
   (case-node (or (: `case expr (must-match ":" "msg.no.colon.case"))
                  (: `default (must-match ":" "msg.no.colo.case")))
              (* (! (or `case `default eof) statement)))
   (catch `catch (must-match "(" "msg.no.paren.catch")
          (or (: (char "\{\[") destruct-primary-expr)
              (must-match name "msg.bad.catchcond"))
          (must-match ")" "msg.bad.catchcond")
          (must-match "{" "msg.no.brace.catchblock")
          (* statement)
          (must-match "}" "msg.no.brace.after.body"))
   (finally `finally statement)
   (function (with-context (in-function t)
                           (or (if-context (use-strict t) strict-function-params function-body)
                               (if-context (use-strict nil)
                                           (or (: function-params function-body-no-strict)
                                               (: strict-function-params function-body-with-strict))))))
   ;; TODO: allow customizing error message for imports
   (export-bindings export-binding (* "," export-binding) (must-match "}" "msg.mod.rc.after.export.spec.list"))
   (export-binding (or (: identifier-name `as (or `default name))
                       name
                       (: identifier-name (error "msg.mod.as.after.reserved.word"))))
   ;; level 3
   (async-arrow `async (or name (: "(" function-params)) "=>"
                (or (: (= "{") (with-context (async-p t) statement)) expr))
   (arrow (or name (: "\(" function-params)) "=>"
          (or (: (= "{") (with-context (async-p nil) statement)) expr))
   (namespace-import (or (: `as name)))
   (variable (or (: (= (char "\{\[")) destruct-primary-expr
                    (must-match (: "=" assign-expr) "msg.destruct.assign.no.init"))
                 (: (must-match name '"msg.bad.var") (\? "=" (must-match assign-expr '"msg.syntax")))))
   (destruct-primary-expr (with-context (destructoring t) primary-expr))
   (function-params (or "\)"
                        (* (or (: (or name (: (char "\[\{") destruct-primary-expr))
                                  (\? "=" assign-expr))
                               ;; TODO report "msg.param.after.rest"
                               (: (\? "...") name)
                               (error "msg.no.paren.after.parms"))))) ; TODO: ensure rest param is final arg
   (strict-function-params
    (with-context (use-strict t)
                  ;; duplicated body to avoid thrashing cache in
                  ;; strict-mode functions
                  (or "\)"
                      (* (or (: (or name (: (char "\[\{") destruct-primary-expr))
                                (\? "=" assign-expr))
                             (: (\? "...") name)
                             (error "msg.no.parm"))))))
   (function-body (must-match "{" "msg.no.brace.body") (* statement)
                  (must-match "}" "msg.no.brace.after.body"))
   (function-body-with-strict (must-match "{" "msg.no.brace.body")
                              use-strict-directive (with-context (use-strict t) (* statement))
                              (must-match "}" "msg.no.brace.after.body"))
   (function-body-no-strict (must-match "{" "msg.no.brace.body")
                            (! use-strict-directive) (* statement)
                            (must-match "}" "msg.no.brace.after.body"))

   ;; Expressions in js2 pass-thru their children
   :pass-thru t
   (cond-expr or-expr (\? "?" assign-expr (must-match ":") assign-expr))
   (or-expr and-expr (\? "||" or-expr))
   (and-expr bit-or-expr (\? "&&" and-expr))
   (bit-or-expr bit-xor-expr (\? "|" bit-or-expr))
   (bit-xor-expr bit-and-expr (\? "^" bit-xor-expr))
   (bit-and-expr eq-expr (\? "&" bit-and-expr))
   (eq-expr rel-expr (\? (or "!==" "!=" "===" "==") rel-expr))
   (rel-expr shift-expr (\? (or `instanceof `in "<=" "=>" "<" ">") shift-expr))
   (shift-expr add-expr (\? (or "<<<" ">>>") add-expr))
   (add-expr mul-expr (\? (char "+-") mul-expr))
   (mul-expr expon-expr (\? (char "*/%") expon-expr))
   (expon-expr unary-expr (\? "**" expon-expr)) ; right-associative
   :pass-thru nil
   (unary-expr (or (: (or (char "!~+-") `void `typeof `delete) unary-expr)
                   ;; TODO: next line needs to be lexical but member-expr needs to be syntactic
                   (: (or "--" "++") (* whitespace-no-eol) member-expr-with-call)
                   await
                   ;; TODO: next line needs to be lexical but member-expr needs to be syntactic
                   (: member-expr-with-call (\? (* whitespace-no-eol) (or "++" "--")))))
   (argument-list (or "\)"
                      (: (\? "...") assign-expr
                         (* "," (\? "...") assign-expr)
                         (\? ",")
                         (must-match ")" "msg.no.paren.arg"))))
   (member-expr-with-call :pass-thru t member-expr-head (* member-expr-tail-with-call))
   (member-expr-head (or (: `new member-expr (\? "\(" argument-list)) primary-expr))
   (member-expr-tail-with-call (or (: "." property-access) (: "\[" element-get) function-call template-literal))
   (member-expr member-expr-head (* member-expr-tail))
   (member-expr-tail (or (: "." property-access) (: "\[" element-get) template-literal))
   (primary-expr (or (: `class class-expr)
                     (: `function function-expr)
                     (: `async `function (with-context (async-p t) function-expr))
                     (: "\[" array-literal)
                     (: "{" object-literal)
                     (: `let let-expr)
                     (: "\(" paren-expr)
                     keyword-lit name number string regexp
                     (: "..." name (\?= "\)" "=>")) ;; TODO: why?
                     reserved
                     (: eof (error "msg.unexpected.eof"))))
   (class-expr (\? name) class)
   (class (\? `extends (or (: (\?= "{") (error "msg.missing.extends"))
                            assign-expr
                            (error "msg.bad.extends")))
          (must-match "{" "msg.no.brace.class")
          (* class-elem)
          (must-match "}" "msg.no.brace.prop"))
   (class-elem (\? `static) (\? "*")
               (\? (or `get `set `async) (= (or name string number "[")))
               (= (or name string number "["))
               named-class-prop
               (\? "," (error "msg.class.unexpected.comma"))
               (\? ";"))
   (named-class-prop (must-match
                      (or string
                          (: "\[" assign-expr (must-match "\]" "msg.missing.computed.rb"))
                          number
                          name)
                      "msg.bad.prop"))
   (function-expr (or (: "*" (\? name) (must-match "(" "msg.no.paren.params")
                         (with-context (star-p t) function))
                      (: (\? name) (must-match "(" "msg.no.paren.params")
                         (with-context (star-p nil) function))))
   (async-function-expr `async `function
                        (or (: "*" (\? name) (must-match "(" "msg.no.paren.params")
                               (with-context (async-p t) (with-context (star-p t) function)))
                            (: (\? name) (must-match "(" "msg.no.paren.params")
                               (with-context (async-p t) (with-context (star-p nil) function)))))
   (array-literal (* ",")
                  (\? (or (: "..." assign-expr) assign-expr))
                  (* (+ ",") (or (: "..." assign-expr) assign-expr))
                  (* ",")
                  ;; TODO: error if spread is not final arg in destructuring
                  (or "\]" (: eof (error "msg.no.bracket.arg"))))
   (object-literal (* (or (: (or "*" `get `set)
                             (or prop-name (error "msg.bad.prop"))
                             method-prop)
                          (: prop-name method-prop)
                          (: prop-name "="
                             (if-context (destructuring nil) (error "msg.init.no.destruct"))
                             assign-expr)
                          (: prop-name plain-property)))
                   (or "\}" (: eof (error "msg.no.bracket.arg"))))
   (prop-name (or string number name
                  (: "[" assign-expr (must-match "]" "msg.missing.computed.rb"))))
   (method-prop (must-match "\(" "msg.no.paren.parms") function)
   (plain-property (or (= (or "," "\}")) (must-match ":" "msg.no.colon.prop")))
   (paren-expr "\(" expr (must-match "\)" "msg.no.paren"))
   (keyword-lit (or `null `this `super `false `true))
   (await `await unary-expr)
   (property-access (must-match (or keyword reserved name) "msg.no.name.after.dot"))
   (element-get expr (must-match "\]" "msg.no.bracket.index"))
   (function-call "\(" argument-list)))

(provide 'mole-js)
;;; mole-js ends here

;; (font-lock-add-keywords nil mole-symbol-font-lock-kw t)

;; Local Variables:
;; mole-symbol-font-lock-kw: (("`\\sw+" . font-lock-constant-face))
;; End:
