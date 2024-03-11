#!/usr/bin/env -S guile -e '(lambda _ (with-output-to-file "lily58.keymap" main))' -s
!#

;;;
;;; Copyright (c) 2024 antlers <antlers@illucid.net>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;;

(use-modules (ice-9 regex)
             (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-26)
             (srfi srfi-171))

(define layers
  '(((base-layer . base)
     ((_         ) (kp N7     ) (kp N8     ) (kp N9     ) (kp N0     ) (kp N5     )                       (kp N6     ) (kp N1     ) (kp N2     ) (kp  N3    ) (kp N4     ) (X         )
      (to BASE   ) (kp V      ) (kp M      ) (kp L      ) (kp C      ) (kp P      )                       (kp B      ) (key_repeat) (kp U      ) (kp O      ) (kp Q      ) (X         )
      (to BASE   ) (kp S      ) (kp T      ) (kp R      ) (kp D      ) (kp Y      )                       (kp F      ) (kp N      ) (kp E      ) (kp A      ) (kp I      ) (kp Q      )
      (X         ) (kp X      ) (kp K      ) (kp J      ) (kp G      ) (kp W      ) (cap_word) (cap_word) (kp Z      ) (kp H      ) (kp COMMA  ) (kp DOT    ) (kp SQT    ) (X         )
                                             (X         ) (mo NUM    ) (mo EXT    ) (mo EXT  ) (kp SPC  ) (kp SPC    ) (X         ) (X         )
      ))
    ((symbol-layer . sym)
     ((_         ) (_         ) (_         ) (_         ) (_         ) (_         )                       (_         ) (_         ) (_         ) (_         ) (_         ) (_         )
      (LAYER_LOCK) (_         ) (_         ) (_         ) (_         ) (_         )                       (kp CARET  ) (kp RBKT   ) (kp LBKT   ) (kp PERCENT) (kp DOLLAR ) (_         )
      (_         ) (sk LALT   ) (sk LMETA  ) (sk LSHIFT ) (sk LCTRL  ) (_         )                       (kp SEMI   ) (kp RPAR   ) (kp LPAR   ) (_         ) (_         ) (_         )
      (_         ) (_         ) (_         ) (_         ) (_         ) (_         ) (_       ) (_       ) (_         ) (kp RBRC   ) (kp LBRC   ) (_         ) (_         ) (_         )
                                             (_         ) (_         ) (_         ) (_       ) (_       ) (_         ) (_         ) (_         )
      ))
    ((extend-layer . ext) ; ie. nav
     ((_         ) (_         ) (_         ) (_         ) (_         ) (_         )                       (_         ) (_         ) (_         ) (_         ) (_         ) (_         )
      (LAYER_LOCK) (_         ) (_         ) (_         ) (_         ) (_         )                       (kp END    ) (_         ) (kp PG_DN  ) (kp PG_UP  ) (_         ) (_         )
      (_         ) (sk LALT   ) (sk LMETA  ) (sk LSHIFT ) (sk LCTRL  ) (_         )                       (kp HOME   ) (kp LEFT   ) (kp DOWN   ) (kp UP     ) (kp RIGHT  ) (_         )
      (_         ) (_         ) (_         ) (_         ) (_         ) (_         ) (_       ) (_       ) (kp DELETE ) (kp BKSP   ) (kp RET    ) (kp TAB    ) (_         ) (_         )
                                             (_         ) (_         ) (_         ) (_       ) (kp BKSP ) (kp BKSP   ) (_         ) (_         )
      ))
    ((function-layer . fun)
     ((_         ) (kp F7     ) (kp F8     ) (kp F9     ) (kp F10    ) (kp F5     )                       (kp F6     ) (kp F1     ) (kp F2     ) (kp F3     ) (kp F4     ) (_         )
      (LAYER_LOCK) (_         ) (_         ) (to BASE   ) (to QRT    ) (_         )                       (kp F12    ) (kp F7     ) (kp F8     ) (kp F9     ) (_         ) (_         )
      (_         ) (sk LALT   ) (sk LMETA  ) (sk LSHIFT ) (sk LCTRL  ) (_         )                       (kp F11    ) (kp F4     ) (kp F5     ) (kp F6     ) (_         ) (_         )
      (_         ) (_         ) (_         ) (_         ) (_         ) (_         ) (_       ) (_       ) (kp F10    ) (kp F1     ) (kp F2     ) (kp F3     ) (_         ) (_         )
                                             (_         ) (_         ) (_         ) (_       ) (_       ) (_         ) (_         ) (_         )
      ))
    ((number-layer . num)
     ((_         ) (_         ) (_         ) (_         ) (_         ) (_         )                       (_         ) (_         ) (_         ) (_         ) (_         ) (_         )
      (LAYER_LOCK) (_         ) (to FUN    ) (_         ) (to FUN    ) (_         )                       (_         ) (kp N7     ) (kp N8     ) (kp N9     ) (_         ) (_         )
      (_         ) (sk LALT   ) (sk LMETA  ) (sk LSHIFT ) (sk LCTRL  ) (_         )                       (kp N0     ) (kp N4     ) (kp N5     ) (kp N6     ) (kp N0     ) (_         )
      (_         ) (_         ) (_         ) (_         ) (_         ) (_         ) (_       ) (_       ) (_         ) (kp N1     ) (kp N2     ) (kp N3     ) (_         ) (_         )
                                             (_         ) (_         ) (_         ) (_       ) (kp N0   ) (kp N0     ) (_         ) (_         )
      ))
    ((qwerty-layer . qrt)
     ((_         ) (kp N1     ) (kp N2     ) (kp N3     ) (kp N4     ) (kp N5     )                       (kp N6     ) (kp N7     ) (kp N8     ) (kp N9     ) (kp N0     ) (kp MINUS  )
      (to QRT    ) (kp Q      ) (kp W      ) (kp E      ) (kp R      ) (kp T      )                       (kp Y      ) (kp U      ) (kp I      ) (kp O      ) (kp P      ) (kp BKSP   )
      (to QRT    ) (kp A      ) (kp S      ) (kp D      ) (kp F      ) (kp G      )                       (kp H      ) (kp J      ) (kp K      ) (kp L      ) (kp SEMI   ) (kp SQT    )
      (_         ) (kp Z      ) (kp X      ) (kp C      ) (kp V      ) (kp B      ) (_       ) (_       ) (kp N      ) (kp M      ) (kp COMMA  ) (kp DOT    ) (kp SLASH  ) (_         )
                                             (_         ) (_         ) (_         ) (_       ) (_       ) (_         ) (_         ) (_         )
      ))))

(define (resolve-layer-locks sexp)
  (let recur ((tail sexp)
              (acc '()))
    (match tail
      ((? null?) (reverse acc))
      (`(((,name . ,short-name) ,bindings) . ,rest)
       (let ((locked (cut symbol-append <> '_locked))
             (list-replace (lambda (target list-replace)
                             (list-transduce (treplace `((,target . ,list-replace)))
                                             rcons bindings))))
         (recur rest
           (append (if (member '(LAYER_LOCK) bindings)
                       `(((,(locked name) . ,(locked short-name))
                          ,(list-replace '(LAYER_LOCK) '(_)))
                         ((,name . ,short-name)
                          ,(list-replace '(LAYER_LOCK) `(to ,(locked short-name)))))
                       (list (car tail)))
                   acc)))))))

(define layer-defines
  (string-join
    (let ((i -1))
      (map (match-lambda
             (`((,name . ,short-name) ,bindings)
              (string-append
                "#define "
                (string-upcase (symbol->string short-name)) " "
                (begin (set! i (1+ i)) (number->string i)))))
           layers))
    "\n"))

(define (serialize-bindings layer-name)
  (lambda (binding)
    (match binding
      ;; TODO: Add dupe layers, serialize in their context
      ;; (`(LAYER_LOCK)
      ;;   (if (member layer-name (list base qwerty))
      ;;       (string-append "&to " (symbol->string layer-name))))
      (`(td_fun) "&td_fun") ; This should come from a list.
      (`(key_repeat) "&key_repeat")
      (`(cap_word) "&caps_word")
      (`(X) "XXX")
      (`(_) "___")
      (`(to ,layer)
       (string-append "&to " (string-upcase (symbol->string layer))))
      (`(lt ,layer)
       (string-append "&lt " (string-upcase (symbol->string layer))))
      (`(mo ,layer)
       (string-append "&mo " (string-upcase (symbol->string layer))))
      (`(kp ,keycode)
       (string-append "&kp " (symbol->string keycode)))
      (`(sk ,mod)
       (string-append "&sk " (symbol->string mod)))
      ;; TEMP
      (else (string-join (map (lambda (x) (format #f "~a" x)) binding) " ")))))

(define layer-bindings
  (string-join
    (map (match-lambda
           (`((,name . ,short-name) ,bindings)
            (string-append
              "        "
              (symbol->string short-name) " {\n            bindings = < "
              (string-join (map (serialize-bindings name) bindings) " ")
              " >;\n        };\n"
              )))
         layers)
    "\n"))

(define (make-subst pattern item)
  (lambda (str)
    (call-with-output-string
      (lambda (port)
        (regexp-substitute port
          (string-match pattern str)
          'pre (format #f "~a" item) 'post)))))

(define (main)
  (display ((compose (make-subst "\\{\\{LAYER_DEFINES\\}\\}" layer-defines)
                     (make-subst "\\{\\{LAYER_BINDINGS\\}\\}" layer-bindings))
            (call-with-input-file "lily58.keymap.in" get-string-all))))
