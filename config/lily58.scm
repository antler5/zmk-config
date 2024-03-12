#!/usr/bin/env -S guile -e '(lambda _ (with-output-to-file "lily58.keymap" main))' -s
!#
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

(use-modules (ice-9 format)
             (ice-9 regex)
             (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-11)
             (srfi srfi-26)
             (srfi srfi-171))

(define-syntax simple-morph
  (syntax-rules ()
    ((_ a s); alpha, shift
     (let*-values (((a s) (apply values (map symbol->string (list 'a 's))))
                   ((A S) (apply values (map string-upcase (list a s)))))
       (format #f "~6tSIMPLE_MORPH(~a_morph, SFT, &kp ~a, &kp ~a)" a A S)))
    ((_ a s c) ; alpha, shift, ctrl
     (let*-values (((a s c) (apply values (map symbol->string (list 'a 's 'c))))
                   ((A S C) (apply values (map string-upcase (list a s c)))))
       (format #f "~?~%~?"
         "~6tSIMPLE_MORPH(~a_morph, SFT, &kp ~a, &~a_inner_morph)"
         (list a A a)
         "~6tSIMPLE_MORPH(~a_inner_morph, CTL, &kp ~a, &kp ~a)"
         (list a S C))))))

(define behaviors
  (string-join
    (append
      ;; TODO: Bind a backup for ctrl-comma (etc.)?
      (list (simple-morph comma semi  gt)
            (simple-morph dot   colon lt)
            (simple-morph sqt   dqt   grave)
            (simple-morph excl  qmark pipe))
      (map (lambda (n)
             (format #f
               "~6tSIMPLE_MORPH(bt_morph_~d, SFT, &bt BT_SEL ~d, &bt BT_DISC ~d)"
               n n n))
           (iota 5)))
    "\n"))

(define layers
  ;; Indexes
  ;; ((00) (01) (02) (03) (04) (05)           (06) (07) (08) (09) (10) (11)
  ;;  (12) (13) (14) (15) (16) (17)           (18) (19) (20) (21) (22) (23)
  ;;  (24) (25) (26) (27) (28) (29)           (30) (31) (32) (33) (34) (35)
  ;;  (36) (37) (38) (39) (40) (41) (42) (43) (44) (45) (46) (47) (48) (49)
  ;;                 (50) (51) (52) (53) (54) (55) (56) (57)
  `(((base-layer . base)
     ((X         ) (X         ) (X         ) (X         ) (X         ) (X         )                       (X         ) (X         ) (X         ) (X         ) (X         ) (X         )
      (X         ) (kp V      ) (kp M      ) (kp L      ) (kp C      ) (kp P      )                       (kp B      ) (key_repeat) (kp U      ) (kp O      ) (kp Q      ) (X         )
      (X         ) (kp S      ) (kp T      ) (kp R      ) (kp D      ) (kp Y      )                       (kp F      ) (kp N      ) (kp E      ) (kp A      ) (kp I      ) (sm SQT    )
      (X         ) (kp X      ) (kp K      ) (kp J      ) (kp G      ) (kp W      ) (X       ) (X       ) (kp Z      ) (kp H      ) (sm COMMA  ) (sm DOT    ) (sm EXCL   ) (X         )
                                             (X         ) (to BASE   ) (mo NUM    ) (mo EXT  ) (X       ) (kp SPC    ) (mo SYM    ) (X         )
      ))
    ((number-layer . num)
     ((_         ) (_         ) (_         ) (_         ) (_         ) (_         )                       (_         ) (_         ) (_         ) (_         ) (_         ) (_         )
      (_         ) (to SYS    ) (to SYM    ) (to FUN    ) (to EXT    ) (to NUM    )                       (kp STAR   ) (kp N7     ) (kp N8     ) (kp N9     ) (kp FSLH   ) (kp TILDE  )
      (_         ) (sk LALT   ) (sk LMETA  ) (mo SYM    ) (sk LCTRL  ) (_         )                       (kp COLON  ) (kp N4     ) (kp N5     ) (kp N6     ) (kp PLUS   ) (kp MINUS  )
      (_         ) (_         ) (_         ) (caps_word ) (_         ) (_         ) (_       ) (_       ) (kp AT     ) (kp N1     ) (kp N2     ) (kp N3     ) (kp GT     ) (kp LT     )
                                             (_         ) (_         ) (_         ) (_       ) (_       ) (kp N0     ) (_         ) (_         )
      ))
    ((extend-layer . ext) ; ie. nav
     ((_         ) (_         ) (_         ) (_         ) (_         ) (_         )                       (_         ) (_         ) (_         ) (_         ) (_         ) (_         )
      (_         ) (kp N0     ) (kp CARET  ) (kp DOLLAR ) (kp PERCENT) (_         )                       (X         ) (kp HOME   ) (kp PG_DN  ) (kp PG_UP  ) (kp END    ) (_         )
      (_         ) (sk LALT   ) (sk LMETA  ) (sk LSHIFT ) (sk LCTRL  ) (_         )                       (kp RET    ) (kp LEFT   ) (kp DOWN   ) (kp UP     ) (kp RIGHT  ) (_         )
      (_         ) (kp K_UNDO ) (kp K_CUT  ) (kp K_COPY ) (kp K_PASTE) (_         ) (_       ) (_       ) (kp DELETE ) (kp BKSP   ) (kp TAB    ) (kp INSERT ) (kp ESC    ) (_         )
                                             (_         ) (_         ) (_         ) (_       ) (_       ) (kp BKSP   ) (_         ) (_         )
      ))
    ((function-layer . fun)
     ((_         ) (_         ) (_         ) (_         ) (_         ) (_         )                       (_         ) (_         ) (_         ) (_         ) (_         ) (_         )
      (_         ) (_         ) (_         ) (to BASE   ) (to QRT    ) (_         )                       (kp F12    ) (kp F7     ) (kp F8     ) (kp F9     ) (kp PSCRN  ) (_         )
      (_         ) (sk LALT   ) (sk LMETA  ) (sk LSHIFT ) (sk LCTRL  ) (_         )                       (kp F11    ) (kp F4     ) (kp F5     ) (kp F6     ) (_         ) (_         )
      (_         ) (kp KP_CLEAR)(kp KP_NUM ) (kp CAPS   ) (kp SLCK   ) (_         ) (_       ) (_       ) (kp F10    ) (kp F1     ) (kp F2     ) (kp F3     ) (_         ) (_         )
      ))
    ((symbol-layer . sym)
     ((_         ) (_         ) (_         ) (_         ) (_         ) (_         )                       (_         ) (_         ) (_         ) (_         ) (_         ) (_         )
      (_         ) (_         ) (_         ) (_         ) (_         ) (_         )                       (kp HASH   ) (_         ) (kp RBKT   ) (kp LBKT   ) (kp BSLH   ) (kp AMPS   )
      (_         ) (sk LALT   ) (sk LMETA  ) (sk LSHIFT ) (sk LCTRL  ) (_         )                       (kp SEMI   ) (_         ) (kp RPAR   ) (kp LPAR   ) (kp EQUAL  ) (kp UNDER  )
      (_         ) (_         ) (_         ) (_         ) (_         ) (_         ) (_       ) (_       ) (_         ) (_         ) (kp RBRC   ) (kp LBRC   ) (kp RPAR   ) (kp LPAR   )
                                             (_         ) (_         ) (_         ) (_       ) (_       ) (_         ) (_         ) (_         ) (_         )
      ))
    ((system-layer . sys)
     ((_         ) (_         ) (_         ) (_         ) (_         ) (&bootloader)                      (&bootloader)(_         ) (_         ) (_         ) (_         ) (_         )
      (_         ) (_         ) (_         ) (to BASE   ) (to QRT    ) (&sys_reset)                       (&sys_reset) (&bt BT_PRV) (&bt BT_NXT)(&out OUT_TOG)(_         ) (_         )
      (_         ) (sk LALT   ) (sk LMETA  ) (sk LSHIFT ) (sk LCTRL  ) (_         )                       (&bt BT_CLR) (bm 3      ) (bm 4      ) (_         ) (_         ) (_         )
      (_         ) (_         ) (_         ) (_         ) (_         ) (_         ) (_       ) (_       )(&bt BT_CLR_ALL)(bm 0    ) (bm 1      ) (bm 2      ) (_         ) (_         )
                                             (_         ) (_         ) (_         ) (_       ) (_       ) (_         ) (_         ) (_         )
      ))
    ((qwerty-layer . qrt)
     ((_         ) (kp N1     ) (kp N2     ) (kp N3     ) (kp N4     ) (kp N5     )                       (kp N6     ) (kp N7     ) (kp N8     ) (kp N9     ) (kp N0     ) (_         )
      (_         ) (kp Q      ) (kp W      ) (kp E      ) (kp R      ) (kp T      )                       (kp Y      ) (kp U      ) (kp I      ) (kp O      ) (kp P      ) (_         )
      (_         ) (kp A      ) (kp S      ) (kp D      ) (kp F      ) (kp G      )                       (kp H      ) (kp J      ) (kp K      ) (kp L      ) (kp SEMI   ) (kp SQT    )
      (_         ) (kp Z      ) (kp X      ) (kp C      ) (kp V      ) (kp B      ) (_       ) (_       ) (kp N      ) (kp M      ) (kp COMMA  ) (kp DOT    ) (kp SLASH  ) (_         )
                                             (to QRT    ) (_         ) (_         ) (_       ) (_       ) (_         ) (_         ) (_         )
       ))
    ))

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

(define simple-behaviors
  '(caps_word
    key_repeat
    td_fun))

(define (serialize-bindings layer-name)
  (lambda (binding)
    (match binding
      (`(X) "XXX")
      (`(_) "___")
      ((and `(,behavior)
            (? (lambda _ (member behavior simple-behaviors))))
       (string-append "&" (symbol->string behavior)))
      (`(sm ,keycode) ; simple-morph
       (string-append "&" (string-downcase (symbol->string keycode)) "_morph"))
      (`(bm ,n) ; bluetooth-morph
       (string-append "&bt_morph_" (number->string n)))
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
      (else (string-join (map (cut format #f "~a" <>) binding) " ")))))

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
                     (make-subst "\\{\\{BEHAVIORS\\}\\}" behaviors)
                     (make-subst "\\{\\{LAYER_BINDINGS\\}\\}" layer-bindings))
            (call-with-input-file "lily58.keymap.in" get-string-all))))

;; TODO [#C]: Mode needs reloaded, can file-local variables invoke `custom' setters?
;; Local Variables:
;; whitespace-style: '(face missing-newline-at-eof tab-mark)
;; End:
