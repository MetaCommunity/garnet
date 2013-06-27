;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;-------------------------------------------------------------------;;
;;         The Garnet User Interface Development Environment.        ;;
;;-------------------------------------------------------------------;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;-------------------------------------------------------------------;;

;;; $Id$

;;; ^f ^b ^d ^h = forward, backwards, delete forwards, delete backwards char
;;  left-arrow, right-arrow =  backwards, forwards
;;  meta-f, meta-b, meta-d, meta-h  = same but by words
;;  ^p = previous line, ^n = next line
;;  uparrow, downarrow = previous line, next line
;;  ^, or HOME = beginning of document
;;  ^. or END = end of document
;;  ^a = beginning of line
;;  ^e = end of line
;;
;;  ^k = kill line, ^u = delete entire string, ^w, CUT = delete selection
;;  META-w, COPY = copy selection to cut buffer
;;  ^c = copy entire string to X cut buffer
;;  ^y, PASTE = yank kill buffer or X cut buffer
;;  ^Y, ^PASTE = yank X buffer
;;  meta-y, meta-PASTE = yank kill buffer
;;
;; The following ones extend the selection while moving
;;   ^leftarrow, ^rightarrow = prev, next char selecting
;;   meta-leftarrow, meta-rightarrow = prev, next word selecting
;;   ^uparrow, ^downarrow = up-line, down-line selecting
;;   ^HOME ^END = beginning, end of string selecting
;;   ^* = select all
;;
;;  CONTROL-META is Lisp stuff:
;;     ^-META-b, ^-META-leftarrow = prev lisp expression
;;     ^-META-f, ^-META-rightarrow =  next lisp expression
;;     ^-META-h, ^-META-backspace, ^-META-delete = delete prev s-expr
;;     ^-META-d  = delete next s-expr
;;
;;  ^-shift- = font stuff:
;;     ^-shift-B = toggle bold
;;     ^-shift-I = toggle italic
;;     ^-shift-F = fixed font (courier)
;;     ^-shift-T = times font (serif)
;;     ^-shift-H = helvetica font (sans-serif)
;;     ^-shift-< = smaller font
;;     ^-shift-> = bigger font
;;     ^1 ^2 ^3 ^4  = small, medium, large, and very-large fonts


(in-package "INTERACTORS")

;;; Initializes the hash table of an-interactor with the standard
;;  translations.  If there is no table in an-interactor, creates one.
;;  Otherwise, removes any translations that are there before adding
;;  the new ones.
(defun Set-MultiFont-Default-Key-Translations (an-interactor)
  " Initializes the hash table of an-interactor with translations
for the multifont gadget.  If there is no table in an-interactor, 
creates one. Otherwise, removes any translations that are there
before adding the new ones."

  ;; Set the ordinary text translations.
  (Set-Default-Key-Translations an-interactor :standard-translation-table)
  (let ((ht (get-local-value an-interactor :standard-translation-table)))

    ;; Add the translations for the multifont gadget.
  
    (bind-key-internal :meta-\b     :prev-word ht)
    (bind-key-internal :meta-B      :prev-word ht)
    #+unicode
    (bind-key-internal :meta-latin_small_letter_b :prev-word ht)

    (bind-key-internal :control-meta-\b         :lisp-prev-expr ht)
    (bind-key-internal :control-meta-B          :lisp-prev-expr ht)
    (bind-key-internal :control-meta-LEFTARROW  :lisp-prev-expr ht)
    #+unicode
    (bind-key-internal :control-meta-latin_small_letter_b :lisp-prev-expr ht)
    
    (bind-key-internal :control-LEFTARROW   :prev-char-select ht)
    (bind-key-internal :meta-LEFTARROW      :prev-word-select ht)

    (bind-key-internal :meta-F      :next-word ht)
    (bind-key-internal :meta-\f     :next-word ht)
    #+unicode
    (bind-key-internal :meta-latin_small_letter_f :next-word ht)

    (bind-key-internal :control-meta-\f :lisp-next-expr ht)
    (bind-key-internal :control-meta-F  :lisp-next-expr ht)
    (bind-key-internal :control-meta-RIGHTARROW  :lisp-next-expr ht)
    #+unicode
    (bind-key-internal :control-meta-latin_small_letter_f :lisp-next-expr ht)

    (bind-key-internal :control-RIGHTARROW   :next-char-select ht)
    (bind-key-internal :meta-RIGHTARROW      :next-word-select ht)
    (bind-key-internal :control-UPARROW      :up-line-select   ht)
    (bind-key-internal :control-DOWNARROW    :down-line-select ht)
    
    (bind-key-internal :R7        :beginning-of-string ht) ; HOME key on Sun

    (bind-key-internal :control-HOME :beginning-of-string-select ht)
    (bind-key-internal :control-R7   :beginning-of-string-select ht) ;HOME key
    
    (bind-key-internal :R13         :end-of-string ht) ; END key on Sun

    (bind-key-internal :control-END :end-of-string-select ht)
    (bind-key-internal :control-R13 :end-of-string-select ht) ;END key
    
    (bind-key-internal :control-\*  :select-all ht)
    #+unicode
    (bind-key-internal :control-asterisk  :select-all ht)
    
    (bind-key-internal :meta-H         :delete-prev-word ht)
    (bind-key-internal :meta-\h        :delete-prev-word ht)
    (bind-key-internal :meta-BACKSPACE :delete-prev-word ht)
    (bind-key-internal :meta-DELETE    :delete-prev-word ht)
    (bind-key-internal :meta-RUBOUT    :delete-prev-word ht)
    #+unicode
    (bind-key-internal :meta-latin_small_letter_h :delete-prev-word ht)
    #+unicode
    (bind-key-internal :meta-latin_capital_letter_h :delete-prev-word ht)

    (bind-key-internal :control-meta-\h         :lisp-delete-prev-expr ht)
    (bind-key-internal :control-meta-H          :lisp-delete-prev-expr ht)
    (bind-key-internal :control-meta-BACKSPACE  :lisp-delete-prev-expr ht)
    (bind-key-internal :control-meta-DELETE     :lisp-delete-prev-expr ht)
    (bind-key-internal :control-meta-RUBOUT     :lisp-delete-prev-expr ht)
    #+unicode
    (bind-key-internal :control-meta-latin_small_letter_h :lisp-delete-prev-expr ht)
    #+unicode
    (bind-key-internal :control-meta-latin_capital_letter_h :lisp-delete-prev-expr ht)

    ;; delete next

    (bind-key-internal :meta-D      :delete-next-word ht)
    (bind-key-internal :meta-\d     :delete-next-word ht)
    #+unicode
    (bind-key-internal :meta-latin_small_letter_d :delete-next-word ht)
    #+unicode
    (bind-key-internal :control-meta-latin_capital_letter_d :delete-next-word ht)

    (bind-key-internal :control-meta-\d    :lisp-delete-next-expr ht)
    (bind-key-internal :control-meta-D     :lisp-delete-next-expr ht)
    #+unicode
    (bind-key-internal :control-meta-latin_small_letter_d :lisp-delete-next-expr ht)
    #+unicode
    (bind-key-internal :control-meta-latin_capital_letter_d :lisp-delete-next-expr ht)

    ;; other deletes and copies

    (bind-key-internal :control-W  :delete-selection ht)
    (bind-key-internal :control-\w :delete-selection ht)
    (bind-key-internal :CUT        :delete-selection ht)
    (bind-key-internal :L10        :delete-selection ht) ;; cut key on Sun
    #+unicode
    (bind-key-internal :control-latin_capital_letter_w :delete-selection ht)
    #+unicode
    (bind-key-internal :control-latin_small_letter_w :delete-selection ht)
    
    (bind-key-internal :meta-W     :copy-selection ht)
    (bind-key-internal :meta-\w    :copy-selection ht)
    (bind-key-internal :COPY       :copy-selection ht)
    (bind-key-internal :L6         :copy-selection ht) ;; copy key on Sun
    #+unicode
    (bind-key-internal :meta-latin_capital_letter_w :copy-selection ht)
    #+unicode
    (bind-key-internal :meta-latin_small_letter_w :copy-selection ht)
    
    ;; PASTES
    
    (bind-key-internal :control-\y  :yank-buffer-or-X-cut-buffer ht)
    (bind-key-internal :insert      :yank-buffer-or-X-cut-buffer ht)
    (bind-key-internal :L8          :yank-buffer-or-X-cut-buffer ht) ;paste on Sun
    (bind-key-internal :insert-line :yank-buffer-or-X-cut-buffer ht)
    #+unicode
    (bind-key-internal :control-latin_small_letter_y :yank-buffer-or-X-cut-buffer ht)

    (bind-key-internal :control-insert  :copy-from-X-cut-buffer ht)
    (bind-key-internal :control-Y       :copy-from-X-cut-buffer ht)
    (bind-key-internal :control-L8      :copy-from-X-cut-buffer ht)
    #+unicode
    (bind-key-internal :control-latin_capital_letter_y :copy-from-X-cut-buffer ht)

    (bind-key-internal :meta-insert     :yank-buffer ht)
    (bind-key-internal :meta-Y          :yank-buffer ht)
    (bind-key-internal :meta-L8         :yank-buffer ht)
    #+unicode
    (bind-key-internal :meta-latin_capital_letter_y :yank-buffer ht)

    ;; NEWLINES

    (bind-key-internal :control-J   #\Newline ht)
    #+unicode
    (bind-key-internal :control-latin_capital_letter_j    #\Newline ht)

    (bind-key-internal :control-O  :Insert-LF-after ht)
    #+unicode
    (bind-key-internal :control-latin_capital_letter_o :insert-lf-after ht)
    
    ;; FONT STUFF
    
    (bind-key-internal :control-B  :toggle-bold ht)
    #+unicode
    (bind-key-internal :control-latin_capital_letter_b :toggle-bold ht)
    (bind-key-internal :control-I  :toggle-italic ht)
    #+unicode
    (bind-key-internal :control-latin_capital_letter_i :toggle-italic ht)
    (bind-key-internal :control->  :bigger ht)
    #+unicode
    (bind-key-internal :control-greater-than_sign :bigger ht)
    (bind-key-internal :control-<  :smaller ht)
    #+unicode
    (bind-key-internal :control-less-than_sign :smaller ht)

    (bind-key-internal :control-1  :small ht)
    #+unicode
    (bind-key-internal :control-digit_one      :smaller ht)
    (bind-key-internal :control-2  :medium ht)
    #+unicode
    (bind-key-internal :control-digit_two      :smaller ht)
    (bind-key-internal :control-3  :large ht)
    #+unicode
    (bind-key-internal :control-digit_three  :smaller ht)
    (bind-key-internal :control-4  :very-large ht)
    #+unicode
    (bind-key-internal :control-digit_four :smaller ht)
    (bind-key-internal :control-!  :small ht)
    #+unicode
    (bind-key-internal :control-exclamation_mark :smaller ht)
    (bind-key-internal :control-@  :medium ht)
    (bind-key-internal :control-#  :large ht)
    #+unicode
    (bind-key-internal :control-number_sign :smaller ht)
    (bind-key-internal :control-$  :very-large ht)
    #+unicode
    (bind-key-internal :control-dollar_sign :smaller ht)

    (bind-key-internal :control-F  :fixed ht)
    #+unicode
    (bind-key-internal :control-latin_capital_letter_F :yank-buffer-or-X-cut-buffer ht)
    (bind-key-internal :control-T  :serif ht)
    #+unicode
    (bind-key-internal :control-latin_capital_letter_T :yank-buffer-or-X-cut-buffer ht)
    (bind-key-internal :control-H  :sans-serif ht)
    #+unicode
    (bind-key-internal :control-latin_capital_letter_H :yank-buffer-or-X-cut-buffer ht)
      
    ;; translate the number pad into regular characters (if CMU)
    #+cmu (bind-key-internal :num-pad-1 #\1 ht)
    #+cmu (bind-key-internal :num-pad-2 #\2 ht)
    #+cmu (bind-key-internal :num-pad-3 #\3 ht)
    #+cmu (bind-key-internal :num-pad-4 #\4 ht)
    #+cmu (bind-key-internal :num-pad-5 #\5 ht)
    #+cmu (bind-key-internal :num-pad-6 #\6 ht)
    #+cmu (bind-key-internal :num-pad-7 #\7 ht)
    #+cmu (bind-key-internal :num-pad-8 #\8 ht)
    #+cmu (bind-key-internal :num-pad-9 #\9 ht)
    #+cmu (bind-key-internal :num-pad-0 #\0 ht)
    #+cmu (bind-key-internal :num-pad-/ #\/ ht)
    #+cmu (bind-key-internal :num-pad-* #\* ht)
    #+cmu (bind-key-internal :num-pad-- #\- ht)
    #+cmu (bind-key-internal :num-pad-+ #\+ ht)
    #+cmu (bind-key-internal :num-pad-. #\. ht)
    #+cmu (bind-key-internal :num-pad-enter #\NewLine ht) ; the enter key

    ))

;; In addition to the standard key bindings, the lisp translation table
;; also handles tabs, parens, semi-colons, etc. specially for lisp mode
;;
(defun Set-Lisp-Key-Translations (an-interactor)
   (let ((ht (copy-hash-table
	      (get-local-value an-interactor :standard-translation-table))))
     (s-value an-interactor :lisp-translation-table ht)
     (bind-key-internal #\tab #'tab ht)
     (bind-key-internal #\; #'semi-func ht)
     (bind-key-internal #\return #'return-func ht)
     (bind-key-internal #\\ #'bslash-func ht)
     (bind-key-internal #\- #'bslash-func ht)
     (bind-key-internal #\rubout #'rubout-func ht)
     (bind-key-internal :control-\d #'cd-func ht)
      #+unicode
      (bind-key-internal :control-latin_small_letter_d #'cd-func ht)
     (bind-key-internal :meta-\d #'md-func ht)
      #+unicode
      (bind-key-internal :meta-latin_small_letter_d #'md-func ht)
     (bind-key-internal :control-\k #'ck-func ht) 
      #+unicode
      (bind-key-internal :control-latin_small_letter_k #'ck-func ht)
     (bind-key-internal :control-\y #'cy-func ht) 
      #+unicode
      (bind-key-internal :control-latin_small_letter_y #'cy-func ht)
     (bind-key-internal :control-meta-\f #'cmf-func ht)
     (bind-key-internal :control-meta-f #'cmf-func ht)
      #+unicode
      (bind-key-internal :control-meta-latin_small_letter_f #'cmf-func ht)
     (bind-key-internal :control-meta-rightarrow #'cmf-func ht)
     (bind-key-internal :control-meta-\b #'cmb-func ht)
     (bind-key-internal :control-meta-b #'cmb-func ht)
      #+unicode
      (bind-key-internal :control-meta-latin_small_letter_b #'cmb-func ht)
     (bind-key-internal :control-meta-leftarrow #'cmb-func ht)
     (bind-key-internal :control-meta-\d #'cmd-func ht)
     (bind-key-internal :control-meta-d #'cmd-func ht)
      #+unicode
      (bind-key-internal :control-meta-latin_small_letter_d #'cmd-func ht)
     (bind-key-internal :control-meta-\h #'cmh-func ht)
     (bind-key-internal :control-meta-h #'cmh-func ht)
      #+unicode
      (bind-key-internal :control-meta-latin_small_letter_h #'cmh-func ht)
     (bind-key-internal :control-meta-rubout #'cmh-func ht)     
     (bind-key-internal #\# #'hash-func ht)
     (bind-key-internal #\( #'open-paren-func ht)
     (bind-key-internal #\) #'close-paren-func ht)
     (bind-key-internal #\space #'space-func ht)
     (bind-key-internal #\| #'bar-func ht)
     (bind-key-internal #\" #'quote-func ht)))


