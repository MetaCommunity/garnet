;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base:
;;-------------------------------------------------------------------;;
;;          The Garnet User Interface Development Environment.       ;;
;;                                                                   ;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;-------------------------------------------------------------------;;

;;; $Id$

;;; The code in this file sets the default bindings for text gadgets.
;;

(in-package "INTERACTORS")

(defun Set-Default-Key-Translations (an-interactor &optional (table :key-translation-table))
"Initializes the hash table of an-interactor with the standard
translations.  If there is no table in an-interactor, creates one.
Otherwise, removes any translations that are there before adding the new ones."
  (let ((ht (get-local-value an-interactor table)))
    (if (not (hash-table-p ht))
      (s-value an-interactor table (setq ht (make-hash-table)))
      ; else re-initialize ht
      (clrhash ht))
      (bind-key-internal :leftarrow   :prev-char ht)
      (bind-key-internal :control-b   :prev-char ht)
      (bind-key-internal :control-\b  :prev-char ht)
      #+unicode
      (bind-key-internal :control-latin_small_letter_b :prev-char ht)
    
      (bind-key-internal :rightarrow  :next-char ht)
      (bind-key-internal :control-f   :next-char ht)
      (bind-key-internal :control-\f  :next-char ht)
      #+unicode
      (bind-key-internal :control-latin_small_letter_f :next-char ht)
      
      (bind-key-internal :uparrow     :up-line ht)
      (bind-key-internal :control-p   :up-line ht)
      (bind-key-internal :control-\p  :up-line ht)
      #+unicode
      (bind-key-internal :control-latin_small_letter_p :up-line ht)
    
      (bind-key-internal :downarrow   :down-line ht)
      (bind-key-internal :control-n   :down-line ht)
      (bind-key-internal :control-\n  :down-line ht)
      #+unicode
      (bind-key-internal :control-latin_small_letter_n :down-line ht)
    
      (bind-key-internal #\delete     :delete-prev-char ht)
      (bind-key-internal #\backspace  :delete-prev-char ht)
      (bind-key-internal :control-h   :delete-prev-char ht)
      (bind-key-internal :control-\h  :delete-prev-char ht)
      #+unicode
      (bind-key-internal :control-latin_small_letter_h :delete-prev-char ht)
    
      (bind-key-internal :control-backspace :delete-prev-word ht)
      (bind-key-internal :control-delete    :delete-prev-word ht)
      (bind-key-internal :control-w         :delete-prev-word ht)
      (bind-key-internal :control-\w        :delete-prev-word ht)
      #+unicode
      (bind-key-internal :control-latin_small_letter_w :delete-prev-word ht)
    
      (bind-key-internal :control-d   :delete-next-char ht)
      (bind-key-internal :control-\d  :delete-next-char ht)
      #+unicode
      (bind-key-internal :control-latin_small_letter_d :delete-next-char ht)
    
      (bind-key-internal :control-u   :delete-string ht)
      (bind-key-internal :control-\u  :delete-string ht)
      #+unicode
      (bind-key-internal :control-latin_small_letter_u :delete-string ht)

      (bind-key-internal :control-o   :insert-lf-after ht)
      (bind-key-internal :control-\o  :insert-lf-after ht)
      #+unicode
      (bind-key-internal :control-latin_small_letter_o :insert-lf-after ht)

      (bind-key-internal :control-k   :kill-line ht)
      (bind-key-internal :control-\k  :kill-line ht)
      #+unicode
      (bind-key-internal :control-latin_small_letter_k :kill-line ht)

      (bind-key-internal :home        :beginning-of-string ht)
      (bind-key-internal :control-\,  :beginning-of-string ht)
      (bind-key-internal :control-<   :beginning-of-string ht)
      #+unicode
      (bind-key-internal :control-less-than_sign :beginning-of-string ht)
      #+unicode
      (bind-key-internal :control-comma  :beginning-of-string ht)
    
      (bind-key-internal :control-a   :beginning-of-line ht)
      (bind-key-internal :control-\a  :beginning-of-line ht)
      #+unicode
      (bind-key-internal :control-latin_small_letter_a :beginning-of-line ht)
    
      (bind-key-internal :end         :end-of-string ht) 
      (bind-key-internal :control-.   :end-of-string ht)
      (bind-key-internal :control->   :end-of-string ht)
      #+unicode
      (bind-key-internal :control-full_stop  :end-of-string ht)
      #+unicode
      (bind-key-internal :control-greater-than_sign :end-of-string ht)
    
      (bind-key-internal :control-e   :end-of-line ht)
      (bind-key-internal :control-\e  :end-of-line ht)
      #+unicode
      (bind-key-internal :control-latin_small_letter_e :end-of-line ht)
    
      (bind-key-internal :control-c   :copy-to-X-cut-buffer ht)
      (bind-key-internal :control-\c  :copy-to-X-cut-buffer ht)
      #+unicode
      (bind-key-internal :control-latin_small_letter_c :copy-to-X-cut-buffer ht)
    
      (bind-key-internal :insert      :copy-from-X-cut-buffer ht)
      (bind-key-internal :insert-line :copy-from-X-cut-buffer ht)

      (bind-key-internal :control-y   :copy-from-X-cut-buffer ht)
      (bind-key-internal :control-\y  :copy-from-X-cut-buffer ht)
      #+unicode
      (bind-key-internal :control-latin_small_letter_y :copy-from-X-cut-buffer ht)
    
      (bind-key-internal #\return     #\Newline ht)
      (bind-key-internal :control-j   #\Newline ht)
      (bind-key-internal :control-\j  #\Newline ht)
      #+unicode
      (bind-key-internal :control-latin_small_letter_j #\Newline ht)
    
    ))
