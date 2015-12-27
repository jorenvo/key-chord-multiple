;;; key-chord-multiple.el --- map simultaneously pressed keys to commands
;;-------------------------------------------------------------------
;;
;; Copyright (C) 2003,2005,2008,2012 David Andersson
;; Copyright (C) 2015 Joren Van Onder
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;-------------------------------------------------------------------

;; Author: Joren Van Onder <joren.vanonder@gmail.com>
;; Maintainer: Joren Van Onder <joren.vanonder@gmail.com>
;; Keywords: keyboard chord input

;;; Commentary:

;; ########   Description   ########################################
;;
;; This is inspired by key chord mode originally written by David
;; Andersson. Functionally key chord multiple is similar to key chord
;; mode, except that in key chord multiple you can define key chords
;; consisting of an arbitrary number of keys. This was not a trivial
;; change (look at the diff between this and key-chord.el), in fact
;; most of the code had to be rewritten. Therefore this will probably
;; remain a separate program, never to be merged with the original key
;; chord mode.
;;
;; ########   Motivation    ########################################
;;
;; I think one of the biggest limitations of key chord mode is finding
;; accessible key chords that do not conflict with normal typing. A
;; proposed method to find good key chords is to use a dictionary in
;; your language to test your key chord against. This helps but is not
;; guaranteed to detect bad key chords, especially for
;; programmers. After spending time attempting to find accessible
;; key chords that do not conflict during regular Emacs use, I have
;; come to the conclusion that they simply do not exist in large
;; numbers. It is difficult to find > 10 key chords that are not part
;; of your regular typing and are also easily accessible (personnally,
;; the only keys that I consider accessible are all keys on the home
;; row, the row above it and the row below it, so this does not
;; include the number keys).
;; 
;; Key chord multiple provides a solution to this problem by allowing
;; key chords to consist of an arbitrary number of keys. This makes it
;; much easier to find key chords that work well for a user. It
;; ofcourse also drastically increases the number of available, good
;; key chords.
;;
;; [TODO] maybe provide some fancy POC QWERTY keymap consisting of a
;; bunch of stuff.
;;
;; ########   Limitations   ########################################
;;
;; Compared to key chord mode the following functionality was lost:
;; - key chords in keyboard macros
;; I might add this back.
;; - key chords that consist of one key pressed multiple times
;; I myself probably won't add this back as it is a feature I never
;; used.
;; - describing key chords with `describe-key' (C-h k)
;; Probably won't add this back either, as I don't think it's useful
;; enough to justify the added complexity.
;;
;; key chord multiple supports key chords that consist of an arbitrary
;; number of keys, your keyboard however may not. A lot of cheap
;; keyboards do not reliably detect 3 or more keys pressed
;; simultaneously. If this problem affects you, the only thing you can
;; do is to buy a better keyboard. Good keyboards will have NKR, which
;; stands for N-key rollover. This means that it will reliably detect
;; any combination of simultaneously pressed keys. Important to note
;; (and something manufacturers sometimes don't mention) is that NKR
;; is not actually achievable over USB, only over PS/2. An NKR
;; keyboard over USB should do 6KR though, which is more than
;; sufficient.

;;; Code:

(defvar key-chord-delay-in-s 0.1	; 0.05 or 0.1
  "Max time delay between two key press to be considered a key chord.")

;; Internal vars
(defvar key-chord-mode nil)
(defvar available-keychord-sequences nil)
(defvar buffered-keys nil)

;;;###autoload
(defun key-chord-mode (arg)
  "Toggle key chord mode.
With positive ARG enable the mode. With zero or negative arg disable the mode.
A key chord is two keys that are pressed simultaneously, or one key quickly
pressed twice.
\nSee functions `key-chord-define-global', `key-chord-define-local', and
`key-chord-define' and variable `key-chord-delay-in-s'."
  (interactive "P")
  (setq key-chord-mode (if arg
			   (> (prefix-numeric-value arg) 0)
			 (not key-chord-mode)))
  (cond (key-chord-mode
	 (setq input-method-function 'key-chord-input-method)
	 (message "Key Chord mode on"))
	(t
	 (setq input-method-function nil)
	 (message "Key Chord mode off"))))

;;;###autoload
(defun key-chord-define-global (keys command)
  "Define a key-chord of an arbitrary number of KEYS starting a COMMAND.
\nKEYS can be a string or a vector. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND has to be an interactive function.
If COMMAND is nil, the key-chord is removed.
\nNote that KEYS defined locally in the current buffer will have precedence."
  (interactive "sSet key chord globally: \nCSet chord \"%s\" to command: ")
  (key-chord-define (current-global-map) keys command))

;;;###autoload
(defun key-chord-define-local (keys command)
  "Locally define a key-chord of KEYS starting a COMMAND.
\nKEYS can be a string or a vector. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function.
If COMMAND is nil, the key-chord is removed.
\nThe binding goes in the current buffer's local map,
which in most cases is shared with all other buffers in the same major mode."
  (interactive "sSet key chord locally: \nCSet chord \"%s\" to command: ")
  (key-chord-define (current-local-map) keys command))

;;;###autoload
(defun key-chord-define (keymap keys command)
  "Define in KEYMAP a key-chord of an arbitrary number of KEYS starting a COMMAND.
\nKEYS can be a string or a vector. Currently only elements that
corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND has to be an interactive function. If COMMAND is nil,
the key-chord is removed."
  (define-key keymap (vconcat (vector 'key-chord) keys) command))

;; prioritizes like this:
;; highest priority: current-minor-mode-maps
;; lower priority  : current-local-map
;; lower priority  : current-global-map
;;
;; mimics emacs' internal keymap preference
;; see http://www.gnu.org/software/emacs/manual/html_node/elisp/Active-Keymaps.html
(defun key-chord-lookup-key (key)
  "Lookup KEY in all current key maps."
  (let ((maps (current-minor-mode-maps))
	res)
    (while (and maps (not res))
      (setq res (key-chord-lookup-key1 (car maps) key)
	    maps (cdr maps)))
    (or res
	(if (current-local-map)
	    (key-chord-lookup-key1 (current-local-map) key))
	(key-chord-lookup-key1 (current-global-map) key))))

(defun key-chord-describe ()
  "List key chord bindings in a help buffer.
\nTwo key chords will be listed twice and there will be Prefix Commands.
Please ignore that."
  (interactive)
  (describe-bindings [key-chord]))

(defun add-to-prefix (prefix element)
  (append prefix (list (car node))))

(defun traverse-branch (node &optional prefix)
  (if (listp (cdr node))
      ;; if node has children
      (mapc (lambda (child)
              (traverse-branch child (append prefix (list (car node)))))
            (cddr node))
    ;; else node has no children
    (setq prefix (add-to-prefix prefix node))
    (push (cdr node) prefix) ;; push the symbol to the beginning of the list
    (setq available-keychord-sequences (append available-keychord-sequences (list prefix)))))

(defun build-available-keychord-sequences ()
  (mapc (lambda (branch)
          (traverse-branch branch))
        (cdr (lookup-key (current-global-map) (vector 'key-chord)))))

(defun remove-matching-key (key-chord-sequence key)
  (if (member key key-chord-sequence)
      ;; filter everything that is not key
      (-filter (lambda (el) (not (eq el key))) key-chord-sequence)
    ;; key not in key-chord, so return empty list
    nil))

(defun filter-out-empty-lists ()
  (setq available-keychord-sequences
        (-filter (lambda (el) (not (eq (length el) 0)))
                 available-keychord-sequences)))

(defun reset-key-chord ()
  (setq buffered-keys nil)
  (setq available-keychord-sequences nil))

(defun key-chord-input-method (key)
  (if (eq (length available-keychord-sequences) 0)
      (build-available-keychord-sequences))
  (setq available-keychord-sequences
        (mapcar (lambda (key-chord-sequence)
                  (remove-matching-key key-chord-sequence key))
                available-keychord-sequences))

  ;; filter out empty lists. maybe not necessary?
  (filter-out-empty-lists)

  (if (eq (length available-keychord-sequences) 0)
      ;; no matches, redispatch all previous keys followed by this current key
      (progn
        (let ((keys-to-redispatch))
          ;; don't return nil as an event from input-method
          (if key
              (setq keys-to-redispatch (reverse (cons key buffered-keys)))
            (setq keys-to-redispatch (reverse buffered-keys)))
          (reset-key-chord)
          keys-to-redispatch))
    (if (and (eq (length available-keychord-sequences) 1) ;; only one keychord left
             (eq (length (car available-keychord-sequences)) 1) ;; in the keychord we only have 1 element left
             (symbolp (caar available-keychord-sequences))) ;; element is symbol (should be function to execute)
        ;; full match, execute symbol
        (progn
          (funcall (caar available-keychord-sequences))
          (reset-key-chord)
          nil)
      ;; > 0 matches, but no full match, buffer and wait
      (setq buffered-keys (cons key buffered-keys))
      ;; it would be nicer to just be able to use the input method
      ;; function, it does not read non-printing characters like
      ;; return or backspace. Because of this, we use read-event
      ;; instead, which reads all events.
      (let ((input-method-function nil))
        (key-chord-input-method (read-event nil nil key-chord-delay-in-s))))))

(provide 'key-chord-multiple)
;;; key-chord-multiple.el ends here
