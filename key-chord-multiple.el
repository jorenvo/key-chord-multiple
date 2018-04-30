;;; key-chord-multiple.el --- map simultaneously pressed keys to commands
;;
;; Copyright (C) 2003,2005,2008,2012 David Andersson
;; Copyright (C) 2015 Joren Van Onder

;; Author: Joren Van Onder <joren.vanonder@gmail.com>
;; URL: https://github.com/jorenvo/key-chord-multiple
;; Maintainer: Joren Van Onder <joren.vanonder@gmail.com>
;; Keywords: keyboard chord input
;; Package-Requires: ((dash "2.12.1"))

;; This file is NOT part of Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defvar key-chord-delay-in-s 0.1	; 0.05 or 0.1
  "Maximum time delay in seconds between two key press to be
considered a key chord.")

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
  (if key-chord-mode
      (progn
        (setq input-method-function 'key-chord-input-method)
        (message "Key Chord Multiple mode on"))
    (setq input-method-function nil)
    (message "Key Chord Multiple mode off")))

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

(defun key-chord-describe ()
  "List key chord bindings in a help buffer.
\nTwo key chords will be listed twice and there will be Prefix Commands.
Please ignore that."
  (interactive)
  (describe-bindings [key-chord]))

(defun add-to-prefix (prefix element)
  (append prefix (list (car node))))

(defun traverse-branch (node &optional prefix)
  (if (and (listp (cdr node))
           (not (functionp (cdr node))))
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
             (commandp (caar available-keychord-sequences))) ;; element is function that can be executed
        ;; full match, execute symbol
        (let ((command-to-execute (caar available-keychord-sequences)))
          (reset-key-chord)
          (command-execute command-to-execute)
          nil)
      ;; > 0 matches, but no full match, buffer and wait
      (setq buffered-keys (cons key buffered-keys))
      ;; it would be nicer to just be able to use the input method
      ;; function, but it does not read non-printing characters like
      ;; return or backspace. Because of this, we use read-event
      ;; instead, which reads all events.
      (let ((input-method-function nil))
        (key-chord-input-method (read-event nil nil key-chord-delay-in-s))))))

(provide 'key-chord-multiple)
;;; key-chord-multiple.el ends here
