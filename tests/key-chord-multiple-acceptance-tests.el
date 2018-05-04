(require 'dash)
(require 'key-chord-multiple)
(key-chord-mode 1)

(setq current-test 0)
(setq test-results-directory "test_results")
(make-directory test-results-directory)

(defun get-new-test-file ()
  (setq current-test (+ current-test 1))
  (format "%s/%d.txt" test-results-directory current-test))

(defun write-current-line-to-file ()
  (interactive)
  (write-region (line-beginning-position) (line-end-position) (get-new-test-file))
  (erase-buffer))

(global-set-key (kbd ",") 'write-current-line-to-file)

(defun write-string-to-file (string)
  (write-region string nil (get-new-test-file)))

(defun append-to-unread-command-events (keys)
  (setq unread-command-events (append unread-command-events (listify-key-sequence keys))))

;; test 1
(defun test-1 ()
  (interactive)
  (write-string-to-file "test-1 success"))
(key-chord-define-global "ab" 'test-1)

;; TODO test 2
;; writes two chars followed by ,

;; test 3
;; ensure that timeout works
(defun test-3 ()
  (interactive)
  (write-string-to-file "test-3 failure"))
(key-chord-define-global "ef" 'test-3)

;; test 4
;; ensure that pressing a key not part of a key-chord replays all
;; buffered characters
(defun test-4 ()
  (interactive)
  (write-string-to-file "test-4 failure"))
(key-chord-define-global "ghijklmn" 'test-4)

;; test 5
;; test whether we can bind to lambda
(key-chord-define-global "op" (lambda ()
                                (interactive)
                                (write-string-to-file "test-5 success")))

;; test 6
;; test whether we can execute a single key key-chord in a keyboard macro
(key-chord-define-global "q" (lambda ()
                               (interactive)
                               (insert "test-6 success")))

;; test 7
;; test whether we can execute a multi key key-chord in a keyboard macro
(key-chord-define-global "rst" (lambda ()
                                 (interactive)
                                 (insert "test-7 success")))

;; test 8
;; test switch-to-buffer, C-g, letter
(key-chord-define-global "uv" 'switch-to-buffer)

;; test 9
;; test if a command gets repeated by pressing the last chord key again
;; sdf
(setq test-9-counter 0)
(key-chord-define-global "sdf" (lambda ()
                                 (interactive)
                                 (if (> test-9-counter 0)
                                     (insert "test-9 success"))
                                 (setq test-9-counter (+ test-9-counter 1))))
