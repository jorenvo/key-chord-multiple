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

;; test 2
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
