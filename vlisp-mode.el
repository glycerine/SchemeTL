;;; vlisp-2.4
;;; (Setq vlisp-version-string "2.4") ; update this below when changing.
;;;
;;; vlisp-mode.el - is a pair of modes for sending lines from a 
;;;                  script (sender) to a comint-started inferior 
;;;                  (receiver) process. We  use Ctrl-u by default as 
;;;                  the "send this line and step" key. This enables 
;;;                  one to step through scripts easily when working
;;;                  with an interpreter, such as Pure, R, Haskell,
;;;                  shell, Python, and so forth.
;;;
;;; License:      This minimal pair of (major and inferior) modes
;;;               was derived from the Emacs Octave Support modes in 
;;;               octave-mod.el and octave-inf.el, with a little help 
;;;               (and alot of inspiration) from the ess-mode.el for
;;;               R. As such it falls under the GNU General Public
;;;               License version 3 or later.
;;;
;;; Copyright (C) 1998, Author: Jason E. Aten
;;;
;;; how to install:
;;;
;;; (1) If you are changing which interpreter you want to use, examine
;;;     and adjust the "commonly-adjusted parameters" section just below.
;;;
;;; (2) Copy vlisp-mode.el into your ~/.emacs.d/  directory.
;;;
;;; (3) Then put this in your .emacs:
;;;
;;; (require 'vlisp-mode)
;;;
;;; (4) Optionally for speed, M-x byte-compile-file <enter> 
;;;                           ~/.emacs.d/vlisp-mode.el <enter>
;;;
;;; (5) To use, do 'M-x run-vlisp' to start the interpreter. Open
;;;     your script and in the script buffer do 'M-x vlisp-mode'.
;;;
;;;     Or, open a file with an automatically recognized extension
;;;     (as specified below) and press 'C-u' on the first line
;;;     you want executed in the interpreter.


;; To over-ride parameters, skip down to the "commonly-adjusted
;; parameters" section below these declarations.
;;
;; Here we just document the typically overriden parameters of interest.
;; NB: these just give defaults, which the setq settings below will override.
;; Putting them here keeps the byte-compiler from complaining.

(defvar inferior-vlisp-program "/usr/local/bin/lisp"
  "Program invoked by `inferior-vlisp'.")

(defvar inferior-vlisp-buffer "*vlisp*"
  "*Name of buffer for running an inferior Vlisp process.")

(defvar inferior-vlisp-regex-prompt "[^>]*> "
  "Regexp to match prompts for the inferior Vlisp process.")

(defvar vlisp-keypress-to-sendline (kbd "C-u")
  "keypress that, when in a pure mode script, sends a line to the interpreter
   and then steps to the next line.")

(defvar vlisp-keypress-to-send-sexp-jdev      (kbd "C-n")
  "keypress that sends the next sexp to the repl, and advances past it.")

(defvar vlisp-keypress-to-send-sexp-jdev-prev (kbd "C-p")
  "keypress that sends the previous sexp to the repl")

(defvar vlisp-version-string "2.4"
  "version of vlisp currently running.")

;; ================================================
;; ================================================
;; begin commonly-adjusted parameters

;; the name of the interpreter to run
(setq inferior-vlisp-program "/usr/local/bin/lisp")

;; the name of the buffer to run the interpreter in
(setq inferior-vlisp-buffer "*vlisp*")

;; file extensions that indicate we should automatically enter vlisp mode...
(setq auto-mode-alist (cons '("\\.lisp$" . vlisp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cl$" . vlisp-mode) auto-mode-alist))

;; regexp to know when interpreter output is done and we are at a prompt.
(setq inferior-vlisp-regex-prompt  "[^>]*> ")

;; keypress that, when in a pure mode script, sends a line to the interpreter
;;  and then steps to the next line.
(setq vlisp-keypress-to-sendline (kbd "C-u"))

;; keypress that, when in a pure mode script, sends an sexp to the JDEV repl
;;  and then steps to the next line.
(setq vlisp-keypress-to-send-sexp-jdev      (kbd "C-n"))
(setq vlisp-keypress-to-send-sexp-jdev-prev (kbd "C-p"))

;; end commonly-adjusted parameters
;; ================================================
;; ================================================


(defvar inferior-vlisp-output-list nil)
(defvar inferior-vlisp-output-string nil)
(defvar inferior-vlisp-receive-in-progress nil)
(defvar inferior-vlisp-process nil)

(defvar vlisp-mode-hook nil
  "*Hook to be run when Vlisp mode is started.")

(defvar vlisp-send-show-buffer t
  "*Non-nil means display `inferior-vlisp-buffer' after sending to it.")

(defvar vlisp-send-line-auto-forward t
  "*Control auto-forward after sending to the inferior Vlisp process.
Non-nil means always go to the next Vlisp code line after sending.")

(defvar vlisp-send-echo-input t
  "*Non-nil means echo input sent to the inferior Vlisp process.")


;;; Motion
(defun vlisp-next-code-line (&optional arg)
  "Move ARG lines of Vlisp code forward (backward if ARG is negative).
Skips past all empty and comment lines.  Default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (let ((n 0)
	(inc (if (> arg 0) 1 -1)))
    (while (and (/= arg 0) (= n 0))
      (setq n (forward-line inc))
      (while (and (= n 0)
		  (looking-at "\\s-*\\($\\|\\s<\\)"))
	(setq n (forward-line inc)))
      (setq arg (- arg inc)))
    n))

(defun vlisp-previous-code-line (&optional arg)
  "Move ARG lines of Vlisp code backward (forward if ARG is negative).
Skips past all empty and comment lines.  Default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (vlisp-next-code-line (- arg)))


;;; Communication with the inferior Vlisp process
(defun vlisp-kill-process ()
  "Kill inferior Vlisp process and its buffer."
  (interactive)
  (if inferior-vlisp-process
      (progn
	(process-send-string inferior-vlisp-process "quit;\n")
	(accept-process-output inferior-vlisp-process)))
  (if inferior-vlisp-buffer
      (kill-buffer inferior-vlisp-buffer)))

(defun vlisp-show-process-buffer ()
  "Make sure that `inferior-vlisp-buffer' is displayed."
  (interactive)
  (if (get-buffer inferior-vlisp-buffer)
      (display-buffer inferior-vlisp-buffer)
    (message "No buffer named %s" inferior-vlisp-buffer)))

(defun vlisp-hide-process-buffer ()
  "Delete all windows that display `inferior-vlisp-buffer'."
  (interactive)
  (if (get-buffer inferior-vlisp-buffer)
      (delete-windows-on inferior-vlisp-buffer)
    (message "No buffer named %s" inferior-vlisp-buffer)))

(defun vlisp-send-region (beg end)
  "Send current region to the inferior Vlisp process."
  (interactive "r")
  (inferior-vlisp t)
  (let ((proc inferior-vlisp-process)
	(string (buffer-substring-no-properties beg end))
	line)
    (with-current-buffer inferior-vlisp-buffer 
      (setq inferior-vlisp-output-list nil)
      (while (not (string-equal string ""))
	(if (string-match "\n" string)
	    (setq line (substring string 0 (match-beginning 0))
		  string (substring string (match-end 0)))
	  (setq line string string ""))
	(setq inferior-vlisp-receive-in-progress t)
	(inferior-vlisp-send-list-and-digest (list (concat line "\n")))
	(while inferior-vlisp-receive-in-progress
	  (accept-process-output proc))
	(insert-before-markers
	 (mapconcat 'identity
		    (append
		     (if vlisp-send-echo-input (list line) (list ""))
		     (mapcar 'inferior-vlisp-strip-ctrl-g
			     inferior-vlisp-output-list)
		     (list inferior-vlisp-output-string))
		    "\n")))))
  (if vlisp-send-show-buffer
      (display-buffer inferior-vlisp-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the entire point of vlisp-mode : to enable the use
;; of vlisp-send-line with a single keypress.
;;
(defun vlisp-send-line (&optional arg)
  "Send current Vlisp code line to the inferior Vlisp process.
With positive prefix ARG, send that many lines.
If `vlisp-send-line-auto-forward' is non-nil, go to the next unsent
code line."
  (interactive "P")
  (or arg (setq arg 1))
  (if (> arg 0)
      (let (beg end)
	(beginning-of-line)
	(setq beg (point))
	(vlisp-next-code-line (- arg 1))
	(end-of-line)
	(setq end (point))
	(if vlisp-send-line-auto-forward
	    (vlisp-next-code-line 1))
	(vlisp-send-region beg end))))

;;;;;;; sexp-based version -- hard code in inferior-vlisp-buffer, and remove SLIME dependencies.

;;;;;;;;;;;  vlisp-advance-and-eval-sexp-jdev    and   vlisp-advance-and-eval-sexp-jdev-prev


(defun nil-to-point-max (x)
  (if x 
      x 
    (point-max)
))

(defun nil-to-point-min (x)
  (if x 
      x 
    (point-min)
))

(defun skip-lisp-comment-lines ()
  "skip over lines that start with semi-colons before they have another non-whitespace character"
  (interactive)
  (let* ((done-skipping)
	 (startp (point))
	 (nextcomment)
	 (eol)
	 (nextword)
	 )
    ;; 
    ;; if nextcomment < eol and ( nextword > nextcomment or nextword is nil ) then skip to next line

    ;; conversely, if nextword < eol and (nextcomment is nil or nextword < nextcomment) then stop skipping lines
    (while (not done-skipping)
      (setq startp        (point))
      (setq nextcomment   (nil-to-point-max (search-forward ";" nil 0 1)))
      (setq eol           (progn (goto-char startp) (nil-to-point-max (search-forward "\n" nil 0 1))))
      (setq nextword      (progn (goto-char startp) (+ (point) (skip-chars-forward "\t ;\n"))))
      
      ;; either stop at the word, or go to the end of line
      (if (< nextword eol)
	  (if (or (< nextword nextcomment)
		  (= (point-max) nextcomment))
	      
	      (progn
		(setq done-skipping t)
		(goto-char nextword)
		)

	    (goto-char eol))
	(progn
	  (when (= nextword eol)
	      (setq done-skipping t) ;; stop if nextword == eol
	      )
	  (goto-char eol)
	  )
	)
      )
    )
  )

(defvar DEBUG_STATUS 'off)

;(setq DEBUG_STATUS 'on)
(setq DEBUG_STATUS 'off)

(defmacro DEBUG (&rest body)
  "DEBUG is simple call to (@body) or nil, depending on the value of DEBUG_STATUS being 'on or not. 
Makes it easy to debug when needed. You will have to recompile, of course, in order for the
macro to be able to take effect, if you change the value of DEBUG_STATUS. For efficiency's
sake, this is a compile time, not a runtime, convenience."
  (if (eql DEBUG_STATUS 'on)
      `(,@body)
    'nil)
)

;; test
;; (DEBUG message "startp       is %s" startp)
;; (macroexpand '(DEBUG message "startp       is %s" startp))


(defun line-is-comment-or-whitespace ()
  "determine if this line can be ignored because it is just a comment or whitespace"
  (interactive)
  (DEBUG message "***************** starting: (line-is-comment-or-whitespace), at %S" (line-number-at-pos))
  (block block-line-is-comment-or-whitespace
  (let* ((startp (point))
	 (nextword nil)
	 (nextcomment nil)
	 (bol nil)
	 (eol nil)
	 (prevbol nil)
	 )

      (setq startp        (point))
      (setq bol           (progn (goto-char startp)                         (nil-to-point-min (search-backward "\n" 0 0 1))))
      (setq prevbol       (progn (goto-char (max (point-min) (- bol 1)))    (nil-to-point-min (search-backward "\n" 0 0 1))))
      (setq nextcomment   (progn (goto-char bol)                            (nil-to-point-max (search-forward ";" nil 0 1))))
      (setq eol           (progn (goto-char startp)                         (nil-to-point-max (search-forward "\n" nil 0 1))))
      (setq nextword      (progn (goto-char bol)                            (+ (point) (skip-chars-forward "\t ;\n"))))

      (goto-char startp)

      (DEBUG message "startp       is %s" startp)
      (DEBUG message "bol          is %s" bol)
      (DEBUG message "eol          is %s" eol)
      (DEBUG message "prevbol      is %s" prevbol)
      (DEBUG message "nextcomment  is %s" nextcomment)
      (DEBUG message "nextword     is %s" nextword)

      ;; when startp == 1 + bol, and nextcomment == 1 + startp, then we have a line of all comments
      (when (and (= startp (+ 1 bol))
		 (= nextcomment (+ 1 startp)))
	(DEBUG message "line is empty, returning t early")
	(return-from block-line-is-comment-or-whitespace t))

      ;; sanity check for empty lines
      (when (and (= eol (+ 1 startp))
		 (= bol (- startp 1))
		 )
	(progn 
	  (DEBUG message "line is empty, returning t early")
	  (return-from block-line-is-comment-or-whitespace t)))


      ;; if nextword    > eol this is skippable.
      (when (> nextword eol)
	(progn
	  (DEBUG message "nextword > eol, returning t early")
	  (return-from block-line-is-comment-or-whitespace t)))


      ;; INVAR: bol < nextword < eol, only question left: is there a comment before the nextword?

      (if (or (> nextcomment eol)
	      (< nextcomment bol))
	  
	  ;; nextcomment is not in play
	  (progn
	    (DEBUG message "nil: cannot skip b/c bol < nextword < eol, and no comment present on this line.")
	    (return-from block-line-is-comment-or-whitespace nil))
	
	;; INVAR: comment is in play, and may obscucate the entire line.
	(if (<= nextcomment nextword)
	    (progn
	      (DEBUG message "t: can skip")
	      (return-from block-line-is-comment-or-whitespace t))
	  
	  ;;
	  (progn
	    (DEBUG message "nil: cannot skip b/c bol < nextword < nextcomment < eol.")
	    (return-from block-line-is-comment-or-whitespace nil)))

	) ;; endif 
)))


(defun skip-lisp-comment-lines-backwards ()
  "Going backwards, skip over lines that start with semi-colons before they have another non-whitespace character.
The main side-effect is to reposition point. The function returns the new position of point, 
which is just following the next form back."

  (interactive)
  (DEBUG message "***************** starting: (skip-lisp-comment-lines-backwards)")
  (block block-skip-lisp-comment-lines-backwards
    (DEBUG message "--> point is starting at %s" (point))
    (let* ((startp (point))
	   (next-word-back)
	   (bol)
	   (nextcomment)
	   (eol)
	   (start-backwards-search)
	   (starting-line (line-number-at-pos))
	   (cur-line      starting-line)
	   )
      
      ;; back up until we find a line with something like a lisp form on it (and the whole line is not commented out)
      (beginning-of-line)

      ;; handle the case of starting at the beginning of a non-comment line, by backing up one line before we search...
      (when (and (= (point) startp)
		 (> startp  (point-min)))
	;; we started at the beginning of a line, and it's not the first line, so back up past the newline to prev line.
	(goto-char (- startp 1))
	(beginning-of-line))
      
      ;; main backup while loop
      (while (and (line-is-comment-or-whitespace)
		  (> (point) (point-min)))
	(forward-line -1)
	)

      ;; if we have moved lines, reset to our new starting place...
      (setq cur-line (line-number-at-pos))
      (if (= cur-line starting-line)
	  (goto-char startp)
	(progn
	  (end-of-line)
	  (setq startp (point))))
      (DEBUG message "--> After revision of backing up past comments, point is at %s" (point))


      ;; INVAR: we are on a line with some content, or we are at the beginning of the buffer
      (when (line-is-comment-or-whitespace)
	;; beginning of buffer, just get to the start.
	(goto-char (point-min)) 
	(return-from block-skip-lisp-comment-lines-backwards (point-min)))
	  
      (DEBUG message "--> INVAR: we are on a line with some content")
    
      (setq bol           (progn (goto-char startp)                         (nil-to-point-min (search-backward "\n" 0 0 1))))
      (setq nextcomment   (progn (goto-char bol)                            (nil-to-point-max (search-forward ";" nil 0 1))))
      (setq eol           (progn (goto-char startp)                         (nil-to-point-max (search-forward "\n" nil 0 1))))

      ;; start from eol, or from nextcomment if nextcomment is < eol
      (setq start-backwards-search eol)
      (when (< nextcomment eol)
	(setq start-backwards-search nextcomment))

      (setq next-word-back 
	    (progn 
	      (goto-char start-backwards-search)    
	      (+ 
	       (point) 
	       (skip-chars-backward "\t ;\n"))))

      (goto-char next-word-back)
)))

;; debug bindings
;;  (global-set-key "\C-o" 'skip-lisp-comment-lines-backwards)
;;  (global-set-key "\C-o" 'line-is-comment-or-whitespace)
;;  (global-set-key "\C-o"   'my-backward-sexp-ignoring-comments)
  (global-set-key "\C-o"   '(forward-line -1))


  
  (defun my-forward-sexp-ignoring-comments ()
  "Move forward across one balanced expression (sexp), ignoring ; comments"
  (interactive "^p")

  ;; first determine if we are in a comment: iff there is a semi-colon between (point) and previous newline or beginning of buffer
  ;; if we are in a comment, then go to the next line.
  ;; now skip over any additional comments, arriving at the first uncommented sexp
    (setq startp (point))

    (search-backward "\n" 0 0 1) ;; side effect is to move point to beginning of line
    (setq bol    (point))

    ;; find previous comment
    (goto-char startp) ;; start over
    (search-backward ";" 0 0 1) 
    (setq prevcomment (point))

    ;; find previous double quote
    (goto-char startp) ;; start over
    (search-backward "\"" 0 0 1)
    (setq prevdoublequote (point))

    ;; find end of line
    (search-forward "\n" nil 0 1)
    (setq eol (point))
    (if (not (string= "\n" (buffer-substring startp (+ 1 startp))))
      (goto-char startp))

    ;; rely on the fact that we are now at the beginning of the next line, and check for comments.
    ;; If there is a comment character that is both >= bol and also > prevdoublequote, then skip to next line
    ;; otherwise start over and return to original starting position.
    (unless (and (>= prevcomment bol) (> prevcomment prevdoublequote))
	(goto-char startp))

    ;; INVAR: we are at the beginning of a line or at the beginning of a non-commented sexp.
    (skip-lisp-comment-lines) ;; call the above function to handle skipping to the next sexp.
    
    (forward-sexp)
    (skip-lisp-comment-lines) ;; get to the beginning of the next form
)


(defun my-backward-sexp-ignoring-comments ()
  "Move backward across one balanced expression (sexp), ignoring comments in the form of semicolons"
  (interactive)
  (DEBUG message "******** starting: my-backward-sexp-ignoring-comments")
  (skip-lisp-comment-lines-backwards)
  (if (line-is-comment-or-whitespace)
      (progn
	(DEBUG message "*************************** my-backward-sexp-ignoring-comments: finishing with (beginning-of-line) and t <<<<<")
	(beginning-of-line)
	t)
    (progn
      (DEBUG message "*************************** my-backward-sexp-ignoring-comments: finishing with (backward-sexp) and t <<<<<")
      (backward-sexp)
      t)))


(defun vlisp-advance-and-eval-sexp-jdev ()
       "advance one sexp, and copy that sexp into the
inferior-vlisp-buffer, so as to step through lines in a lisp .cl file"
       (interactive)
       (inferior-vlisp t)
       (setq clisp-buf inferior-vlisp-buffer)
       (setq pstart (point))
       (push-mark)
       (my-forward-sexp-ignoring-comments)
       (setq pend (point))
       ;;(setq str (buffer-substring pstart pend))
       (with-current-buffer clisp-buf (goto-char (point-max)))
       (append-to-buffer clisp-buf pstart pend)
       (with-current-buffer clisp-buf 
	 (setq comint-eol-on-send t)
	 (setq comint-process-echoes nil)
	 (setq comint-move-point-for-output t)
	 (comint-send-input)
	 (display-buffer clisp-buf)
	 (recenter)
))
	 

(defun vlisp-advance-and-eval-sexp-jdev-prev ()
       "copy the previous sexp into the inferior-vlisp-buffer"
       (interactive)
       (inferior-vlisp t)
       (setq clisp-buf inferior-vlisp-buffer)
       (if (and (my-backward-sexp-ignoring-comments)
		  (not (line-is-comment-or-whitespace)))
	   (progn
	     (setq pstart (point))
	     (push-mark)
	     (forward-sexp)
	     (setq pend (point))
	     (with-current-buffer clisp-buf (goto-char (point-max)))
	     (append-to-buffer clisp-buf pstart pend)
	     (with-current-buffer clisp-buf 
	       (setq comint-eol-on-send t)
	       (setq comint-process-echoes nil)
	       (setq comint-move-point-for-output t)
	       (comint-send-input)
	       (display-buffer clisp-buf)
	       (recenter)
	       )
	     ;;(skip-lisp-comment-lines)
	     )
	 (beginning-of-line)))


;;;;;;;;;;;; simplest possible major mode stuff

(defvar vlisp-mode-map nil
  "Local keymap for vlisp mode buffers.")

(setq vlisp-mode-map nil)

(if vlisp-mode-map
    nil
  (setq vlisp-mode-map (make-sparse-keymap))
  (define-key vlisp-mode-map vlisp-keypress-to-sendline 'vlisp-send-line)
  (define-key vlisp-mode-map vlisp-keypress-to-send-sexp-jdev 'vlisp-advance-and-eval-sexp-jdev)
  (define-key vlisp-mode-map vlisp-keypress-to-send-sexp-jdev-prev 'vlisp-advance-and-eval-sexp-jdev-prev)
)

(defun vlisp-mode ()
  "simple send-line, aka vlisp mode."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'vlisp-mode)
  (setq mode-name "vlisp")
  (use-local-map vlisp-mode-map)
  (run-mode-hooks 'jdev-mode-hook))


;;; provide ourself

;;;(provide 'vlisp-mod)

;;; vlisp-mod.el ends here


;;; vlisp-inf.el --- running Vlisp as an inferior Emacs process

;;(require 'vlisp-mod)
(require 'comint)

(defgroup vlisp-inferior nil
  "Running Vlisp as an inferior Emacs process."
  :group 'vlisp)




(defvar inferior-vlisp-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    map)
  "Keymap used in Inferior Vlisp mode.")

(defvar inferior-vlisp-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\` "w" table)
    table)
  "Syntax table in use in inferior-vlisp-mode buffers.")

(defvar inferior-vlisp-mode-hook nil
  "*Hook to be run when Inferior Vlisp mode is started.")


;;; Compatibility functions
(if (not (fboundp 'comint-line-beginning-position))
    ;; comint-line-beginning-position is defined in Emacs 21
    (defun comint-line-beginning-position ()
      "Returns the buffer position of the beginning of the line, after
any prompt. The prompt is assumed to be any text at the beginning of the 
line matching the regular expression `comint-prompt-regexp', a buffer local 
variable."
      (save-excursion (comint-bol nil) (point))))


(defvar inferior-vlisp-output-list nil)
(defvar inferior-vlisp-output-string nil)
(defvar inferior-vlisp-receive-in-progress nil)
(defvar inferior-vlisp-startup-hook nil)


(defun inferior-vlisp-mode ()
  "Major mode for interacting with an inferior Vlisp process.
Runs Vlisp as a subprocess of Emacs, with Vlisp I/O through an Emacs
buffer.

Entry to this mode successively runs the hooks `comint-mode-hook' and
`inferior-vlisp-mode-hook'."
  (interactive)
  (delay-mode-hooks (comint-mode))
  (setq comint-prompt-regexp inferior-vlisp-regex-prompt
	major-mode 'inferior-vlisp-mode
	mode-name "Inferior Vlisp"
	mode-line-process '(":%s"))
  (use-local-map inferior-vlisp-mode-map)
  (setq comint-input-ring-file-name
	(or (getenv "VLISP_HISTFILE") "~/.vlisp_hist")
	comint-input-ring-size (or (getenv "VLISP_HISTSIZE") 1024))
  (comint-read-input-ring t)

  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'vlisp-kill-buffer-function)

  (run-mode-hooks 'inferior-vlisp-mode-hook))

;;;###autoload
(defun inferior-vlisp (&optional arg)
  "Run an inferior Vlisp process, I/O via `inferior-vlisp-buffer'.
This buffer is put in Inferior Vlisp mode.  See `inferior-vlisp-mode'.

Unless ARG is non-nil, switches to this buffer.

The elements of the list `inferior-vlisp-startup-args' are sent as
command line arguments to the inferior Vlisp process on startup.

Additional commands to be executed on startup can be provided either in
the file specified by `inferior-vlisp-startup-file' or by the default
startup file, `~/.emacs-vlisp'."
  (interactive "P")
  (let ((buffer inferior-vlisp-buffer))
    (get-buffer-create buffer)
    (if (comint-check-proc buffer)
	()
      (with-current-buffer buffer
	(comint-mode)
	(inferior-vlisp-startup)
	(inferior-vlisp-mode)))
    (if (not arg)
	(pop-to-buffer buffer))))

;;;###autoload
(defalias 'run-vlisp 'inferior-vlisp)

(defun inferior-vlisp-startup ()
  "Start an inferior Vlisp process."
  (let ((proc (comint-exec-1
	       (substring inferior-vlisp-buffer 1 -1)
	       inferior-vlisp-buffer
	       inferior-vlisp-program
	       '(""))))
    (set-process-filter proc 'inferior-vlisp-output-filter)
    (setq comint-ptyp process-connection-type
	  inferior-vlisp-process proc
	  inferior-vlisp-output-list nil
	  inferior-vlisp-output-string nil
	  inferior-vlisp-receive-in-progress t)

    (run-hooks 'inferior-vlisp-startup-hook)
    (run-hooks 'inferior-vlisp-startup-hook)))


(defun inferior-vlisp-strip-ctrl-g (string)
  "Strip leading `^G' character.
If STRING starts with a `^G', ring the bell and strip it."
  (if (string-match "^\a" string)
      (progn
        (ding)
        (setq string (substring string 1))))
  string)

(defun inferior-vlisp-output-filter (proc string)
  "Standard output filter for the inferior Vlisp process.
Ring Emacs bell if process output starts with an ASCII bell, and pass
the rest to `comint-output-filter'."
  (comint-output-filter proc (inferior-vlisp-strip-ctrl-g string)))

(defun inferior-vlisp-output-digest (proc string)
  "Special output filter for the inferior Vlisp process.
Save all output between newlines into `inferior-vlisp-output-list', and
the rest to `inferior-vlisp-output-string'."
  (setq string (concat inferior-vlisp-output-string string))
  (while (string-match "\n" string)
    (setq inferior-vlisp-output-list
	  (append inferior-vlisp-output-list
		  (list (substring string 0 (match-beginning 0))))
	  string (substring string (match-end 0))))
  (if (string-match inferior-vlisp-regex-prompt string)
      (setq inferior-vlisp-receive-in-progress nil))
  (setq inferior-vlisp-output-string string))

(defun inferior-vlisp-send-list-and-digest (list)
  "Send LIST to the inferior Vlisp process and digest the output.
The elements of LIST have to be strings and are sent one by one.  All
output is passed to the filter `inferior-vlisp-output-digest'."
  (let* ((proc inferior-vlisp-process)
	 (filter (process-filter proc))
	 string)
    (set-process-filter proc 'inferior-vlisp-output-digest)
    (setq inferior-vlisp-output-list nil)
    (unwind-protect
	(while (setq string (car list))
	  (setq inferior-vlisp-output-string nil
		inferior-vlisp-receive-in-progress t)
	  (comint-send-string proc string)
	  (while inferior-vlisp-receive-in-progress
	    (accept-process-output proc))
	  (setq list (cdr list)))
      (set-process-filter proc filter))))


(defun vlisp-kill-buffer-function nil
  "Function run just before an VLISP process buffer is killed.
  This simply deletes the buffers process to avoid an Emacs bug
  where the sentinel is run *after* the buffer is deleted."
  (let ((proc (get-buffer-process (current-buffer))))
    (if proc (delete-process proc))))


;;; provide ourself


(provide 'vlisp-mode)

