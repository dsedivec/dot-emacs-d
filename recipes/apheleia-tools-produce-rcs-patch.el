;; -*- lexical-binding: t; -*-

;; https://github.com/raxod502/apheleia/issues/42
;;
;; This allows Apheleia to directly accept RCS patch output from
;; tools.  I use this along with Darker (Python formatter) and a
;; little script I wrote to convert unified -> RCS diff format.
;;

(require 'el-patch)

(el-patch-feature apheleia)

(with-eval-after-load 'apheleia
  (el-patch-defun apheleia-format-buffer (command &optional callback)
    "Run code formatter asynchronously on current buffer, preserving point.

Interactively, run the currently configured formatter (see
`apheleia-formatter' and `apheleia-mode-alist'), or prompt from
`apheleia-formatters' if there is none configured for the current
buffer. With a prefix argument, prompt always.

In Lisp code, COMMAND is similar to what you pass to
`make-process', except as follows. Normally, the contents of the
current buffer are passed to the command on stdin, and the output
is read from stdout. However, if you use the symbol `file' as one
of the elements of COMMAND, then the filename of the current
buffer is substituted for it. (Use `filepath' instead of `file'
if you need the filename of the current buffer, but you still
want its contents to be passed on stdin.) If you instead use the
symbol `input' as one of the elements of COMMAND, then the
contents of the current buffer are written to a temporary file
and its name is substituted for `input'. Also, if you use the
symbol `output' as one of the elements of COMMAND, then it is
substituted with the name of a temporary file. In that case, it
is expected that the command writes to that file, and the file is
then read into an Emacs buffer. Finally, if you use the symbol
`npx' as one of the elements of COMMAND, then the first string
element of COMMAND is resolved inside node_modules/.bin if such a
directory exists anywhere above the current `default-directory'.

In any case, after the formatter finishes running, the diff
utility is invoked to determine what changes it made. That diff
is then used to apply the formatter's changes to the current
buffer without moving point or changing the scroll position in
any window displaying the buffer. If the buffer has been modified
since the formatter started running, however, the operation is
aborted.

If the formatter actually finishes running and the buffer is
successfully updated (even if the formatter has not made any
changes), CALLBACK, if provided, is invoked with no arguments."
    (interactive (progn
                   (when-let ((err (apheleia--disallowed-p)))
                     (user-error err))
                   (list (apheleia--get-formatter-command
                          (if current-prefix-arg
                              'prompt
                            'interactive)))))
    ;; Fail silently if disallowed, since we don't want to throw an
    ;; error on `post-command-hook'.
    (unless (apheleia--disallowed-p)
      (setq-local apheleia--buffer-hash (apheleia--buffer-hash))
      (let ((cur-buffer (current-buffer))
            (el-patch-add (output-is-rcs (when (eq (car command) 'rcs)
                                           (pop command)))))
        (apheleia--run-formatter
         command
         (lambda (formatted-buffer)
           (with-current-buffer cur-buffer
             ;; Short-circuit.
             (when (equal apheleia--buffer-hash (apheleia--buffer-hash))
               (el-patch-let (($callback (when callback
                                           (funcall callback))))
                 (el-patch-wrap 2 2
                   (if (not output-is-rcs)
                       (apheleia--create-rcs-patch
                        (current-buffer) formatted-buffer
                        (lambda (patch-buffer)
                          (with-current-buffer cur-buffer
                            (when (equal apheleia--buffer-hash (apheleia--buffer-hash))
                              (apheleia--apply-rcs-patch
                               (current-buffer) patch-buffer)
                              $callback))))
                     (apheleia--apply-rcs-patch
                      (current-buffer) formatted-buffer)
                     $callback))))))))))

  (el-patch-validate 'apheleia-format-buffer 'defun t))
