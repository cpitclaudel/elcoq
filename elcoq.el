;;; elcoq.el --- Blah                                -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit--Claudel <clement.pit-claudel@live.com>
;; Keywords: convenience, languages
;; URL: https://github.com/cpitclaudel/el-coq
;; Keywords: convenience, languages
;; Package-Requires: ((dash "2.12.1") (cl-lib "0.5"))
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)
(require 'cl-lib)

(defmacro elcoq-alist-get (key alist)
  "Copy of Emacs 25's `alist-get', minus default.
Get the value associated to KEY in ALIST, or nil."
  (declare (debug t))
  `(cdr (assq ,key ,alist)))

(defmacro elcoq-alist-put (key alist value)
  "Set binding of KEY in ALIST to VALUE."
  (declare (debug t))
  `(setcdr (assq ,key ,alist) ,value))

(defvar elcoq-sertop-path "/build/coq-serapi/sertop.native"
  "Path to sertop.")

(defvar elcoq-coq-directory "/build/coq/"
  "Location of the directory containing Coq's sources, or nil.")

(defun elcoq--sertop-args ()
  "Compute sertop arguments."
  `("-print0"
    ,@(when elcoq-coq-directory
        `("-prelude" ,elcoq-coq-directory))))

;; Alists are nicer to use than defstructs, and extensible.

(defun elcoq--query (query callback)
  "Create a query alist.
QUERY and CALLBACK are stored."
  `((query . ,query)
    (callback . ,callback)))

(defun elcoq--prover (process buffer)
  "Create a prover alist.
The resulting alist has the following fields:
- `process' (populated from PROCESS), a process object.
- `buffer' (populated from BUFFER), the reference buffer of this prover.
- `fresh-id', a fresh id for future queries.
- `timer', a timer used to re-fire the dispatching event after running a
  synchronous query.
- `queries', a hash of query-id → in-flight query.
- `tip', state ID of the current tip.
- `overlays', a hash of state-id → overlay.
- `in-flight-overlays', a list of overlays that have not been attributed state
  ids yet, from latest to newest
- `pending-overlays', a list of overlays that haven't been sent yet.
- `accumulator', a list of message chunks, in reverse order.
- `messages', a list of complete messages that haven't been dispatched yet.
- `synchronous-query-id', the ID of the current synchronous query, if any."
  `((process . ,process)
    (buffer . ,buffer)
    (fresh-id . 0)
    (timer . 0)
    (queries . ,(make-hash-table :test 'eq))
    (tip . nil)
    (overlays . ,(make-hash-table :test 'eq))
    (in-flight-overlays . nil) ;; TODO could use a queue
    (pending-overlays . nil) ;; TODO could use a queue
    (accumulator . nil)
    (messages . nil)
    (synchronous-query-id . nil)))

(defmacro elcoq--with-prover (prover &rest body)
  "`let-alist'-bind PROVER, change to its buffer, and run BODY."
  (declare (debug t)
           (indent 1))
  `(when ,prover
     (let-alist ,prover
       (with-current-buffer .buffer
         ,@body))))

(defvar-local elcoq--prover nil
  "Buffer-local `elcoq--prover' object.
The same prover may be shared by multiple buffer; for this
reason, all prover state should be stored in that object.")

(defun elcoq--sertop-fresh-id ()
  "Return a fresh query ID."
  (1- (cl-incf (elcoq-alist-get 'fresh-id elcoq--prover))))

(defun elcoq-running-p ()
  "Check if an instance of sertop is running in current buffer."
  (and elcoq--prover
       (let-alist elcoq--prover
         (process-live-p .process))))

(defun elcoq--sertop-message-query-id (msg)
  "Read query id of message MSG."
  (pcase msg
    (`(Answer ,tag ,_) tag)))

(defun elcoq--sertop-may-dispatch-id (id)
  "Check if id ID may be dispatched.
Takes `synchronous-query-id' into account."
  (elcoq--with-prover elcoq--prover
    (or (null .synchronous-query-id)
        (eq id .synchronous-query-id))))

(defun elcoq--sertop-may-dispatch (msg)
  "Check if message MSG may be dispatched.
Takes `synchronous-query-id' into account."
  (elcoq--sertop-may-dispatch-id (elcoq--sertop-message-query-id msg)))

(defun elcoq--sertop-dispatch-answer (answer id)
  "Handle ANSWER to query with id ID."
  (elcoq--with-prover elcoq--prover
    (cl-assert (elcoq--sertop-may-dispatch-id id))
    (let* ((query (gethash id .queries)))
      (unless query
        (error "Invalid query ID %S in response" id))
      (pcase answer
        (`Ack (message "Acked %S" id))
        ;; FIXME decide when to clear the synchronous-query-id, in particular
        ;; because not all queries get a Completed message.
        (`Completed (message "Completed %S" id)) ;; FIXME
        (answer
         (message "Query %S got answer %S" id answer)
         (let ((callback (elcoq-alist-get 'callback query)))
           (unwind-protect
               (when (functionp callback)
                 (funcall callback answer)))
           (elcoq-alist-put 'synchronous-query-id elcoq--prover nil)))))))

(defun elcoq--sertop-set-overlay-status (overlay status)
  "Set tracking overlay OVERLAY's status to STATUS."
  (when overlay
    (overlay-put overlay 'elcoq--status status)
    (overlay-put overlay 'face
                 `(:background ,(pcase status
                                  (`Pending "grey")
                                  (`Sent "brown")
                                  (`ProcessingIn "blue")
                                  (`Processed "green"))))))

(defun elcoq--sertop-dispatch-feedback (feedback)
  "Dispatch feedback object FEEDBACK, updating overlays as needed."
  (pcase feedback
    (`(Feedback
       ((id (State ,state))
        (contents ,contents)
        (route ,_))) ;; FIXME what is route?
     (message "Feedback on state %S: %S" state contents)
     (elcoq--with-prover elcoq--prover
       (let ((overlay (gethash state .overlays)))
         (pcase contents
           (`(ProcessingIn ,_)
            (elcoq--sertop-set-overlay-status overlay 'ProcessingIn))
           (`Processed
            (elcoq--sertop-set-overlay-status overlay 'Processed))
           (other
            (message "Unrecognized feedback contents %S" other))))))))

(defun elcoq--sertop-dispatch-1 (msg)
  "Dispatch message MSG."
  (elcoq--with-prover elcoq--prover
    (pcase msg
      (`(Feedback ,feedback) (elcoq--sertop-dispatch-feedback feedback))
      (`(Answer ,tag ,answer) (elcoq--sertop-dispatch-answer answer tag))
      (response (error "Unsupported response %S" response)))))

(defun elcoq--sertop-dispatch (prover)
  "Dispatch messages in PROVER.
All messages are dispatched, unless
`.sertop-synchronous-query-id' is set.  In that case, only
the relevant messages are dispatched, and a timer is set to
re-run this function."
  ;; FIXME decide what to do with Feedback messages
  (elcoq--with-prover prover
    (message "Synchronous query is %S\nDispatching %S"
             .sertop-synchronous-query-id
             (pp-to-string .messages))
    (when (process-live-p .process)
      (let ((dispatchable-messages nil)
            (leftover-messages nil))
        (dolist (msg .messages)
          (if (elcoq--sertop-may-dispatch msg)
              (push msg dispatchable-messages)
            (push msg leftover-messages)))
        (message "May dispatch %s\nMay not dispatch %s"
                 (pp-to-string dispatchable-messages)
                 (pp-to-string leftover-messages))
        (elcoq-alist-put 'messages prover (nreverse leftover-messages))
        (mapc #'elcoq--sertop-dispatch-1 dispatchable-messages)
        (message "Remaining messages: %S" (elcoq-alist-get 'messages prover))
        (when (and nil leftover-messages) ;; FIXME
          ;; FIXME reset to 0
          (elcoq-alist-put 'timer prover (run-with-idle-timer 1 nil #'elcoq--sertop-dispatch prover)))))))

(defun elcoq--sertop-filter (proc string)
  "Accumulate PROC's output STRING, and act on full responses."
  ;; FIXME handle dead buffer case
  (message "Filter got %S" string)
  (-when-let* ((buf (process-get proc 'elcoq--buffer))
               (prover (buffer-local-value 'elcoq--prover buf)))
    (elcoq--with-prover prover
      (let ((parts (split-string string "\0" nil)))
        (while (consp (cdr parts))
          (push (pop parts) .accumulator)
          (let ((msg-string (apply #'concat (nreverse .accumulator))))
            (push (read msg-string) .messages))
          (setq .accumulator nil))
        (push (car parts) .accumulator)
        ;; Save back into prover object
        (elcoq-alist-put 'accumulator prover .accumulator)
        (elcoq-alist-put 'messages prover .messages))
      (elcoq--sertop-dispatch prover))))

(defun elcoq--sertop-start ()
  "Start a new sertop.
Does not kill existing instances.  Use `elcoq-run' for that."
  ;; FIXME Disallow in goal and response.
  (let ((proc (apply #'start-process "coq" nil
                     elcoq-sertop-path (elcoq--sertop-args))))
    (process-put proc 'elcoq--buffer (current-buffer))
    (set-process-filter proc #'elcoq--sertop-filter)
    (setq elcoq--prover (elcoq--prover proc (current-buffer)))))

(defun elcoq--sertop-ensure ()
  "Ensure that an instance of sertop is running in current buffer."
  (unless (elcoq-running-p)
    (elcoq--sertop-start)))

(defun elcoq--sertop-query (sexp callback)
  "Run query SEXP in current buffer, calling CALLBACK on the result.
Return the query's ID."
  (elcoq--sertop-ensure)
  (let* ((id (elcoq--sertop-fresh-id))
         (query-string (prin1-to-string (list id sexp))))
    (elcoq--with-prover elcoq--prover
      (message "Sending %S" query-string)
      (puthash id (elcoq--query sexp callback) .queries)
      (process-send-string .process query-string)
      (process-send-string .process "\n")
      id)))

(defun elcoq--sertop-query-synchronously (sexp)
  "Run query SEXP synchronously in current buffer."
  (elcoq--with-prover elcoq--prover
    (cl-assert (null .synchronous-query-id))
    (let* ((response nil)
           (callback (lambda (resp)
                       (message "Callback fired with %S" resp)
                       (setq response resp)))
           (id (elcoq--sertop-query sexp callback)))
      (elcoq-alist-put 'synchronous-query-id elcoq--prover id)
      (while (elcoq-alist-get 'synchronous-query-id elcoq--prover)
        ;; Wait until query is done processing
        (accept-process-output))
      (cl-assert response)
      response)))

(defun elcoq-run ()
  "Run Coq using sertop.
If needed, kill the existing sertop process."
  (interactive)
  (elcoq-kill)
  (elcoq--sertop-ensure))

(defun elcoq--sertop-kill-process ()
  "Kill sertop process."
  (elcoq--with-prover elcoq--prover
    (set-process-filter .process #'ignore)
    (when (process-live-p .process)
      (kill-process .process))
    (let ((timer (elcoq-alist-get 'timer elcoq--prover)))
      (when (timerp timer)
        (cancel-timer timer)))
    (setq elcoq--prover nil)))

(defun elcoq--remove-overlays ()
  "Remove all elcoq overlays in current buffer."
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'elcoq--status)
        (delete-overlay ov)))))

(defun elcoq-kill ()
  "Kill sertop process and clean up buffer."
  (interactive)
  (elcoq--sertop-kill-process)
  (elcoq--remove-overlays))

(defun elcoq--queries-StmState ()
  "Construct an StmState query."
  `(Control StmState))

(defun elcoq--sertop-ensure-tip (&optional force)
  "Ensure that tip is known.
With FORCE, refresh by querying sertop."
  (elcoq--with-prover elcoq--prover
    (unless (and .tip (not force))
      (let ((resp (elcoq--sertop-query-synchronously (elcoq--queries-StmState))))
        (pcase resp
          (`(StmInfo ,tip ())
           (elcoq-alist-put 'tip elcoq--prover tip))
          (_ (error "Unexpected answer %S" resp)))))))

(defun elcoq-queue-up-to (pos)
  "Queue sentences up to POS (interactively, up to point) ."
  (interactive "d")
  (elcoq--sertop-ensure)
  (let ((ov (make-overlay 1 pos nil)))
    (elcoq--sertop-set-overlay-status ov 'Pending)
    (elcoq-alist-put 'pending-overlays elcoq--prover
                (append
                 (elcoq-alist-get 'pending-overlays elcoq--prover)
                 (list ov)))))

(defun elcoq-buffers ()
  "Return a list of goal and response buffers, creating them if needed."
  (list (get-buffer-create "*Goal*")
        (get-buffer-create "*Response*")))

(defun elcoq-layout ()
  "Layout windows of current frame."
  (interactive)
  (delete-other-windows)
  (with-selected-window (split-window-horizontally)
    (pcase-let ((`(,goalw ,respw) (elcoq-buffers)))
      (switch-to-buffer goalw)
      (with-selected-window (split-window)
        (switch-to-buffer respw)))))

(defvar elcoq--debug nil)

(defun elcoq-toggle-debug ()
  "Toggle elcoq debugging."
  (interactive)
  (require 'trace)
  (setq elcoq--debug (not elcoq--debug))
  (mapatoms (lambda (func)
              (when (and (fboundp func)
                         (string-match-p "^elcoq-" (symbol-name func))
                         (not (eq (car-safe (symbol-function func)) 'macro)))
                (if elcoq--debug
                    (trace-function func)
                  (with-no-warnings (untrace-function func))))))
  (message "elcoq debugging: %S" elcoq--debug))

(provide 'elcoq)
;;; elcoq.el ends here
