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

(defmacro elcoq-alist-put (key value alist)
  "Set binding of KEY to VALUE in ALIST."
  (declare (debug t))
  `(setcdr (assq ,key ,alist) ,value))

(defconst elcoq--directory
  (file-name-directory (or (and load-in-progress load-file-name)
                           (bound-and-true-p byte-compile-current-file)
                           buffer-file-name)))

(defvar elcoq-coq-directory "/build/coq/"
  "Location of the directory containing Coq's sources, or nil.")

(defvar elcoq-async t
  "Whether to run sertop in async mode.")

(defun elcoq--sertop-args ()
  "Compute sertop arguments."
  `("--print0"
    ,@(when elcoq-async
        `("--async" ,(file-truename
                      (if elcoq-coq-directory
                          (expand-file-name "bin/coqtop" elcoq-coq-directory)
                        (executable-find "coqtop")))))
    ,@(when elcoq-coq-directory
        `("--prelude" ,elcoq-coq-directory))))

;; Alists are nicer to use than defstructs, and extensible.

(defun elcoq--make-query (sexp callback)
  "Create a query alist.
The resulting alist has the following fields:
- `sexp' (populated from SEXP), a query sexp.
- `callback' (populated from CALLBACK), a callback to fire when output or events
  are received for this query.  `callback' it always called in the prover's main
  buffer.  It should take two arguments: a type of even (one of `output', `ack',
  `done', or `failed') and optionally the actual output."
  `((sexp . ,sexp)
    (callback . ,callback)))

(defun elcoq--make-prover (process buffer)
  "Create a prover alist.
The resulting alist has the following fields:
- `process' (populated from PROCESS), a process object.
- `buffer' (populated from BUFFER), the reference buffer of this prover.
- `fresh-id', a fresh id for future queries.
- `timer', a timer used to re-fire the dispatching event after running a
  synchronous query.
- `queries', a hash of query-id → in-flight query.
- `overlays', a hash of state-id → overlay.
- `pending-overlays', a list of overlays that haven't been sent yet.
- `accumulator', a list of message chunks, in reverse order.
- `messages', a list of complete messages that haven't been dispatched yet.
- `synchronous-query-id', the ID of the current synchronous query, if any."
  `((process . ,process)
    (buffer . ,buffer)
    (fresh-id . 0)
    (timer . 0)
    (queries . ,(make-hash-table :test 'eq))
    (overlays . ,(make-hash-table :test 'eq))
    (pending-overlays . ,(list)) ;; TODO could use a queue
    (accumulator . ,(list))
    (messages . ,(list))
    (synchronous-query-id . ,(list))))

(defmacro elcoq--with-other-prover (prover &rest body)
  "`let-alist'-bind PROVER, change to its buffer, and run BODY."
  (declare (debug t)
           (indent 1))
  `(when ,prover
     (let-alist ,prover
       (with-current-buffer .buffer
         ,@body))))

(defmacro elcoq--with-prover (&rest body)
  "`let-alist'-bind PROVER, change to its buffer, and run BODY."
  (declare (debug t)
           (indent 0))
  `(elcoq--with-other-prover elcoq--prover ,@body))

(defvar-local elcoq--prover nil
  "Buffer-local `elcoq--prover' object.
The same prover may be shared by multiple buffer; for this
reason, all prover state should be stored in that object.")

(defun elcoq--sertop-fresh-id ()
  "Return a fresh query ID."
  (intern (format "elcoq-%d" (1- (cl-incf (elcoq-alist-get 'fresh-id elcoq--prover))))))

(defun elcoq-running-p ()
  "Check if an instance of sertop is running in current buffer."
  (and elcoq--prover
       (let-alist elcoq--prover
         (process-live-p .process))))

(defun elcoq--sertop-busy ()
  "Check if sertop is busy."
  (elcoq--with-prover
    (or (> (hash-table-count .queries) 0)
        .synchronous-query-id)))

(defun elcoq--sertop-message-query-id (msg)
  "Read query id of message MSG."
  (pcase msg
    (`(Answer ,tag ,_) tag)))

(defun elcoq--sertop-may-dispatch-id (id)
  "Check if id ID may be dispatched.
Takes `synchronous-query-id' into account."
  (elcoq--with-prover
    (or (null .synchronous-query-id)
        (eq id .synchronous-query-id))))

(defun elcoq--sertop-may-dispatch (msg)
  "Check if message MSG may be dispatched.
Takes `synchronous-query-id' into account."
  (elcoq--sertop-may-dispatch-id (elcoq--sertop-message-query-id msg)))

(defun elcoq--sertop-query-is-control (sexp)
  "Check whether SEXP is a control query.
This determines whether QUERY receives a “Completed” message."
  (eq (car sexp) 'Control))

(defun elcoq--sertop-check-for-completion (answer sexp id)
  "Update state if ANSWER suggests that query SEXP (with ID) has completed.
Non-control queries complete after the first response.  Control
queries complete after a “Completed” signal."
  (-when-let* ((completed (pcase answer
                            (`Ack nil)
                            (`Completed t)
                            (`(CoqExn ,_) t)
                            (_ (not (elcoq--sertop-query-is-control sexp))))))
    (message "Answer %S shows that query %S (%S) is complete." answer sexp id)
    (elcoq-alist-put 'synchronous-query-id nil elcoq--prover)
    (remhash id (elcoq-alist-get 'queries elcoq--prover))))

(defun elcoq--sertop-handle-canceled-state (state-id)
  "Forget about STATE-ID, and remove corresponding overlay."
  (elcoq--with-prover
    (message "Canceling state %S" state-id)
    (-if-let* ((overlay (gethash state-id .overlays)))
        (progn
          (remhash state-id .overlays)
          (delete-overlay overlay))
      (message "Already canceled: %S" state-id))))

(defun elcoq--sertop-may-be-outdated (answer)
  "Check whether ANSWER refering to an outdated state is OK.
This is useful because “Canceled” messages for outdated states
are fine to ignore."
  (eq (car-safe answer) 'StmCanceled))

(defun elcoq--sertop-dispatch-answer (answer id)
  "Handle ANSWER to query with id ID."
  (elcoq--with-prover
    (cl-assert (elcoq--sertop-may-dispatch-id id))
    (let* ((query (gethash id .queries))
           (sexp (elcoq-alist-get 'sexp query)))
      (message "Query %S got answer %S" id answer)
      (unless (or query (elcoq--sertop-may-be-outdated answer))
        (error "Invalid query ID %S in response" id))
      (let ((callback (elcoq-alist-get 'callback query)))
        (unwind-protect
            (pcase answer
              (`Ack
               (funcall callback 'ack))
              (`Completed
               (funcall callback 'done)
               (run-with-timer 0 nil #'elcoq--sertop-process-queue (current-buffer)))
              (`(CoqExn ,errs)
               (funcall callback 'failed errs))
              (`(StmCanceled ,state-ids)
               (mapc #'elcoq--sertop-handle-canceled-state state-ids))
              (_
               (if callback
                   (funcall callback 'output answer)
                 (message "Unexpected answer with no callback: %S" answer))))
          (elcoq--sertop-check-for-completion answer sexp id))))))

(defun elcoq--sertop-set-overlay-status (overlay status)
  "Set tracking overlay OVERLAY's status to STATUS."
  (when overlay
    (message "Setting status of %S to %S" overlay status)
    (overlay-put overlay 'elcoq--status status)
    (overlay-put overlay 'face
                 `(:background ,(pcase status
                                  (`Pending "#555753")
                                  (`Sent "orchid")
                                  (`Identified "mediumorchid")
                                  (`ProcessingIn "lightslateblue")
                                  (`Processed "darkslateblue")
                                  (`Failed "red"))))))

(defun elcoq--sertop-dispatch-feedback (feedback)
  "Dispatch feedback object FEEDBACK, updating overlays as needed."
  (message "Feedback %S" feedback)
  (pcase feedback
    (`((id (State ,state-id))
       (contents ,contents)
       (route ,_)) ;; FIXME what is route?
     (elcoq--with-prover
       (-when-let* ((overlay (gethash state-id .overlays)))
         (overlay-put overlay 'help-echo (pp-to-string contents))
         (pcase contents
           (`(ProcessingIn ,_)
            (elcoq--sertop-set-overlay-status overlay 'ProcessingIn))
           (`Processed
            (elcoq--sertop-set-overlay-status overlay 'Processed))
           (`(ErrorMsg ,_details ,message)
            ;; `details' has `fname', `line_nb', `bol_pos', `line_nb_last',
            ;; `bol_pos_last', `bp', `ep'.
            (elcoq--sertop-set-overlay-status overlay 'Failed)
            (overlay-put overlay 'after-string (format "\n%s\n" message)))
           (other
            (message "Unrecognized feedback contents %S" other))))))
    (_ (message "Unrecognized feedback format %S" feedback))))

(defun elcoq--sertop-dispatch-1 (msg)
  "Dispatch message MSG."
  (elcoq--with-prover
    (pcase msg
      (`(Feedback ,feedback) (elcoq--sertop-dispatch-feedback feedback))
      (`(Answer ,tag ,answer) (elcoq--sertop-dispatch-answer answer tag))
      (response (error "Unsupported response %S" response)))))

(defun elcoq--sertop-dispatch (prover)
  "Dispatch messages in PROVER.
All messages are dispatched, unless
`.synchronous-query-id' is set.  In that case, only
the relevant messages are dispatched, and a timer is set to
re-run this function."
  ;; FIXME decide what to do with Feedback messages
  (elcoq--with-other-prover prover
    (message "Synchronous query is %S\nDispatching %S elements"
             .synchronous-query-id
             (length .messages))
    (when (process-live-p .process)
      (let ((dispatchable-messages nil)
            (leftover-messages nil))
        (dolist (msg .messages)
          (if (elcoq--sertop-may-dispatch msg)
              (push msg dispatchable-messages)
            (push msg leftover-messages)))
        (message "May dispatch %S elements\nMay not dispatch %S elements"
                 (length dispatchable-messages)
                 (length leftover-messages))
        (setq leftover-messages (nreverse leftover-messages))
        (elcoq-alist-put 'messages leftover-messages prover)
        (mapc #'elcoq--sertop-dispatch-1 dispatchable-messages)
        (message "%S messages remain" (length leftover-messages))
        (when (and nil leftover-messages) ;; FIXME
          ;; FIXME reset to 0
          (elcoq-alist-put 'timer (run-with-idle-timer 1 nil #'elcoq--sertop-dispatch prover) prover))))))

(defun elcoq--sertop-filter (proc string)
  "Accumulate PROC's output STRING, and act on full responses."
  ;; FIXME handle dead buffer case
  (message "Filter got %S" (truncate-string-to-width string 80 nil nil "..."))
  (-when-let* ((buf (process-get proc 'elcoq--buffer))
               (prover (buffer-local-value 'elcoq--prover buf)))
    (elcoq--with-other-prover prover
      (let ((parts (split-string string "\0" nil)))
        (while (consp (cdr parts))
          (push (pop parts) .accumulator)
          (let ((msg-string (apply #'concat (nreverse .accumulator))))
            (push (read msg-string) .messages))
          (setq .accumulator nil))
        (push (car parts) .accumulator)
        ;; Save back into prover object
        (elcoq-alist-put 'accumulator .accumulator prover)
        (elcoq-alist-put 'messages .messages prover))
      (elcoq--sertop-dispatch prover))))

(defun elcoq--sertop-start ()
  "Start a new sertop.
Does not kill existing instances.  Use `elcoq-run' for that."
  ;; FIXME Disallow in goal and response.
  (let* ((process-connection-type nil)
         (proc (apply #'start-process "coq" nil
                      (expand-file-name "sertop/sertop.native" elcoq--directory)
                      (elcoq--sertop-args))))
    (process-put proc 'elcoq--buffer (current-buffer))
    (set-process-filter proc #'elcoq--sertop-filter)
    (setq elcoq--prover (elcoq--make-prover proc (current-buffer)))))

(defun elcoq--sertop-ensure ()
  "Ensure that an instance of sertop is running in current buffer."
  (unless (elcoq-running-p)
    (elcoq--sertop-start)))

(defun prin1-to-sexp (val)
  "Convert VAL to a sexp.
Crucially, renders nil as (), not nil."
  (if (listp val)
      (concat "(" (mapconcat #'prin1-to-sexp val " ") ")")
    (prin1-to-string val)))

(defun elcoq--sertop-query (sexp callback)
  "Run query SEXP in current buffer, calling CALLBACK on the result.
Return the query's ID."
  (elcoq--sertop-ensure)
  (let* ((id (elcoq--sertop-fresh-id))
         (full-sexp (list id sexp))
         (query-string (let ((print-escape-newlines t))
                         (prin1-to-sexp full-sexp))))
    (elcoq--with-prover
      (message "Sending %s" query-string)
      (puthash id (elcoq--make-query sexp callback) .queries)
      (process-send-string .process query-string)
      (process-send-string .process "\n")
      id)))

(defun elcoq--sertop-query-synchronously (sexp)
  "Run query SEXP synchronously in current buffer."
  (elcoq--sertop-ensure)
  (elcoq--with-prover
    (cl-assert (null .synchronous-query-id))
    (let* ((response nil)
           (callback (lambda (status &optional output)
                       (when (eq status 'output)
                         (setq response output))))
           (id (elcoq--sertop-query sexp callback)))
      (message "Synchronous query %S starting" sexp)
      (elcoq-alist-put 'synchronous-query-id id elcoq--prover)
      (while (elcoq-alist-get 'synchronous-query-id elcoq--prover)
        ;; Wait until query is done processing
        ;; Passing “1” ensures that timers are not run
        (accept-process-output .process nil nil 1))
      (cl-assert response)
      (message "Synchronous query %S completed" sexp)
      response)))

(defun elcoq-run ()
  "Run Coq using sertop.
If needed, kill the existing sertop process."
  (interactive)
  (elcoq-kill)
  (elcoq--sertop-ensure))

(defun elcoq--sertop-kill-process ()
  "Kill sertop process."
  (elcoq--with-prover
    (set-process-filter .process #'ignore)
    (when (process-live-p .process)
      (delete-process .process)
      (accept-process-output))
    (let ((timer (elcoq-alist-get 'timer elcoq--prover)))
      (when (timerp timer)
        (cancel-timer timer)))
    (setq elcoq--prover nil)))

(defun elcoq--is-elcoq-overlay (ov)
  "Check if OV is an elcoq overlay."
  (overlay-get ov 'elcoq--status))

(defun elcoq--remove-overlays ()
  "Remove all elcoq overlays in current buffer.
Does not interact with the prover; only safe if there is no
sertop running."
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (elcoq--is-elcoq-overlay ov)
        (delete-overlay ov)))))

(defun elcoq-kill ()
  "Kill sertop process and clean up buffer."
  (interactive)
  (elcoq--sertop-kill-process)
  (elcoq--remove-overlays))

(defun elcoq--queries-StmState ()
  "Construct an StmState query."
  `(Control StmState))

(defun elcoq--queries-StmCancel (state-ids)
  "Construct an StmCancel query for states STATE-IDS."
  `(Control (StmCancel ,state-ids)))

(defun elcoq--queries-StmJoin ()
  "Construct an StmJoin query."
  `(Control StmJoin))

(defun elcoq--queries-StmAdd (overlay previous-state)
  "Construct an StmAdd query to add text in OVERLAY to PREVIOUS-STATE."
  (elcoq--with-prover
    (let ((str (buffer-substring-no-properties
                (overlay-start overlay)
                (overlay-end overlay))))
      `(Control (StmAdd
                 1 ;; Just one sentence for now
                 ,(if previous-state `(Some ,previous-state) `None)
                 ,str)))))

(defun elcoq--queries-StmObserve (state-id)
  "Construct an StmObserve query for STATE-ID."
  `(Control (StmObserve ,state-id)))

(defun elcoq--queries-Goals ()
  "Construct an Goals query."
  `(Query (() None PpStr) Goals))

(defun elcoq--sertop-read-state-id (response)
  "Read state id from RESPONSE."
  (pcase response
    ((or `(StmAdded ,state-id ,_ ,_)
         `(StmCurId ,state-id)) ;; FIXME remove StmCurId
     state-id)))

(defun elcoq--sertop-overlay-callback (overlay status &optional output)
  "Update OVERLAY based on STATUS and OUTPUT."
  (when (eq status 'output)
    (let ((id (elcoq--sertop-read-state-id output)))
      (unless id
        (error "Overlay callback: Unexpected answer %S" output))
      (message "Mapping id %S to overlay %S" id overlay)
      (overlay-put overlay 'elcoq--state-id id)
      (elcoq--sertop-set-overlay-status overlay 'Identified)
      (puthash id overlay (elcoq-alist-get 'overlays elcoq--prover)))))

(defun elcoq--prover-update-goals (status &optional _)
  "Synchronously update goals if STATUS is `done'."
  ;; FIXME should this check if the prover is available (ideally in the future
  ;; this query will be asynchronous).
  (when (eq status 'done)
    (let ((response (elcoq--sertop-query-synchronously (elcoq--queries-Goals))))
      (pcase response
        (`(ObjList ,goals)
         (let-alist (elcoq-buffers)
           (with-current-buffer .goals
             (let ((inhibit-read-only))
               (erase-buffer)
               (dolist (goal goals)
                 (pcase goal
                   (`(CoqString ,str)
                    (insert str))))))))))))

(defun elcoq--sertop-observe (state-id)
  "If no overlays are pending and the prover isn't busy, observe STATE-ID."
  (interactive (list (elcoq--previous-state (point)))) ;; FIXME don't be interactive
  (elcoq--with-prover
    (unless (or (elcoq--sertop-busy) .pending-overlays)
      (elcoq--sertop-query (elcoq--queries-StmObserve state-id) #'ignore)))) ;; elcoq--prover-update-goals

(defun elcoq--sertop-join ()
  "If no overlays are pending and the prover isn't busy, join."
  (interactive) ;; FIXME don't be interactive
  (elcoq--with-prover
    (unless (or (elcoq--sertop-busy) .pending-overlays)
      (elcoq--sertop-query (elcoq--queries-StmJoin) #'ignore)))) ;; elcoq--prover-update-goals

(defun elcoq--cancel-state (state-id)
  "Send an “StmCancel” for STATE-ID and remove it from .overlays."
  (elcoq--with-prover
    (let ((sexp (elcoq--queries-StmCancel (list state-id))))
      (elcoq--sertop-query sexp #'ignore)
      (remhash state-id .overlays))))

(defun elcoq--remove-pending-overlay (overlay)
  "Remove OVERLAY from pending overlays list."
  (elcoq--with-prover
    (elcoq-alist-put 'pending-overlays (delete overlay .pending-overlays) elcoq--prover)))

(defun elcoq--edit-in-overlay (overlay)
  "Remove OVERLAY, sending an “StmCancel” message if needed.
This clears all references to OVERLAY held in internal data structures."
  ;; FIXME what happens if overlay was sent but not yet numbered and needs to
  ;; be removed? → Must be locked (this bit could be removed by changing the
  ;; semantics of StmAdd)
  (when (elcoq--is-elcoq-overlay overlay)
    (-if-let* ((state-id (overlay-get overlay 'elcoq--state-id)))
        (elcoq--cancel-state state-id)
      (elcoq--remove-pending-overlay overlay))
    (delete-overlay overlay)))

(defun elcoq--edit-at-point (&optional pos)
  "Remove overlays at POS (default: point), sending appropriate messages."
  (dolist (ov (overlays-at (or pos (point))))
    (elcoq--edit-in-overlay ov)))

(defun elcoq--state-id-at-pos (pos)
  "Return state id at position POS."
  (get-pos-property pos 'elcoq--state-id))

(defun elcoq--previous-state (pos)
  "Find state id of sentence before POS."
  (save-restriction
    (widen) ;; FIXME http://emacs.stackexchange.com/questions/23970/
    (let ((1-past-sentence-end (previous-single-char-property-change (1+ pos) 'elcoq--state-id)))
      (when (> 1-past-sentence-end (point-min))
        (let ((state-id (elcoq--state-id-at-pos (1- 1-past-sentence-end))))
          (cl-assert state-id nil "Expecting to find a state ID at position %S" pos)
          state-id)))))

(defun elcoq--sertop-send-one ()
  "Send contents of first pending overlay to prover."
  (elcoq--with-prover
    (-when-let* ((ov (car .pending-overlays)))
      (elcoq-alist-put 'pending-overlays (cdr .pending-overlays) elcoq--prover)
      (elcoq--sertop-set-overlay-status ov 'Sent)
      (elcoq--sertop-query (elcoq--queries-StmAdd ov (elcoq--previous-state (overlay-start ov)))
                      (apply-partially #'elcoq--sertop-overlay-callback ov)))))

(defun elcoq--sertop-process-queue (&optional buffer)
  "Process first pending overlay in BUFFER."
  (setq buffer (or buffer (current-buffer)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (elcoq--with-prover
        (unless (elcoq--sertop-busy)
          (elcoq--sertop-send-one))))))

(defun elcoq--first-uncovered-point (pos) ;; FIXME implement retraction as well
  "Find first point before POS not covered by an elcoq overlay."
  (->> (overlays-in 1 pos) ;; FIXME (1- pos)?
       (-filter #'elcoq--is-elcoq-overlay)
       (mapcar #'overlay-end)
       (apply #'max 1)))

(defun elcoq--overlay-modified (overlay _before-p _beg _end &optional _len)
  "Register modification of overlay OVERLAY.
BEFORE-P, BEG, END, LEN: see info node `(elisp)Overlay Properties'"
  (elcoq--edit-in-overlay overlay))

(defun elcoq--queue-area (beg end)
  "Create an overlay identifying BEG .. END as a single sentence."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'modification-hooks '(elcoq--overlay-modified))
    (overlay-put ov 'insert-in-front-hooks '(elcoq--overlay-modified))
    (elcoq--sertop-set-overlay-status ov 'Pending)
    ov))

(defun elcoq--queue-sentences (beg end)
  "Queue sentences in BEG .. END."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((overlays nil)
            (ov-beg (point)))
        (while (re-search-forward "\\.\\(\\s-\\|$\\)" nil t)
          (skip-syntax-backward "-")
          (push (elcoq--queue-area ov-beg (point)) overlays)
          (setq ov-beg (point)))
        overlays))))

(defun elcoq-queue-up-to (pos)
  "Queue sentences up to POS (interactively, up to point) ."
  (interactive "d")
  (elcoq--sertop-ensure)
  (let* ((beg (elcoq--first-uncovered-point pos))
         (pending (append (elcoq-alist-get 'pending-overlays elcoq--prover)
                          (nreverse (elcoq--queue-sentences beg pos)))))
    (elcoq-alist-put 'pending-overlays pending elcoq--prover))
  (elcoq--sertop-process-queue))

(defun elcoq-buffers ()
  "Return a list of goal and response buffers, creating them if needed."
  `((goals . ,(get-buffer-create "*Goal*"))
    (response . ,(get-buffer-create "*Response*"))))

(defun elcoq-layout ()
  "Layout windows of current frame."
  (interactive)
  (delete-other-windows)
  (with-selected-window (split-window-horizontally)
    (let-alist (elcoq-buffers)
      (switch-to-buffer .goals)
      (with-selected-window (split-window)
        (switch-to-buffer .response)))))

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
