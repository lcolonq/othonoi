;;; othonoi --- Something Near Corfu -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'cl-lib)
(require 'dash)
(require 's)
(require 'ht)
(require 'f)

(defcustom o/num-candidates 9
  "Number of candidates to display in completion window."
  :type '(number)
  :group 'othonoi)

(defcustom o/background-color "black"
  "Background color for completion window."
  :type '(string)
  :group 'othonoi)

(defface o/highlight
  '((t :inherit highlight :extend t))
  "Face for highlighting completion candidates."
  :group 'othonoi)

(defconst o/mode-map (make-sparse-keymap)
  "Keymap used when `o/mode' is active.")
(define-key o/mode-map (kbd "<tab>") #'o/tab)

(defconst o/popup-map (make-sparse-keymap))
(define-key o/popup-map (kbd "<backtab>") #'o/prev)
(define-key o/popup-map (kbd "<down>") #'o/next)
(define-key o/popup-map (kbd "<up>") #'o/prev)
(define-key o/popup-map (kbd "<return>") #'o/insert)

(define-minor-mode o/mode
  "Minor mode for in-buffer autocompletion."
  :group 'othonoi
  :keymap o/mode-map
  (cond
   (o/mode
    (add-hook 'post-command-hook #'o/update nil 'local)
    t)
   (t
    (remove-hook 'post-command-hook #'o/update 'local)
    t)))

(defun o/show-frame (f vis)
  "If VIS is non-nil, make the frame F visible.
Otherwise make it invisible."
  (when f
    (if vis
        (make-frame-visible f)
      (make-frame-invisible f))))
(defun o/move-frame (f x y)
  "Move the frame F to X, Y."
  (when f
    (modify-frame-parameters
     f
     (list
      (cons 'top y)
      (cons 'left x)))))
(defun o/resize-frame (f w h)
  "Resize the frame F to W, H."
  (when f
    (modify-frame-parameters
     f
     (list
      (cons 'width w)
      (cons 'height h)))))

(define-derived-mode o/candidates-mode special-mode "Othonoi Completions"
  "Major mode for displaying completion candidates."
  :group 'othonoi
  (setq mode-line-format nil)
  (setq truncate-lines t))
(defvar o/candidates-frame nil)
(defun o/get-candidates-frame-buffer ()
  "Return the buffer used to display completion candidates."
  (let ((name " *othonoi-completions*"))
    (unless (get-buffer name)
      (with-current-buffer (get-buffer-create name)
        (o/candidates-mode)))
    (get-buffer name)))
(defun o/create-candidates-frame ()
  "Build a frame for displaying completion candidates."
  (when (framep o/candidates-frame)
    (delete-frame o/candidates-frame))
  (setf
   o/candidates-frame
   (make-frame
    (append
     `((name . "othonoi")
       (unsplittable . t)
       (undecorated . t)
       (no-accept-focus . t)
       (no-focus-on-map . t)
       (override-redirect . t)
       (user-size . t)
       (width . 30)
       (height . 15)
       (user-position . t)
       (left . -1)
       (top . -1)
       (default-minibuffer-frame . ,(selected-frame))
       (minibuffer . nil)
       (left-fringe . 0)
       (right-fringe . 0)
       (cursor-type . nil)
       (background-color . ,o/background-color)))))
  (make-frame-invisible o/candidates-frame)
  (let ((window (frame-selected-window o/candidates-frame)))
    (set-window-buffer window (o/get-candidates-frame-buffer))))

(define-derived-mode o/context-mode special-mode "Othonoi Context"
  "Major mode for displaying completion candidate context."
  :group 'othonoi
  (setq mode-line-format nil))
(defvar o/context-frame nil)
(defun o/get-context-frame-buffer ()
  "Return the buffer used to display completion candidate context."
  (let ((name " *othonoi-context*"))
    (unless (get-buffer name)
      (with-current-buffer (get-buffer-create name)
        (o/context-mode)))
    (get-buffer name)))
(defun o/create-context-frame ()
  "Build a frame for displaying completion candidate context."
  (when (framep o/context-frame)
    (delete-frame o/context-frame))
  (setf
   o/context-frame
   (make-frame
    (append
     `((name . "othonoi-context")
       (unsplittable . t)
       (undecorated . t)
       (no-accept-focus . t)
       (no-focus-on-map . t)
       (override-redirect . t)
       (user-size . t)
       (width . 60)
       (height . 30)
       (user-position . t)
       (left . -1)
       (top . -1)
       (default-minibuffer-frame . ,(selected-frame))
       (minibuffer . nil)
       (left-fringe . 0)
       (right-fringe . 0)
       (cursor-type . nil)
       (background-color . ,o/background-color)))))
  (make-frame-invisible o/context-frame)
  (let ((window (frame-selected-window o/context-frame)))
    (set-window-buffer window (o/get-context-frame-buffer))))

(defvar-local o/backends nil
  "List of backends to use for completion.")

(defvar-local o/prefix-at-point-function #'o/symbol-prefix-at-point
  "Function returning the start position and prefix at point.")

(defvar-local o/completion nil
  "Current completion state.")

(cl-defstruct (o/backend (:constructor o/make-backend))
  name
  function)

(cl-defstruct (o/candidate (:constructor o/make-candidate))
  string
  backend-name
  context)
(defun o/candidate->string (cand)
  "Return the string for CAND, which could be one of many types."
  (cond
   ((o/candidate-p cand) (o/candidate-string cand))
   ((stringp cand) cand)
   (t (error "Invalid candidate: %s" cand))))

(cl-defstruct (o/state (:constructor o/make-state))
  candidates ;; completion candidates being chosen from. note this should not change!
  pos ;; position in source buffer of completion (end of replacement)
  start-pos ;; position in source buffer of start of replacement
  (index 0) ;; index of currently selected candidate
  )

(defun o/write (text &optional face)
  "Write TEXT to the current buffer and apply FACE."
  (let ((text-final (if face (propertize text 'face face) text)))
    (insert text-final)))

(defun o/write-line (line &optional face)
  "Write LINE and a newline to the current buffer and apply FACE."
  (o/write (concat line "\n") face))

(defun o/filter-prefix (prefix candidates)
  "Return CANDIDATES matching PREFIX."
  (let ((-compare-fn (lambda (x y) (s-equals? (o/candidate->string x) (o/candidate->string y)))))
    (-uniq
     (--filter
      (when-let ((s (o/candidate->string it)))
        (s-prefix? prefix s))
      (-non-nil candidates)))))

(cl-defun o/first-result (pred xs)
  "Return the first non-nil PRED on XS alongside the element that yielded it."
  (--each xs
    (when-let ((y (funcall pred it)))
      (cl-return-from o/first-result (cons it y)))))
(defun o/complete-with (prefix backends)
  "Return a list of candidates matching PREFIX given the list of BACKENDS."
  (when-let*
      ((res
        (o/first-result
         (lambda (it) (funcall (o/backend-function it) prefix))
         (-map #'funcall backends)))
       (backend (car res))
       (comps (cdr res)))
    (--each comps
      (when (o/candidate-p it)
        (setf (o/candidate-backend-name it) (o/backend-name backend))))
    comps))

(defun o/symbol-prefix-at-point ()
  "Return a pair of the start position and prefix string preceding point."
  (let ((start (save-excursion (skip-syntax-backward "w_") (point))))
    (cons
     start
     (buffer-substring-no-properties start (point)))))

(defun o/line-prefix-at-point ()
  "Return a pair of the start position and prefix string preceding point."
  (let ((start (line-beginning-position)))
    (cons
     start
     (buffer-substring-no-properties start (point)))))

(defun o/fish-prefix-at-point ()
  "Return a pair of the start position and prefix string preceding point."
  (let ((start (save-excursion (skip-chars-backward "^[ \t\n]") (point))))
    (cons
     start
     (buffer-substring-no-properties (line-beginning-position) (point)))))

(defun o/render-context (cand)
  "Render the context for CAND to the context buffer."
  (with-current-buffer (o/get-context-frame-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (o/write-line
       (o/candidate-context cand)))))

(defun o/render-candidates (cs index)
  "Render the candidates CS to the completions buffer.
INDEX is the index of the selected candidate."
  (with-current-buffer (o/get-candidates-frame-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let* ((len (length cs))
             (scroll
              (min
               (max 0 (- len o/num-candidates))
               (max 0 (- index (/ o/num-candidates 2)))))
             (scrolled (-take o/num-candidates (-drop scroll cs)))
             (sidx (- index scroll)))
        (--each-indexed scrolled
          (o/write-line
           (o/candidate->string it)
           (when (= it-index sidx)
             'o/highlight)))))))

(defun o/render ()
  "Update the frame to display completion state from the current buffer."
  (unless (framep o/candidates-frame)
    (o/create-candidates-frame))
  (unless (framep o/context-frame)
    (o/create-context-frame))
  (cond
   (o/completion
    (let* ((pos (window-absolute-pixel-position (o/state-start-pos o/completion)))
           (cands (o/state-candidates o/completion))
           (idx (o/state-index o/completion))
           (selected (nth idx cands))
           (o/num-candidates (min o/num-candidates (length cands)))
           (width (max 20 (+ 1 (-max (--map (length (o/candidate->string it)) cands)))))
           (bx (car pos))
           (by (+ (cdr pos) (if header-line-format 0 (line-pixel-height))))
           )
      (set-transient-map o/popup-map)
      (o/render-candidates cands idx)
      (o/resize-frame o/candidates-frame width o/num-candidates)
      (o/move-frame o/candidates-frame bx by)
      (o/show-frame o/candidates-frame t)
      (if (and selected (o/candidate-p selected) (o/candidate-context selected))
          (progn
            (o/render-context selected)
            (o/move-frame o/context-frame (+ bx (* width (default-font-width))) by)
            (o/show-frame o/context-frame t))
        (o/show-frame o/context-frame nil))))
   (t
    (o/show-frame o/context-frame nil)
    (o/show-frame o/candidates-frame nil))))

(defun o/update ()
  "Update and redisplay the completion state.
Intended to run in `post-command-hook'."
  (when o/completion
    (when (not (= (point) (o/state-pos o/completion)))
      (setq-local o/completion nil)
      (with-current-buffer (o/get-candidates-frame-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)))))
  (o/render))

(defun o/common-prefix (cs)
  "Return the common prefix string of CS."
  (let*
      ((strs (-map #'o/candidate->string cs))
       (s (car strs))
       (ss (cdr strs))
       (end 0)
       (res nil))
    (while (and (not res) (<= end (length s)))
      (let ((pfx (substring s 0 end)))
        (if (--all? (and (< end (length it)) (s-equals? pfx (substring it 0 end))) ss)
            (cl-incf end)
          (setf res (substring s 0 (- end 1))))))
    (and (s-present? res) res)))

(defun o/expand ()
  "Expand common prefix of completions from point in the current buffer."
  (interactive)
  (when o/completion
    (when-let*
        ((prefix (funcall o/prefix-at-point-function))
         (cs (o/state-candidates o/completion)))
      (when-let ((common (o/common-prefix cs))
                 ((s-prefix? (cdr prefix) common)))
        (delete-region (o/state-start-pos o/completion) (o/state-pos o/completion))
        (goto-char (o/state-start-pos o/completion))
        (insert common)
        (o/complete)))))

(defun o/complete ()
  "Complete from point in the current buffer."
  (interactive)
  (when-let*
      ((prefix (funcall o/prefix-at-point-function))
       (cs (o/complete-with (cdr prefix) o/backends)))
    (setq-local
     o/completion
     (o/make-state
      :candidates cs
      :start-pos (car prefix)
      :pos (point)))
    (if (= 1 (length cs))
        (o/insert)
      (o/render))))

(defun o/next ()
  "Go to the next candidate."
  (interactive)
  (when o/completion
    (setf
     (o/state-index o/completion)
     (mod (+ 1 (o/state-index o/completion)) (length (o/state-candidates o/completion))))))

(defun o/prev ()
  "Go to the previous candidate."
  (interactive)
  (when o/completion
    (setf
     (o/state-index o/completion)
     (mod (+ -1 (o/state-index o/completion)) (seq-length (o/state-candidates o/completion))))))

(defun o/tab ()
  "Start completion if no completion is active, or move to the next candidate."
  (interactive)
  (cond
   (o/completion (o/next))
   (t
    (o/complete)
    (o/expand)
    (o/render))))

(defun o/insert ()
  "Replace the prefix with the selected completion."
  (interactive)
  (when o/completion
    (when-let ((cand (seq-elt (o/state-candidates o/completion) (o/state-index o/completion))))
      (delete-region (o/state-start-pos o/completion) (o/state-pos o/completion))
      (goto-char (o/state-start-pos o/completion))
      (insert (o/candidate->string cand))
      (setq o/completion nil))))

(defun o/helper-external-command (command &rest args)
  "Return the output of COMMAND ARGS as a string."
  (with-output-to-string
    (with-current-buffer standard-output
      (apply #'call-process command nil '(t nil) nil args))))

(defun o/backend-lsp ()
  "Build a new completion backend for `lsp-mode'."
  (o/make-backend
   :name "LSP"
   :function
   (lambda (prefix)
     (when-let*
         ((res (lsp-completion-at-point))
          (col (caddr res))
          (cands
           (cond
            ((functionp col) (funcall col prefix nil t))
            (t nil))))
       (o/filter-prefix
        prefix
        (--map
         (let*
             ((props (text-properties-at 0 it))
              (ci (plist-get props 'lsp-completion-item)))
           (o/make-candidate
            :string
            (-some-> ci
              (ht-get "filterText")
              (or it)
              (substring-no-properties))
            :context
            (-some-> ci
              (ht-get "documentation")
              (ht-get "value")
              (substring-no-properties))))
         cands))))))

(defun o/backend-eglot ()
  "Build a new completion backend for `eglot'."
  (o/make-backend
   :name "eglot"
   :function
   (lambda (prefix)
     (when-let*
         ((res (eglot-completion-at-point))
          (col (caddr res))
          (cands
           (cond
            ((functionp col) (funcall col prefix nil t))
            (t nil))))
       (o/filter-prefix
        prefix
        (--map
         (let*
             ((props (text-properties-at 0 it))
              (ci (plist-get props 'eglot--lsp-item))
              (ft
               (-some-> ci
                 (plist-get :filterText)
                 (or it)
                 (substring-no-properties)))
              (s (substring-no-properties it)))
           (o/make-candidate
            :string
            (or ft s)
            :context
            (-some-> ci
              (plist-get :documentation)
              (plist-get :value)
              (substring-no-properties)
              (eglot--format-markup))))
         cands))))))

(defun o/backend-fish ()
  "Build a new completion backend for the Fish shell."
  (o/make-backend
   :name "Fish"
   :function
   (lambda (prefix)
     (when-let*
         ((res
           (o/helper-external-command
            "fish" "-c"
            (format "complete -C%s" (shell-quote-argument prefix))))
          (lines (s-lines res))
          (cands (--map (s-split "\t" it) lines)))
       (--filter
        (s-present? (o/candidate->string it))
        (--map
         (o/make-candidate
          :string (car it)
          :context (cadr it))
         cands))))))

(defun o/backend-elisp ()
  "Build a new completion backend for Emacs Lisp."
  (o/make-backend
   :name "Emacs Lisp"
   :function
   (lambda (prefix)
     (when-let*
         ((res (elisp-completion-at-point))
          (col (caddr res))
          (props (cdddr res))
          (cands (all-completions prefix col (plist-get props :predicate))))
       cands))))

(defun o/backend-test ()
  "Test completion backend."
  (o/make-backend
   :name "Test"
   :function
   (lambda (prefix)
     (o/filter-prefix
      prefix
      (list
       (o/make-candidate :string "foo")
       (o/make-candidate :string "bar")
       (o/make-candidate :string "baz"))))))

(provide 'othonoi)
;;; othonoi.el ends here
