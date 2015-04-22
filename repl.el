(require 'comint)

(defvar lambda-cli-file-path "/Users/larsen/Dropbox/Projects/hs/Lambda"
  "Path to the program used by `run-lambda'")

(defvar lambda-cli-arguments '()
  "Commandline arguments to pass to `lambda-cli'")

(defvar lambda-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-lambda'")

(defvar lambda-prompt-regexp "^λ> "
  "Prompt for `run-lambda'.")

(defun run-lambda ()
  "Run an inferior instance of `lambda' inside Emacs."
  (interactive)
  (let* ((lambda-program lambda-cli-file-path)
         (buffer (comint-check-proc "Lambda")))
    ;; pop to the "*Lambda*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'lambda-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*Lambda*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "Lambda" buffer
             lambda-program lambda-cli-arguments)
      (lambda-mode))))

(defun lambda--initialize ()
  "Helper function to initialize Lambda"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(defconst lambda-keywords
  '("lambda" "define"))

(defvar lambda-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt lambda-keywords) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `lambda-mode'.")

(define-derived-mode lambda-mode comint-mode "Lambda"
  "Major mode for `run-lambda'.

\\<lambda-mode-map>"
  nil "Lambda"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp lambda-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(lambda-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) lambda-prompt-regexp))

;; this has to be done in a hook. grumble grumble.
(add-hook 'lambda-mode-hook 'lambda--initialize)
