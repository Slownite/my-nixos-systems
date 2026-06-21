;;; core-gptel.el --- LLM chat via gptel -*- lexical-binding: t; -*-

(use-package gptel
  :commands (gptel gptel-send gptel-menu my/gptel-set-model)
  :config
  ;; HuggingFace Inference Providers expose an OpenAI-compatible router.
  ;; The API key (a HF token) is read from auth-source, e.g. in
  ;; ~/.authinfo.gpg:
  ;;   machine router.huggingface.co login apikey password hf_...
  (setq gptel-model 'deepseek-ai/DeepSeek-V3-0324
        gptel-backend (gptel-make-openai "HuggingFace"
                        :host "router.huggingface.co"
                        :endpoint "/v1/chat/completions"
                        :stream t
                        :key #'gptel-api-key-from-auth-source
                        ;; Seed list; the full catalogue is browsable with
                        ;; `my/gptel-set-model' below.
                        :models '(deepseek-ai/DeepSeek-V3-0324
                                  meta-llama/Llama-3.3-70B-Instruct
                                  Qwen/Qwen2.5-72B-Instruct
                                  Qwen/Qwen3-235B-A22B))))

;; ---- Pick any model the HuggingFace router currently serves ----

(defun my/gptel--hf-token ()
  "Return the HuggingFace router token from auth-source, if present."
  (let ((found (car (auth-source-search :host "router.huggingface.co"
                                        :require '(:secret) :max 1))))
    (when found
      (let ((secret (plist-get found :secret)))
        (if (functionp secret) (funcall secret) secret)))))

(defun my/gptel-hf-models ()
  "Fetch the list of model ids the HuggingFace router currently serves."
  (let* ((token (my/gptel--hf-token))
         (url-request-method "GET")
         (url-request-extra-headers
          (when token `(("Authorization" . ,(concat "Bearer " token))))))
    (with-current-buffer
        (url-retrieve-synchronously "https://router.huggingface.co/v1/models" t t 15)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let ((data (alist-get 'data (json-parse-buffer :object-type 'alist
                                                      :array-type 'list))))
        (sort (mapcar (lambda (m) (alist-get 'id m)) data) #'string<)))))

(defun my/gptel-set-model ()
  "Choose any model the HuggingFace router serves and set it for gptel."
  (interactive)
  (let ((choice (intern (completing-read "HF model: " (my/gptel-hf-models) nil t))))
    (setq gptel-model choice)
    ;; Register it on the backend so the gptel menu also offers it.
    (cl-callf (lambda (models) (cl-remove-duplicates (cons choice models)))
        (gptel-backend-models gptel-backend))
    (message "gptel model: %s" choice)))

(provide 'core-gptel)
;;; core-gptel.el ends here
