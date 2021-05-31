;;; ob-csharp.el --- Modified version of ob-csharp.el to work with dotnet core project

(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("csharp" . "cs"))

(defcustom org-babel-csharp-command "dotnet run"
  "Name of the csharp command."
  :group 'org-babel
  :version "24.3"
  :type 'string)


(defun org-babel-execute:csharp (body params)
  (let* ((full-body (org-babel-expand-body:generic body params))
         (cmpflag (or (cdr (assoc :cmpflag params)) ""))
         (cmdline (or (cdr (assoc :cmdline params)) ""))
         (src-file "Program.cs")
         (results 
          (progn (with-temp-file src-file (insert full-body))
                 (org-babel-eval 
                  (concat org-babel-csharp-command " " cmpflag " "  src-file) ""))))
      (org-babel-reassemble-table
       (org-babel-result-cond (cdr (assoc :result-params params))
         (org-babel-read results)
         (let ((tmp-file (org-babel-temp-file "c-")))
           (with-temp-file tmp-file (insert results))
           (org-babel-import-elisp-from-file tmp-file)))
       (org-babel-pick-name
        (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
       (org-babel-pick-name
        (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))

(defun org-babel-prep-session:csharp (session params)
  "Return an error because csharp does not support sessions."
  (error "Sessions are not (yet) supported for CSharp"))


(provide 'ob-csharp)
;;; ob-csharp.el ends here
