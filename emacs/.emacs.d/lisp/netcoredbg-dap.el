;;; netcoredbg-dap.el -*- lexical-binding: t; -*-

(defun dap-netcore--debugger-cmd ()
  "The location of the netcoredbg executable."
  (let ((file-ext (pcase system-type
                    (`windows-nt ".exe")
                    (_ ""))))
    (executable-find "netcoredbg")))

(defun dap-netcore--debugger-locate-or-install ()
  "Return the location of netcoredbg."
  (let ((dbg (dap-netcore--debugger-cmd)))
    (unless (file-exists-p dbg)
      (error "Cannot start debugger configuration without netcoredbg"))
    dbg))

(defun dap-netcore--populate-args (conf)
  "Populate CONF with arguments to launch or attach netcoredbg."
  (dap--put-if-absent conf :dap-server-path (list (dap-netcore--debugger-locate-or-install) "--interpreter=vscode"))
  (pcase (plist-get conf :mode)
    ("launch"
     (dap--put-if-absent
      conf
      :program
      (let ((project-dir (locate-dominating-file
                          (file-name-directory buffer-file-name)
                          (lambda (parent) (directory-files parent nil ".*\.csproj")))))
        (save-mark-and-excursion
         (find-file (concat (f-slash project-dir) "*.*proj") t)
         (let ((csproj (buffer-file-name))
               (res (if (libxml-available-p)
                     (libxml-parse-xml-region (point-min) (point-max))
                     (xml-parse-region (point-min) (point-max)))))
          (kill-buffer)
          (f-join project-dir "bin" "Debug"
           (dom-text (dom-by-tag res 'TargetFramework))
           (dom-text (dom-by-tag res 'RuntimeIdentifier))
           (concat (f-base csproj) ".dll")))))))
    ("attach"
     (dap--put-if-absent conf :processId (string-to-number (read-string "Enter PID: " "2345"))))))

(dap-register-debug-provider
 "coreclr"
 'dap-netcore--populate-args)

(dap-register-debug-template ".Net Core Launch (Console)"
                             (list :type "coreclr"
                                   :request "launch"
                                   :mode "launch"
                                   :name "NetCoreDbg::Launch"
                                   :dap-compilation "dotnet build"))


(provide 'netcoredbg-dap)
