#-swank
(quit
 :unix-status
 (process-exit-code
  (run-program *runtime-pathname*
               (list "--noinform"
                     "--eval" (format nil "(compile-file \"~A\")" *load-pathname*)
                     "--quit")
               :output t :error t :input t)))
