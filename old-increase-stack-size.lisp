#-(or child-sbcl swank)
(quit
 :unix-status
 (process-exit-code
  (run-program *runtime-pathname*
               `("--control-stack-size" "32MB"
                 "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
                 "--eval" "(push :child-sbcl *features*)"
                 "--script" ,(namestring *load-pathname*))
               :output t :error t :input t)))
