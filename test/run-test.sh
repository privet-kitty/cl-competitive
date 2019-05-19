for test_file in *.test.lisp; do
    sbcl --script ${test_file}
done
