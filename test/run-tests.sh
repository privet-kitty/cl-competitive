STATUS=0

for test_file in *.test.lisp; do
    sbcl --script ${test_file} --eval "(quit :unix-status (if test-util:*failures* 1 0))"
    if [ "$?" -ne 0 ]; then
	STATUS=1
    fi
done

exit $STATUS
