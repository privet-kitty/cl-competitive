STATUS=0

for test_file in *.test.lisp; do
    sbcl --script ${test_file}
    if [ "$?" -ne 0 ]; then
	STATUS=1
    fi
done

exit $STATUS
