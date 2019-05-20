STATUS=0

cd `dirname $0`

# sbcl --noinform --no-userinit --no-sysinit --noprint --eval "(compile-file \"test-util.lisp\")" --quit

for test_file in *.test.lisp; do
    sbcl --script ${test_file}
    if [ "$?" -ne 0 ]; then
	STATUS=1
    fi
done

exit $STATUS
