#!/bin/sh

## run all tests in test/ directory

STATUS=0

if [ -z "$LISP_COMMAND" ]; then
    LISP_COMMAND="sbcl --script"
fi

cd $(dirname $0)

# sbcl --noinform --no-userinit --no-sysinit --noprint --eval "(compile-file \"test-util.lisp\")" --quit

for test_file in *.test.lisp; do
    ${LISP_COMMAND} ${test_file}
    if [ "$?" -ne 0 ]; then
	STATUS=1
    fi
done

if [ "$STATUS" -ne 0 ]; then
    echo "Some test cases failed."
fi

exit $STATUS
