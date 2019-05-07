#!/bin/sh

echo 'Testing code under Racket\n'
raco test template-tests.rkt
echo '\nDone testing code under Racket\n'


echo 'Testing code under Chez\n'
scheme test.scm
echo '\nDone testing code under Chez\n'
