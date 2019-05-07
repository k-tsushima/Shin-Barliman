#!/bin/sh

echo 'Testing code under Racket\n'
cd racket
raco test template-tests.rkt
cd ..
echo '\nDone testing code under Racket\n'


echo 'Testing code under Chez\n'
cd chez
scheme test.scm
cd ..
echo '\nDone testing code under Chez\n'
