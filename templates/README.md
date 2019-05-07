# This template code runs under both Chez Scheme and Racket.

Below are simple transcipts showing how to load the libraries/modules in Chez Scheme and Racket.


## Chez Scheme

```
Chez Scheme Version 9.5.3
Copyright 1984-2019 Cisco Systems, Inc.

> (import (construct-templates-lib))
> (time (construct-pattern 'reverse (list (cons (list (list 1 2)) (list 2 1)))))
(time (construct-pattern (quote reverse) ...))
    no collections
    0.000005864s elapsed cpu time
    0.000006000s elapsed real time
    1024 bytes allocated
```

To run all the tests:

```
> (import (template-tests-lib))
> (test-all)
```

or, run the command:

```
scheme test.scm
```

from the terminal in this directory.


## Racket

```
Welcome to Racket v7.2.
> (require "construct-templates.rkt")
> (time (construct-pattern 'reverse (list (cons (list (list 1 2)) (list 2 1)))))
cpu time: 0 real time: 0 gc time: 0
(define reverse (lambda (l) (if (null? l) ,B (,C (car l) (reverse (cdr l))))))
```

To run all the tests:

```
> (require "template-tests.rkt")
```

or, run the Racket command:

```
raco test template-tests.rkt
```

from the terminal in this directory.



## To run all tests under both Chez and Racket

```
/bin/sh test.sh
```
