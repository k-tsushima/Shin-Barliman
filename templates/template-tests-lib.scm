(library (template-tests-lib)
  (export test-all)
  (import (rnrs)
          (construct-templates-lib)
          (check-equal-lib)
          (only (chezscheme) include))

  (define test-all
    (lambda ()
      (include "template-tests.scm")))  
  
  )
