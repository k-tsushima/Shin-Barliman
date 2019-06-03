(library (template-tests-lib)
  (export test-all)
  (import (rnrs)
          (mk-lib)
          (interp-lib)
          (construct-templates-lib)
          (check-equal-lib)
          (only (chezscheme)
                include
                load
                gensym))
  
  (define test-all
    (lambda ()
      
      (include "../template-code/template-tests.scm")
      (include "../template-code/evalo-template-tests.scm")))  
  
  )
