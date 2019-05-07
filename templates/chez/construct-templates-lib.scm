(library (construct-templates-lib)
  (export construct-pattern)
  (import (rnrs)
          (pmatch-lib)
          (only (chezscheme) include format add1))

  (include "../template-code/construct-templates.scm")
  
  )
