(library (construct-templates-lib)
  (export construct-pattern)
  (import (rnrs)
          (pmatch-lib)
          (only (chezscheme) include format add1))

  (include "construct-templates.scm")
  
  )
