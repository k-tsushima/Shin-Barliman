(library (mk-lib)
  (export run
          run*
          fresh
          conde
          ==
          =/=
          symbolo
          numbero
          absento          
          lambdag@
          var?
          walk
          state-S
          state-deferred-defer
          conde1
          conde1$
          conde$
          conde$-dfs
          conde-weighted
          state-depth
          state-depth-set
          allow-incomplete-search?
          enable-conde1?
          let/vars
          unit
          project0)
  (import (rnrs)
          (only (chezscheme)
                include
                gensym?
                ormap
                sub1
                fxsra
                fxsll
                fx=))

  (include "mk/mk-vicare.scm")
  (include "mk/mk.scm")
  
  )
