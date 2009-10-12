(library (hello world)
  (export start)
  (import (only (lunula) define-scenario page start))

  (define-scenario (index io request)
    (page (io) index))

)
