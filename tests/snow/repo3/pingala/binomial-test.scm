(import (scheme base) (pingala binomial) (pingala test-map))

(test-map (1 4 6 4 1)
          (lambda (k) (binomial 4 k))
          (0 1 2 3 4))
(test-map (1 5 10 10 5 1)
          (lambda (k) (binomial 5 k))
          (0 1 2 3 4 5))

(test-exit)
