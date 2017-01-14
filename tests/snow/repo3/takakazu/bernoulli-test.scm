(import (scheme base) (takakazu bernoulli) (pingala test-map))

(test-map (1 -1/2 1/6 0 -1/30 0 1/42 0 -1/30 0 5/66)
          (lambda (m) (bernoulli m))
          (0 1 2 3 4 5 6 7 8 9 10))

(test-exit)
