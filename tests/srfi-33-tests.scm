
(import (chibi) (srfi 33) (chibi test))

(test-begin "srfi-33")

(test 1 (arithmetic-shift 1 0))
(test 2 (arithmetic-shift 1 1))
(test 4 (arithmetic-shift 1 2))
(test 8 (arithmetic-shift 1 3))
(test 16 (arithmetic-shift 1 4))
(test (expt 2 31) (arithmetic-shift 1 31))
(test (expt 2 32) (arithmetic-shift 1 32))
(test (expt 2 33) (arithmetic-shift 1 33))
(test (expt 2 63) (arithmetic-shift 1 63))
(test (expt 2 64) (arithmetic-shift 1 64))
(test (expt 2 65) (arithmetic-shift 1 65))
(test (expt 2 127) (arithmetic-shift 1 127))
(test (expt 2 128) (arithmetic-shift 1 128))
(test (expt 2 129) (arithmetic-shift 1 129))
(test 3028397001194014464 (arithmetic-shift 11829675785914119 8))

(test -1 (arithmetic-shift -1 0))
(test -2 (arithmetic-shift -1 1))
(test -4 (arithmetic-shift -1 2))
(test -8 (arithmetic-shift -1 3))
(test -16 (arithmetic-shift -1 4))
(test (- (expt 2 31)) (arithmetic-shift -1 31))
(test (- (expt 2 32)) (arithmetic-shift -1 32))
(test (- (expt 2 33)) (arithmetic-shift -1 33))
(test (- (expt 2 63)) (arithmetic-shift -1 63))
(test (- (expt 2 64)) (arithmetic-shift -1 64))
(test (- (expt 2 65)) (arithmetic-shift -1 65))
(test (- (expt 2 127)) (arithmetic-shift -1 127))
(test (- (expt 2 128)) (arithmetic-shift -1 128))
(test (- (expt 2 129)) (arithmetic-shift -1 129))

(test 0 (arithmetic-shift 1 -63))
(test 0 (arithmetic-shift 1 -64))
(test 0 (arithmetic-shift 1 -65))

(test #x1000000000000000100000000000000000000000000000000
    (arithmetic-shift #x100000000000000010000000000000000 64))

(test-not (bit-set? 64 1))
(test-assert (bit-set? 64 #x10000000000000000))

(test-end)
