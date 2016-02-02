
(import (chibi) (chibi ast) (chibi match)
        (chibi process) (chibi filesystem) (chibi test))

(define generated-shared-objects '())

(define (trash-shared-object! file)
  (set! generated-shared-objects (cons file generated-shared-objects)))

(define (cleanup-shared-objects!)
  (for-each (lambda (file) (protect (exn (else #f)) (delete-file file)))
            generated-shared-objects))

(test-begin "ffi")

(define-syntax test-ffi
  (syntax-rules ()
    ((test-ffi name-expr decls tests ...)
     (let* ((name name-expr)
            (stub-file (string-append "tests/ffi/" name ".stub"))
            (c-file (string-append "tests/ffi/" name ".c"))
            (lib-file
             (string-append "tests/ffi/" name *shared-object-extension*)))
       (call-with-output-file stub-file
         (lambda (out) (write 'decls out) (newline out)))
       (let ((res (system
                   "./chibi-scheme" "tools/chibi-ffi" "-c"
                   "-f" (string-append
                         "-O0 -L. -Iinclude"
                         (cond-expand
                          (boehm-gc
                           " -DSEXP_USE_BOEHM=1 -I/opt/local/include")
                          (else
                           "")))
                   stub-file)))
         (cond
          ((zero? (cadr res))
           (let ((orig-failures (test-failure-count)))
             (load lib-file)
             tests ...
             ;; on any failure leave the stub and c file for reference
             (cond
              ((= orig-failures (test-failure-count))
               (delete-file stub-file)
               (delete-file c-file)))
             (trash-shared-object! lib-file)))
          (else
           (test-assert (string-append "couldn't compile " name)
             #f))))))))

;; A couple of dummy definitions to ensure the basic FFI and test
;; framework are working.
(test-ffi
 "basic"
 (begin
   (c-declare "
int zero() {
  return 0;
}
int cube(int x) {
  return x * x * x;
}
int sub(int x, int y) {
  return x - y;
}
")
   (define-c int zero ())
   (define-c int cube (int))
   (define-c int sub (int int)))
 (test 0 (zero))
 (test 4 (sub 7 3))
 (test -27 (cube -3))
 (test -3 (sub (zero) 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; More detailed tests on integer conversions and overflow.

(cond
 ((fixnum? (expt 2 60))
  (test-ffi
   "integers"
   (begin
     (c-declare "
unsigned hoge(unsigned x) {
  return x;
}
long poge(long x) {
  return x;
}
unsigned long piyo(unsigned long x) {
  return x;
}
int ponyo(int x) {
  return !x;
}
enum suuji {
  ichi, ni, san, yon, go, roku, shichi, hachi, kyuu, juu
};
enum suuji tasu(enum suuji a, enum suuji b) {
  return a + b;
}
")
     (define-c unsigned hoge (unsigned))
     (define-c long poge (long))
     (define-c unsigned-long piyo (unsigned-long))
     (define-c boolean ponyo (boolean))
     (define-c-int-type suuji)
     (define-c suuji tasu (suuji suuji)))
   ;; static cast
   (test 4294967295 (hoge -1))
   ;; pass and return a signed bignum
   (test -9223372036854775808 (poge (- (expt 2 63))))
   ;; pass and return an unsigned bignum
   (test 4611686018427387904 (piyo (expt 2 62)))
   ;; booleans
   (test #f (ponyo 'blah))
   (test #t (ponyo #f))
   ;; int types
   (test 5 (tasu 2 3))
   )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String passing, returning and mutation.

(test-ffi
 "strings"
 (begin
   (c-declare "
int char_count(char* str, char ch) {
  int count = 0;
  if (!str) return -1;
  while (*str) if (*str++ == ch) count++;
  return count;
}

char message[] = \"Hello, World!\";
char* get_message() {
  return message;
}
char* get_message2(int k) {
  return k == 1 ? message : NULL;
}

void reverse_string(char* str) {
  int lo=0, hi=strlen(str)-1, ch;
  for (; lo < hi; lo++, hi--) {
    ch = str[lo];
    str[lo] = str[hi];
    str[hi] = ch;
  }
}

char test_cwd[] = \"/home/home/on/the/range\";
char* my_getcwd(char* buf, int len) {
  char* res = test_cwd;
  int needed = strlen(res);
  if (needed >= len) return NULL;
  strncpy(buf, res, needed+1);
  return res;
}
")

   (define-c int char_count (string char))
   (define-c int (char_count2 char_count) (char* char))
   (define-c int (char_count3 char_count) (char* int))
   (define-c int (char_count4 char_count) ((maybe-null char*) int))
   ;; failures
   ;;(define-c int (char_count5 char_count) ((pointer char) int))
   ;;(define-c int (char_count6 char_count) ((array char) int))
   (define-c string get_message ())
   ;; fix maybe-null results
   ;;(define-c (maybe-null string) get_message2 (int))
   ;; fix error results
   ;;(define-c non-null-string (get_message3 get_message2) (int))
   (define-c (free string) strdup (string))
   (define-c void reverse_string (string))
   (define-c non-null-string my_getcwd
     ((result (array char (auto-expand arg1))) (value 4 int)))
   (define-c non-null-string strncpy
     (string string (value (string-length arg1) int)))
   )
 (test 1 (char_count "1-2" #\-))
 (test 2 (char_count2 "1-2-3" #\-))
 (test 3 (char_count3 "-1-2-3" #x2D))
 (test -1 (char_count4 #f #x2D))
 (test 4 (char_count4 "-1-2-3-" #x2D))
 ;;(test 5 (char_count5 "-1--2-3-" #x2D))
 ;;(test 5 (char_count5 "-1-2--3-" #x2D))
 (test "Hello, World!" (get_message))
 ;; (test "Hello, World!" (get_message2 1))
 ;; (test #f (get_message2 0))
 ;; (test "Hello, World!" (get_message3 1))
 ;; (test #f (get_message3 0))
 ;; memory leak
 (test "Hello, World!" (strdup "Hello, World!"))
 (let ((str "abcdef"))
   (reverse_string str)
   (test "fedcba" str))
 (test "/home/home/on/the/range" (my_getcwd))
 (let ((str1 "abcdef")
       (str2 "xxx"))
   (test #t (strncpy str1 str2))  ;; update after fixing error results
   (test "xxxdef" str1))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structs and opaque types.

(test-ffi
 "structs"
 (begin
   (c-declare "
struct Point {
  double x, y;
};
struct Rectangle {
  struct Point *top_left, *bottom_right;
};
struct Circle {
  struct Point center;
  double radius;
};
typedef struct {
  short r, g, b;
} Color;
typedef struct {
  struct Circle circle;
  Color color;
} ColoredCircle;

struct Point make_point_struct(double x, double y) {
  struct Point res;
  res.x = x;
  res.y = y;
  return res;
}

void set_color(short r, short g, short b, Color* res) {
  res->r = r;
  res->g = g;
  res->b = b;
}

Color* make_color(short r, short g, short b) {
  Color* res = malloc(sizeof(Color));
  if (res) {
    res->r = r;
    res->g = g;
    res->b = b;
  }
  return res;
}

short color_red(Color* color) {
  return color->r;
}

int draw_rect(struct Rectangle* rect, Color color) {
  return 0;
}

double circle_area1(struct Circle* circ) {
  return (circ->radius * circ->radius * 3.14);
}
double circle_area2(struct Circle circ) {
  return circle_area1(&circ);
}
")
   (define-c-struct Point
     predicate: point?
     constructor: (make-point x y)
     (double x point-x point-x-set!)
     (double y point-y point-y-set!))
   (define-c (struct Point) make-point-struct (double double))
   (define-c-struct Rectangle
     predicate: rect?
     constructor: (make-rect top_left bottom_right)
     (Point top_left rect-top-left)
     (Point bottom_right rect-bottom-right))
   ;; constructor with point fails
   (define-c-struct Circle
     predicate: circle?
     constructor: (make-circle radius)
     ((struct Point) center circle-center)
     (double radius circle-radius))
   (define-c double circle_area1 (Circle))
   ;; TODO: struct means no pointer
   ;;(define-c double circle_area2 ((struct Circle)))
   (define-c-type Color predicate: color?)
   (define-c void set_color (short short short (result pointer Color)))
   (define-c Color make_color (short short short))
   (define-c short color_red ((pointer Color)))
   ;;(define-c errno draw_rect (Rectangle Color))
   )
 (test-assert (point? (make-point 1. 2.)))
 (test 1. (point-x (make-point 1. 2.)))
 (test 2. (point-y (make-point 1. 2.)))
 (test 3. (point-x (let ((pt (make-point 1. 2.))) (point-x-set! pt 3.) pt)))
 (test 1. (point-x (make-point-struct 1. 2.)))
 (test 2. (point-y (make-point-struct 1. 2.)))
 ;; need constructor argument checking
 ;;(test-error (rect? (make-rect 1 2)))
 ;; gc miss - we don't preserve the pointers
 (test-assert (rect? (make-rect (make-point 1. 2.) (make-point 3. 4.))))
 (test-assert
     (point? (rect-top-left
              (make-rect (make-point 1. 2.) (make-point 3. 4.)))))
 (test-assert
     (point? (rect-bottom-right
              (make-rect (make-point 1. 2.) (make-point 3. 4.)))))
 (test 2.
     (point-y (rect-top-left
               (make-rect (make-point 1. 2.) (make-point 3. 4.)))))
 (test 3.
     (point-x (rect-bottom-right
               (make-rect (make-point 1. 2.) (make-point 3. 4.)))))
 (test-assert (circle? (make-circle 3.)))
 (test 3.0 (circle-radius (make-circle 3.)))
 (test 28.26 (circle_area1 (make-circle 3.)))
 ;;(test 28.26 (circle_area2 (make-circle 3.)))
 (let ((circle (make-circle 3.)))
   (point-x-set! (circle-center circle) 7.)
   (point-y-set! (circle-center circle) 11.)
   (test 7. (point-x (circle-center circle)))
   (test 11. (point-y (circle-center circle))))
 (test-assert (color? (set_color 1 2 3)))
 (test 1 (color_red (set_color 1 2 3)))
 (test-assert (color? (make_color 1 2 3)))
 (test 1 (color_red (make_color 1 2 3)))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exception wrapping.

(test-ffi
 "exceptions"
 (begin
   (c-declare "
sexp usererror(sexp ctx, sexp self, const char* str) {
  return sexp_maybe_wrap_error(ctx, sexp_user_exception(ctx, self, str, SEXP_NULL));
}
")
   (define-c sexp usererror
     ((value ctx sexp) (value self sexp) string)))
 (test-assert (exception? (usererror "BOOM!")))
 (test-error (usererror 'not-a-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; More complex return parameters.

(test-ffi
 "results"
 (begin
   (c-declare "
struct password {
  char* pw_name;
  char* pw_passwd;
};
char* etc_passwd[] = {
  \"root:12345\",
  \"joe:joe\",
};
int getpwnam_x(char* name, struct password* pwd, char* buf,
               size_t bufsize, struct password** res) {
  int i;
  char* entry;
  for (i=0; i < sizeof(etc_passwd) / sizeof(etc_passwd[0]); i++) {
    entry = etc_passwd[i];
    if (strstr(entry, name) == entry && entry[strlen(name)] == ':') {
      strncpy(buf, entry, bufsize);
      buf[strlen(entry)] = 0;
      buf[strlen(name)] = 0;
      pwd->pw_name = buf;
      pwd->pw_passwd = buf + strlen(name) + 1;
      *res = pwd;
      return 0;
    }
  }
  *res = NULL;
  return -1;
}
")
   (define-c-struct password
     predicate: user?
     (string  pw_name     user-name)
     (string  pw_passwd   user-password))
   (define-c errno getpwnam_x
     (string
      (result password)
      (link string)
      (value (string-length arg2) int)
      (result pointer password))))
 (test-assert (user? (car (getpwnam_x "root" (make-string 1024)))))
 (test "root" (user-name (car (getpwnam_x "root" (make-string 1024)))))
 (test "12345" (user-password (car (getpwnam_x "root" (make-string 1024)))))
 (test-assert (user? (car (getpwnam_x "joe" (make-string 1024)))))
 (test "joe" (user-name (car (getpwnam_x "joe" (make-string 1024)))))
 (test "joe" (user-password (car (getpwnam_x "joe" (make-string 1024)))))
 (test-not (getpwnam_x "hacker" (make-string 1024)))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field introspection and matching.

(test-ffi
 "introspection"
 (begin
   (c-declare "
struct point3d {
  int x, y, z;
};
")
   (define-c-struct point3d
     predicate: point3d?
     constructor: (make-point3d x y z)
     (int x point3d-x point3d-x-set!)
     (int y point3d-y point3d-y-set!)
     (int z point3d-z point3d-z-set!)))
 (test-assert (point3d? (make-point3d 1 2 3)))
 (test '(4 5 6)
     (let ((pt (make-point3d 4 5 6)))
       (list (point3d-x pt) (point3d-y pt) (point3d-z pt))))
 (test '(7 8 9)
     (match (make-point3d 7 8 9)
       (($ point3d a b c) (list a b c))))
 (test '(12 11 10)
     (match (make-point3d 10 11 12)
       ((@ point3d (z a) (y b) (x c)) (list a b c))))
 (test '(13 14 15 42)
     (let ((pt (make-point3d 13 14 15)))
       (match pt
         ((@ point3d (y b) (z (and (set! set-z!) orig-z)) (x c))
          (set-z! 42)
          (list c b orig-z (point3d-z pt))))))
 )

(test-ffi
 "virtual accessors"
 (begin
   (c-declare "
struct VirtComplex {
  double r, phi;
};
double complex_real(struct VirtComplex* c) {
  return c->r * cos(c->phi);
}
double complex_imag(struct VirtComplex* c) {
  return c->r * sin(c->phi);
}
void complex_set(struct VirtComplex* c, double x, double y) {
  c->r = sqrt(x*x + y*y);
  c->phi = atan2(y, x);
}
void complex_real_set(struct VirtComplex* c, double x) {
  complex_set(c, x, complex_imag(c));
}
void complex_imag_set(struct VirtComplex* c, double y) {
  complex_set(c, complex_real(c), y);
}
")
   (define-c-struct VirtComplex
     predicate: virt-complex?
     constructor: (make-virt-complex real imag)
     (double real
             (virt-complex-real function: "complex_real")
             (virt-complex-real-set! function: "complex_real_set"))
     (double imag
             (virt-complex-imag function: "complex_imag")
             (virt-complex-imag-set! function: "complex_imag_set"))
     ))
 (test-assert (virt-complex? (make-virt-complex 1.0 2.0)))
 (test 1.0 (virt-complex-real (make-virt-complex 1.0 2.0)))
 (test 2.0 (virt-complex-imag (make-virt-complex 1.0 2.0)))
 (let ((c (make-virt-complex 1.0 2.0)))
   (test 1.0 (virt-complex-real c))
   (virt-complex-real-set! c 3.0)
   (test 3.0 (virt-complex-real c))
   (test 2.0 (virt-complex-imag c)))
 (test '(5 6 7)
     (let ((c (make-virt-complex 5.0 6.0)))
       (match c
         ((@ VirtComplex (real r) (imag (and (set! set-imag!) orig-i)))
          (set-imag! 7.0)
          (map inexact->exact
               (map round (list r orig-i (virt-complex-imag c))))))))
 )

;; TODO: virtual method accessors

(cleanup-shared-objects!)
(test-end)
