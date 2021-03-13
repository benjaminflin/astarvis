#!/snap/bin/idris2.chez --script

; @generated
(import (chezscheme))
(case (machine-type)
  [(i3le ti3le a6le ta6le) (load-shared-object "libc.so.6")]
  [(i3osx ti3osx a6osx ta6osx) (load-shared-object "libc.dylib")]
  [(i3nt ti3nt a6nt ta6nt) (load-shared-object "msvcrt.dll")                           (load-shared-object "ws2_32.dll")]
  [else (load-shared-object "libc.so")])



(let ()
(define (blodwen-os)
  (case (machine-type)
    [(i3le ti3le a6le ta6le) "unix"]  ; GNU/Linux
    [(i3ob ti3ob a6ob ta6ob) "unix"]  ; OpenBSD
    [(i3fb ti3fb a6fb ta6fb) "unix"]  ; FreeBSD
    [(i3nb ti3nb a6nb ta6nb) "unix"]  ; NetBSD
    [(i3osx ti3osx a6osx ta6osx) "darwin"]
    [(i3nt ti3nt a6nt ta6nt) "windows"]
    [else "unknown"]))

(define blodwen-read-args (lambda (desc)
  (case (vector-ref desc 0)
    ((0) '())
    ((1) (cons (vector-ref desc 2)
               (blodwen-read-args (vector-ref desc 3)))))))
(define b+ (lambda (x y bits) (remainder (+ x y) (ash 1 bits))))
(define b- (lambda (x y bits) (remainder (- x y) (ash 1 bits))))
(define b* (lambda (x y bits) (remainder (* x y) (ash 1 bits))))
(define b/ (lambda (x y bits) (remainder (exact-floor (/ x y)) (ash 1 bits))))

(define integer->bits8 (lambda (x) (modulo x (expt 2 8))))
(define integer->bits16 (lambda (x) (modulo x (expt 2 16))))
(define integer->bits32 (lambda (x) (modulo x (expt 2 32))))
(define integer->bits64 (lambda (x) (modulo x (expt 2 64))))

(define bits16->bits8 (lambda (x) (modulo x (expt 2 8))))
(define bits32->bits8 (lambda (x) (modulo x (expt 2 8))))
(define bits32->bits16 (lambda (x) (modulo x (expt 2 16))))
(define bits64->bits8 (lambda (x) (modulo x (expt 2 8))))
(define bits64->bits16 (lambda (x) (modulo x (expt 2 16))))
(define bits64->bits32 (lambda (x) (modulo x (expt 2 32))))

(define truncate-bits
  (lambda (x bits)
    (if (logbit? bits x)
        (logor x (ash (- 1) bits))
        (logand x (- (ash 1 bits) 1)))))

(define blodwen-bits-shl-signed (lambda (x y bits) (truncate-bits (ash x y) bits)))

(define blodwen-bits-shl (lambda (x y bits) (remainder (ash x y) (ash 1 bits))))
(define blodwen-shl (lambda (x y) (ash x y)))
(define blodwen-shr (lambda (x y) (ash x (- y))))
(define blodwen-and (lambda (x y) (logand x y)))
(define blodwen-or (lambda (x y) (logor x y)))
(define blodwen-xor (lambda (x y) (logxor x y)))

(define cast-num
  (lambda (x)
    (if (number? x) x 0)))
(define destroy-prefix
  (lambda (x)
    (cond
      ((equal? x "") "")
      ((equal? (string-ref x 0) #\#) "")
      (else x))))
(define exact-floor
  (lambda (x)
    (inexact->exact (floor x))))
(define cast-string-int
  (lambda (x)
    (exact-floor (cast-num (string->number (destroy-prefix x))))))
(define cast-int-char
  (lambda (x)
    (if (and (>= x 0)
             (<= x #x10ffff))
        (integer->char x)
        0)))
(define cast-string-double
  (lambda (x)
    (cast-num (string->number (destroy-prefix x)))))

(define (from-idris-list xs)
  (if (= (vector-ref xs 0) 0)
    '()
    (cons (vector-ref xs 1) (from-idris-list (vector-ref xs 2)))))
(define (string-pack xs) (apply string (from-idris-list xs)))
(define (to-idris-list-rev acc xs)
  (if (null? xs)
    acc
    (to-idris-list-rev (vector 1 (car xs) acc) (cdr xs))))
(define (string-unpack s) (to-idris-list-rev (vector 0) (reverse (string->list s))))
(define (string-concat xs) (apply string-append (from-idris-list xs)))
(define string-cons (lambda (x y) (string-append (string x) y)))
(define get-tag (lambda (x) (vector-ref x 0)))
(define string-reverse (lambda (x)
  (list->string (reverse (string->list x)))))
(define (string-substr off len s)
    (let* ((l (string-length s))
          (b (max 0 off))
          (x (max 0 len))
          (end (min l (+ b x))))
          (if (> b l)
              ""
              (substring s b end))))

(define (blodwen-string-iterator-new s)
  0)

(define (blodwen-string-iterator-to-string _ s ofs f)
  (f (substring s ofs (string-length s))))

(define (blodwen-string-iterator-next s ofs)
  (if (>= ofs (string-length s))
      (vector 0)  ; EOF
      (vector 1 (string-ref s ofs) (+ ofs 1))))

(define either-left
  (lambda (x)
    (vector 0 x)))

(define either-right
  (lambda (x)
    (vector 1 x)))

(define blodwen-error-quit
  (lambda (msg)
    (display msg)
    (newline)
    (exit 1)))

(define (blodwen-get-line p)
    (if (port? p)
        (let ((str (get-line p)))
            (if (eof-object? str)
                ""
                str))
        void))

(define (blodwen-get-char p)
    (if (port? p)
        (let ((chr (get-char p)))
            (if (eof-object? chr)
                #\nul
                chr))
        void))

;; Buffers

(define (blodwen-new-buffer size)
  (make-bytevector size 0))

(define (blodwen-buffer-size buf)
  (bytevector-length buf))

(define (blodwen-buffer-setbyte buf loc val)
  (bytevector-u8-set! buf loc val))

(define (blodwen-buffer-getbyte buf loc)
  (bytevector-u8-ref buf loc))

(define (blodwen-buffer-setbits16 buf loc val)
  (bytevector-u16-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getbits16 buf loc)
  (bytevector-u16-ref buf loc (native-endianness)))

(define (blodwen-buffer-setbits32 buf loc val)
  (bytevector-u32-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getbits32 buf loc)
  (bytevector-u32-ref buf loc (native-endianness)))

(define (blodwen-buffer-setbits64 buf loc val)
  (bytevector-u64-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getbits64 buf loc)
  (bytevector-u64-ref buf loc (native-endianness)))

(define (blodwen-buffer-setint32 buf loc val)
  (bytevector-s32-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getint32 buf loc)
  (bytevector-s32-ref buf loc (native-endianness)))

(define (blodwen-buffer-setint buf loc val)
  (bytevector-s64-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getint buf loc)
  (bytevector-s64-ref buf loc (native-endianness)))

(define (blodwen-buffer-setdouble buf loc val)
  (bytevector-ieee-double-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getdouble buf loc)
  (bytevector-ieee-double-ref buf loc (native-endianness)))

(define (blodwen-stringbytelen str)
  (bytevector-length (string->utf8 str)))

(define (blodwen-buffer-setstring buf loc val)
  (let* [(strvec (string->utf8 val))
         (len (bytevector-length strvec))]
    (bytevector-copy! strvec 0 buf loc len)))

(define (blodwen-buffer-getstring buf loc len)
  (let [(newvec (make-bytevector len))]
    (bytevector-copy! buf loc newvec 0 len)
    (utf8->string newvec)))

(define (blodwen-buffer-copydata buf start len dest loc)
  (bytevector-copy! buf start dest loc len))

;; Threads

(define-record thread-handle (semaphore))

(define (blodwen-thread proc)
  (let [(sema (blodwen-make-semaphore 0))]
    (fork-thread (lambda () (proc (vector 0)) (blodwen-semaphore-post sema)))
    (make-thread-handle sema)
    ))

(define (blodwen-thread-wait handle)
  (blodwen-semaphore-wait (thread-handle-semaphore handle)))

;; Thread mailboxes

(define blodwen-thread-data
  (make-thread-parameter #f))

(define (blodwen-get-thread-data ty)
  (blodwen-thread-data))

(define (blodwen-set-thread-data a)
  (blodwen-thread-data a))

;; Semaphore

(define-record semaphore (box mutex condition))

(define (blodwen-make-semaphore init)
  (make-semaphore (box init) (make-mutex) (make-condition)))

(define (blodwen-semaphore-post sema)
  (with-mutex (semaphore-mutex sema)
    (let [(sema-box (semaphore-box sema))]
      (set-box! sema-box (+ (unbox sema-box) 1))
      (condition-signal (semaphore-condition sema))
    )))

(define (blodwen-semaphore-wait sema)
  (with-mutex (semaphore-mutex sema)
    (let [(sema-box (semaphore-box sema))]
      (when (= (unbox sema-box) 0)
        (condition-wait (semaphore-condition sema) (semaphore-mutex sema)))
      (set-box! sema-box (- (unbox sema-box) 1))
      )))

;; Barrier

(define-record barrier (count-box num-threads mutex cond))

(define (blodwen-make-barrier num-threads)
  (make-barrier (box 0) num-threads (make-mutex) (make-condition)))

(define (blodwen-barrier-wait barrier)
  (let [(count-box (barrier-count-box barrier))
        (num-threads (barrier-num-threads barrier))
        (mutex (barrier-mutex barrier))
        (condition (barrier-cond barrier))]
    (with-mutex mutex
    (let* [(count-old (unbox count-box))
           (count-new (+ count-old 1))]
      (set-box! count-box count-new)
      (if (= count-new num-threads)
          (condition-broadcast condition)
          (condition-wait condition mutex))
      ))))

;; Channel

(define-record channel (box mutex semaphore-get semaphore-put))

(define (blodwen-make-channel ty)
  (make-channel
   (box '())
   (make-mutex)
   (blodwen-make-semaphore 0)
   (blodwen-make-semaphore 0)))

(define (blodwen-channel-get ty chan)
  (blodwen-semaphore-post (channel-semaphore-get chan))
  (blodwen-semaphore-wait (channel-semaphore-put chan))
  (with-mutex (channel-mutex chan)
    (let* [(chan-box (channel-box chan))
           (chan-msg-queue (unbox chan-box))]
      (set-box! chan-box (cdr chan-msg-queue))
      (car chan-msg-queue)
      )))

(define (blodwen-channel-put ty chan val)
  (with-mutex (channel-mutex chan)
    (let* [(chan-box (channel-box chan))
           (chan-msg-queue (unbox chan-box))]
      (set-box! chan-box (append chan-msg-queue (list val)))))
  (blodwen-semaphore-post (channel-semaphore-put chan))
  (blodwen-semaphore-wait (channel-semaphore-get chan)))

;; Mutex

(define (blodwen-make-mutex)
  (make-mutex))
(define (blodwen-mutex-acquire mutex)
  (mutex-acquire mutex))
(define (blodwen-mutex-release mutex)
  (mutex-release mutex))

;; Condition variable

(define (blodwen-make-condition)
  (make-condition))
(define (blodwen-condition-wait condition mutex)
  (condition-wait condition mutex))
(define (blodwen-condition-wait-timeout condition mutex timeout)
  (let* [(sec (div timeout 1000000))
         (micro (mod timeout 1000000))]
    (condition-wait condition mutex (make-time 'time-duration (* 1000 micro) sec))))
(define (blodwen-condition-signal condition)
  (condition-signal condition))
(define (blodwen-condition-broadcast condition)
  (condition-broadcast condition))

;; Future

(define-record future-internal (result ready mutex signal))
(define (blodwen-make-future work)
  (let ([future (make-future-internal #f #f (make-mutex) (make-condition))])
    (fork-thread (lambda ()
      (let ([result (work)])
        (with-mutex (future-internal-mutex future)
          (set-future-internal-result! future result)
          (set-future-internal-ready! future #t)
          (condition-broadcast (future-internal-signal future))))))
    future))
(define (blodwen-await-future ty future)
  (let ([mutex (future-internal-mutex future)])
    (with-mutex mutex
      (if (not (future-internal-ready future))
          (condition-wait (future-internal-signal future) mutex))
      (future-internal-result future))))

(define (blodwen-sleep s) (sleep (make-time 'time-duration 0 s)))
(define (blodwen-usleep s)
  (let ((sec (div s 1000000))
        (micro (mod s 1000000)))
       (sleep (make-time 'time-duration (* 1000 micro) sec))))

(define (blodwen-time) (time-second (current-time)))
(define (blodwen-clock-time-utc) (current-time 'time-utc))
(define (blodwen-clock-time-monotonic) (current-time 'time-monotonic))
(define (blodwen-clock-time-duration) (current-time 'time-duration))
(define (blodwen-clock-time-process) (current-time 'time-process))
(define (blodwen-clock-time-thread) (current-time 'time-thread))
(define (blodwen-clock-time-gccpu) (current-time 'time-collector-cpu))
(define (blodwen-clock-time-gcreal) (current-time 'time-collector-real))
(define (blodwen-is-time? clk) (if (time? clk) 1 0))
(define (blodwen-clock-second time) (time-second time))
(define (blodwen-clock-nanosecond time) (time-nanosecond time))

(define (blodwen-args)
  (define (blodwen-build-args args)
    (if (null? args)
        (vector 0) ; Prelude.List
        (vector 1 (car args) (blodwen-build-args (cdr args)))))
    (blodwen-build-args (command-line)))

(define (blodwen-hasenv var)
  (if (eq? (getenv var) #f) 0 1))

(define (blodwen-system cmd)
  (system cmd))

;; Randoms
(define random-seed-register 0)
(define (initialize-random-seed-once)
  (if (= (virtual-register random-seed-register) 0)
      (let ([seed (time-nanosecond (current-time))])
        (set-virtual-register! random-seed-register seed)
        (random-seed seed))))

(define (blodwen-random-seed seed)
  (set-virtual-register! random-seed-register seed)
  (random-seed seed))
(define blodwen-random
  (case-lambda
    ;; no argument, pick a real value from [0, 1.0)
    [() (begin
          (initialize-random-seed-once)
          (random 1.0))]
    ;; single argument k, pick an integral value from [0, k)
    [(k)
      (begin
        (initialize-random-seed-once)
        (if (> k 0)
              (random k)
              (assertion-violationf 'blodwen-random "invalid range argument ~a" k)))]))

;; For finalisers

(define blodwen-finaliser (make-guardian))
(define (blodwen-register-object obj proc)
  (let [(x (cons obj proc))]
       (blodwen-finaliser x)
       x))
(define blodwen-run-finalisers
  (lambda ()
    (let run ()
      (let ([x (blodwen-finaliser)])
        (when x
          (((cdr x) (car x)) 'erased)
          (run))))))
(define prim__add_Integer (lambda (arg-0 arg-1) (+ arg-0 arg-1)))
(define prim__sub_Integer (lambda (arg-0 arg-1) (- arg-0 arg-1)))
(define prim__mul_Integer (lambda (arg-0 arg-1) (* arg-0 arg-1)))
(define prim__lt_Integer (lambda (arg-0 arg-1) (or (and (< arg-0 arg-1) 1) 0)))
(define prim__lt_Double (lambda (arg-0 arg-1) (or (and (< arg-0 arg-1) 1) 0)))
(define prim__lte_Integer (lambda (arg-0 arg-1) (or (and (<= arg-0 arg-1) 1) 0)))
(define prim__lte_Double (lambda (arg-0 arg-1) (or (and (<= arg-0 arg-1) 1) 0)))
(define prim__eq_Integer (lambda (arg-0 arg-1) (or (and (= arg-0 arg-1) 1) 0)))
(define prim__eq_Double (lambda (arg-0 arg-1) (or (and (= arg-0 arg-1) 1) 0)))
(define prim__gte_Integer (lambda (arg-0 arg-1) (or (and (>= arg-0 arg-1) 1) 0)))
(define prim__gte_Double (lambda (arg-0 arg-1) (or (and (>= arg-0 arg-1) 1) 0)))
(define prim__gt_Integer (lambda (arg-0 arg-1) (or (and (> arg-0 arg-1) 1) 0)))
(define prim__gt_Double (lambda (arg-0 arg-1) (or (and (> arg-0 arg-1) 1) 0)))
(define prim__believe_me (lambda (arg-0 arg-1 arg-2) arg-2))
(define prim__cast_IntegerDouble (lambda (arg-0) (exact->inexact arg-0)))
(define AStar-case--test-1388 (lambda (arg-0) (let ((sc0 arg-0)) (let ((e-2 (vector-ref sc0 1))) (lambda (eta-0) (let ((sc1 (let ((sc2 e-2)) (let ((e-1 (vector-ref sc2 2))) e-1)))) (let ((e-0 (vector-ref sc1 1))) e-0)))))))
(define AStar-case--caseC-32blockC-32inC-32step-1223 (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 arg-3)) (let ((e-0 (vector-ref sc0 1))) (let ((e-1 (vector-ref sc0 2))) (let ((sc1 e-1)) (vector 0 e-0 (vector 0 arg-2 (DataC-45SortedSet-delete 'erased (DataC-45Vect-head 'erased 'erased (let ((sc2 arg-1)) (let ((e-4 (vector-ref sc2 2))) e-4))) (let ((sc2 (let ((sc3 arg-3)) (let ((e-4 (vector-ref sc3 2))) e-4)))) (let ((e-4 (vector-ref sc2 2))) e-4)))))))))))
(define AStar-case--caseC-32blockC-32inC-32step-1176 (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 arg-3)) (cond ((equal? sc0 0) (lambda () (lambda (_-1415) (lambda (s) (lambda (w) (lambda (eta-0) (vector 0 (vector 0 arg-1) (vector 0 s w)))))))) (else (lambda () (PreludeC-45Interfaces-C-62C-62 'erased 'erased (vector 0 (vector 0 (lambda (b) (lambda (a) (lambda (func) (lambda (arg-143) (lambda (r) (lambda (s) (lambda (w) (PreludeC-45Interfaces-C-60C-36C-62 'erased 'erased 'erased (lambda (b-0) (lambda (a-0) (lambda (func-0) (lambda (arg-144) (lambda (eta-0) (PreludeC-45IO-map_Functor_IO 'erased 'erased func-0 arg-144 eta-0)))))) (lambda (lamc-0) (let ((sc1 lamc-0)) (let ((e-2 (vector-ref sc1 1))) (let ((e-3 (vector-ref sc1 2))) (let ((sc2 e-3)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (vector 0 (func e-2) (vector 0 e-6 e-7))))))))) (((arg-143 r) s) w))))))))) (lambda (a) (lambda (arg-556) (lambda (_-1415) (lambda (s) (lambda (w) (lambda (eta-0) (vector 0 arg-556 (vector 0 s w)))))))) (lambda (b) (lambda (a) (lambda (arg-557) (lambda (arg-559) (lambda (r) (lambda (s) (lambda (w) (lambda (eta-0) (let ((act-24 ((((arg-557 r) s) w) eta-0))) (let ((sc1 act-24)) (let ((e-2 (vector-ref sc1 1))) (let ((e-3 (vector-ref sc1 2))) (let ((sc2 e-3)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (let ((act-25 ((((arg-559 r) e-6) e-7) eta-0))) (let ((sc3 act-25)) (let ((e-5 (vector-ref sc3 1))) (let ((e-4 (vector-ref sc3 2))) (let ((sc4 e-4)) (let ((e-9 (vector-ref sc4 1))) (let ((e-8 (vector-ref sc4 2))) (vector 0 (e-2 e-5) (vector 0 e-9 e-8))))))))))))))))))))))))) (lambda (b) (lambda (a) (lambda (arg-855) (lambda (arg-856) (lambda (r) (lambda (s) (lambda (w) (lambda (eta-0) (let ((act-24 ((((arg-855 r) s) w) eta-0))) (let ((sc1 act-24)) (let ((e-2 (vector-ref sc1 1))) (let ((e-3 (vector-ref sc1 2))) (let ((sc2 e-3)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (((((arg-856 e-2) r) e-6) e-7) eta-0)))))))))))))))) (lambda (a) (lambda (arg-858) (lambda (r) (lambda (s) (lambda (w) (lambda (eta-0) (let ((act-24 ((((arg-858 r) s) w) eta-0))) (let ((sc1 act-24)) (let ((e-2 (vector-ref sc1 1))) (let ((e-3 (vector-ref sc1 2))) (let ((sc2 e-3)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) ((((e-2 r) e-6) e-7) eta-0))))))))))))))) (lambda (eta-0) (lambda (eta-1) (lambda (eta-2) (lambda (eta-3) (AStar-explore arg-1 eta-0 eta-1 eta-2 eta-3))))) (lambda () (lambda (_-1415) (lambda (s) (lambda (w) (lambda (eta-0) (vector 0 (vector 1 ) (vector 0 s w))))))))))))))
(define AStar-case--step-1134 (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (case (vector-ref sc0 0) ((0) (lambda (_-1415) (lambda (s) (lambda (w) (lambda (eta-0) (vector 0 (vector 2 ) (vector 0 s w))))))) (else (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (PreludeC-45Interfaces-C-62C-62 'erased 'erased (vector 0 (vector 0 (lambda (b) (lambda (a) (lambda (func) (lambda (arg-143) (lambda (r) (lambda (s) (lambda (w) (PreludeC-45Interfaces-C-60C-36C-62 'erased 'erased 'erased (lambda (b-0) (lambda (a-0) (lambda (func-0) (lambda (arg-144) (lambda (eta-0) (PreludeC-45IO-map_Functor_IO 'erased 'erased func-0 arg-144 eta-0)))))) (lambda (lamc-0) (let ((sc1 lamc-0)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (vector 0 (func e-5) (vector 0 e-6 e-7))))))))) (((arg-143 r) s) w))))))))) (lambda (a) (lambda (arg-556) (lambda (_-1415) (lambda (s) (lambda (w) (lambda (eta-0) (vector 0 arg-556 (vector 0 s w)))))))) (lambda (b) (lambda (a) (lambda (arg-557) (lambda (arg-559) (lambda (r) (lambda (s) (lambda (w) (lambda (eta-0) (let ((act-24 ((((arg-557 r) s) w) eta-0))) (let ((sc1 act-24)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (let ((act-25 ((((arg-559 r) e-6) e-7) eta-0))) (let ((sc3 act-25)) (let ((e-9 (vector-ref sc3 1))) (let ((e-8 (vector-ref sc3 2))) (let ((sc4 e-8)) (let ((e-11 (vector-ref sc4 1))) (let ((e-10 (vector-ref sc4 2))) (vector 0 (e-5 e-9) (vector 0 e-11 e-10))))))))))))))))))))))))) (lambda (b) (lambda (a) (lambda (arg-855) (lambda (arg-856) (lambda (r) (lambda (s) (lambda (w) (lambda (eta-0) (let ((act-24 ((((arg-855 r) s) w) eta-0))) (let ((sc1 act-24)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (((((arg-856 e-5) r) e-6) e-7) eta-0)))))))))))))))) (lambda (a) (lambda (arg-858) (lambda (r) (lambda (s) (lambda (w) (lambda (eta-0) (let ((act-24 ((((arg-858 r) s) w) eta-0))) (let ((sc1 act-24)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) ((((e-5 r) e-6) e-7) eta-0))))))))))))))) (ControlC-45MonadC-45StateC-45Interface-modify 'erased 'erased (vector 0 (vector 0 (vector 0 (lambda (b) (lambda (a) (lambda (func) (lambda (arg-143) (lambda (r) (lambda (s) (lambda (w) (PreludeC-45Interfaces-C-60C-36C-62 'erased 'erased 'erased (lambda (b-0) (lambda (a-0) (lambda (func-0) (lambda (arg-144) (lambda (eta-0) (PreludeC-45IO-map_Functor_IO 'erased 'erased func-0 arg-144 eta-0)))))) (lambda (lamc-0) (let ((sc1 lamc-0)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (vector 0 (func e-5) (vector 0 e-6 e-7))))))))) (((arg-143 r) s) w))))))))) (lambda (a) (lambda (arg-556) (lambda (_-1415) (lambda (s) (lambda (w) (lambda (eta-0) (vector 0 arg-556 (vector 0 s w)))))))) (lambda (b) (lambda (a) (lambda (arg-557) (lambda (arg-559) (lambda (r) (lambda (s) (lambda (w) (lambda (eta-0) (let ((act-24 ((((arg-557 r) s) w) eta-0))) (let ((sc1 act-24)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (let ((act-25 ((((arg-559 r) e-6) e-7) eta-0))) (let ((sc3 act-25)) (let ((e-9 (vector-ref sc3 1))) (let ((e-8 (vector-ref sc3 2))) (let ((sc4 e-8)) (let ((e-11 (vector-ref sc4 1))) (let ((e-10 (vector-ref sc4 2))) (vector 0 (e-5 e-9) (vector 0 e-11 e-10))))))))))))))))))))))))) (lambda (b) (lambda (a) (lambda (arg-855) (lambda (arg-856) (lambda (r) (lambda (s) (lambda (w) (lambda (eta-0) (let ((act-24 ((((arg-855 r) s) w) eta-0))) (let ((sc1 act-24)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (((((arg-856 e-5) r) e-6) e-7) eta-0)))))))))))))))) (lambda (a) (lambda (arg-858) (lambda (r) (lambda (s) (lambda (w) (lambda (eta-0) (let ((act-24 ((((arg-858 r) s) w) eta-0))) (let ((sc1 act-24)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) ((((e-5 r) e-6) e-7) eta-0))))))))))))))) (lambda (_-2064) (lambda (s) (lambda (w) (lambda (eta-0) (vector 0 s (vector 0 s w)))))) (lambda (arg-1404) (lambda (_-2106) (lambda (_-2108) (lambda (w) (lambda (eta-0) (vector 0 (vector 0 ) (vector 0 arg-1404 w))))))) (lambda (a) (lambda (arg-1405) (lambda (_-2153) (lambda (s) (lambda (w) (ControlC-45MonadC-45StateC-45Interface-case--state-2160 'erased 'erased 'erased 'erased 'erased (vector 0 (vector 0 (lambda (b) (lambda (a-0) (lambda (func) (lambda (arg-143) (lambda (eta-0) (PreludeC-45IO-map_Functor_IO 'erased 'erased func arg-143 eta-0)))))) (lambda (a-0) (lambda (arg-556) (lambda (eta-0) arg-556))) (lambda (b) (lambda (a-0) (lambda (arg-557) (lambda (arg-559) (lambda (eta-0) (let ((act-17 (arg-557 eta-0))) (let ((act-16 (arg-559 eta-0))) (act-17 act-16))))))))) (lambda (b) (lambda (a-0) (lambda (arg-855) (lambda (arg-856) (lambda (eta-0) (let ((act-24 (arg-855 eta-0))) ((arg-856 act-24) eta-0))))))) (lambda (a-0) (lambda (arg-858) (lambda (eta-0) (let ((act-29 (arg-858 eta-0))) (act-29 eta-0)))))) arg-1405 _-2153 s w (arg-1405 s)))))))) (lambda (st) (let ((sc1 st)) (let ((e-0 (vector-ref sc1 1))) (let ((e-1 (vector-ref sc1 2))) (let ((sc2 e-1)) (vector 0 e-0 (vector 0 e-3 (DataC-45SortedSet-delete 'erased (DataC-45Vect-head 'erased 'erased (let ((sc3 e-2)) (let ((e-6 (vector-ref sc3 2))) e-6))) (let ((sc3 (let ((sc4 st)) (let ((e-6 (vector-ref sc4 2))) e-6)))) (let ((e-6 (vector-ref sc3 2))) e-6))))))))))) (AStar-case--caseC-32blockC-32inC-32step-1176 arg-0 e-2 e-3 (PreludeC-45EqOrd-C-60_Ord_Double (let ((sc1 e-2)) (let ((e-1 (vector-ref sc1 1))) e-1)) 0.1))))))))))
(define AStar-case--explore-1090 (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 arg-2)) (let ((e-0 (vector-ref sc0 1))) (vector 0 e-0 (DataC-45Vect-foldl_Foldable_C-40VectC-32C-36nC-41 'erased 'erased 'erased (lambda (eta-0) (lambda (eta-1) (PreludeC-45Basics-flip 'erased 'erased 'erased (lambda (eta-2) (lambda (eta-3) (AStar-push eta-2 eta-3))) eta-0 eta-1))) (let ((sc1 arg-2)) (let ((e-2 (vector-ref sc1 2))) e-2)) (let ((sc1 arg-3)) (let ((e-3 (vector-ref sc1 2))) e-3))))))))
(define AStar-case--exploreC-44paths-1012 (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 arg-3)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (vector 0 e-2 (PreludeC-45Interfaces-C-60C-36C-62 'erased 'erased 'erased (lambda (eta-0) (lambda (eta-1) (lambda (eta-2) (lambda (eta-3) (DataC-45Vect-map_Functor_C-40VectC-32C-36nC-41 'erased 'erased 'erased eta-2 eta-3))))) (lambda (t) (AStar-appendTile (let ((sc1 arg-2)) (let ((e-4 (vector-ref sc1 4))) ((e-4 (let ((sc2 arg-2)) (let ((e-7 (vector-ref sc2 3))) e-7))) t))) t arg-0)) e-3)))))))
(define AStar-case--push-942 (lambda (arg-0 arg-1) (let ((sc0 arg-0)) (vector 0 (AStar-queueInsert arg-1 (let ((sc1 arg-0)) (let ((e-3 (vector-ref sc1 1))) e-3))) (DataC-45SortedSet-insert 'erased (DataC-45Vect-head 'erased 'erased (let ((sc1 arg-1)) (let ((e-2 (vector-ref sc1 2))) e-2))) (let ((sc1 arg-0)) (let ((e-2 (vector-ref sc1 2))) e-2)))))))
(define AStar-case--queueInsert-913 (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 arg-3)) (cond ((equal? sc0 0) (vector 1 arg-2 (vector 1 arg-0 arg-1))) (else (vector 1 arg-0 (AStar-queueInsert arg-2 arg-1)))))))
(define AStar-case--max-659 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) arg-1) (else arg-0)))))
(define AStar-case--min-645 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) arg-1) (else arg-0)))))
(define AStar-n--3694-951-succsC-39 (lambda (arg-0 arg-1 arg-2) (DataC-45Vect-filter 'erased 'erased (lambda (eta-0) (AStar-n--3694-950-check arg-0 arg-1 arg-2 eta-0)) (AStar-succs (DataC-45Vect-head 'erased 'erased (let ((sc0 arg-0)) (let ((e-2 (vector-ref sc0 2))) e-2)))))))
(define AStar-n--3694-952-paths (lambda (arg-0 arg-1 arg-2) (AStar-case--exploreC-44paths-1012 arg-0 arg-2 arg-1 (AStar-n--3694-951-succsC-39 arg-0 arg-1 arg-2))))
(define AStar-n--3694-950-check (lambda (arg-0 arg-1 arg-2 arg-3) (let ((C-58C-58 (lambda (eta-0) (lambda (eta-1) (lambda (eta-2) (lambda (eta-3) (vector 1 eta-2 eta-3))))))) ((PreludeC-45Interfaces-all 'erased 'erased (vector 0 (lambda (acc) (lambda (elem) (lambda (func) (lambda (init) (lambda (input) (DataC-45Vect-foldr_Foldable_C-40VectC-32C-36nC-41 'erased 'erased 'erased func init input)))))) (lambda (elem) (lambda (acc) (lambda (func) (lambda (init) (lambda (input) (DataC-45Vect-foldl_Foldable_C-40VectC-32C-36nC-41 'erased 'erased 'erased func init input)))))) (lambda (elem) (lambda (arg-1123) (DataC-45Vect-null_Foldable_C-40VectC-32C-36nC-41 'erased 'erased arg-1123)))) (lambda (eta-0) eta-0)) (PreludeC-45Interfaces-C-60C-36C-62 'erased 'erased 'erased (lambda (eta-0) (lambda (eta-1) (lambda (eta-2) (lambda (eta-3) (DataC-45Vect-map_Functor_C-40VectC-32C-36nC-41 'erased 'erased 'erased eta-2 eta-3))))) (lambda (eta-0) (PreludeC-45Basics-not (DataC-45SortedSet-contains 'erased arg-3 eta-0))) ((((C-58C-58 'erased) 'erased) (let ((sc0 arg-1)) (let ((e-0 (vector-ref sc0 1))) e-0))) ((((C-58C-58 'erased) 'erased) (let ((sc0 (let ((sc1 arg-2)) (let ((e-1 (vector-ref sc1 2))) e-1)))) (let ((e-1 (vector-ref sc0 2))) e-1))) ((((C-58C-58 'erased) 'erased) (let ((sc0 arg-2)) (let ((e-0 (vector-ref sc0 1))) e-0))) (vector 0 )))))))))
(define AStarC-45Path-rf--trail (lambda (arg-0) (let ((sc0 arg-0)) (let ((e-2 (vector-ref sc0 2))) e-2))))
(define AStarC-45Frontier-rf--set (lambda (arg-0) (let ((sc0 arg-0)) (let ((e-1 (vector-ref sc0 2))) e-1))))
(define AStarC-45Frontier-rf--queue (lambda (arg-0) (let ((sc0 arg-0)) (let ((e-0 (vector-ref sc0 1))) e-0))))
(define AStarC-45Params-rf--map (lambda (arg-0) (let ((sc0 arg-0)) (let ((e-0 (vector-ref sc0 1))) e-0))))
(define AStarC-45Params-rf--heuristic (lambda (arg-0) (let ((sc0 arg-0)) (let ((e-3 (vector-ref sc0 4))) e-3))))
(define AStarC-45Params-rf--goal (lambda (arg-0) (let ((sc0 arg-0)) (let ((e-2 (vector-ref sc0 3))) e-2))))
(define AStarC-45State-rf--frontier (lambda (arg-0) (let ((sc0 arg-0)) (let ((e-1 (vector-ref sc0 2))) e-1))))
(define AStarC-45State-rf--explored (lambda (arg-0) (let ((sc0 arg-0)) (let ((e-0 (vector-ref sc0 1))) e-0))))
(define AStarC-45Path-rf--cost (lambda (arg-0) (let ((sc0 arg-0)) (let ((e-1 (vector-ref sc0 1))) e-1))))
(define AStar-min_Ord_Path (lambda (arg-0 arg-1) (AStar-case--min-645 arg-1 arg-0 (AStar-C-60_Ord_Path arg-0 arg-1))))
(define AStar-max_Ord_Path (lambda (arg-0 arg-1) (AStar-case--max-659 arg-1 arg-0 (AStar-C-62_Ord_Path arg-0 arg-1))))
(define AStar-compare_Ord_Path (lambda (arg-0 arg-1) (PreludeC-45EqOrd-compare_Ord_Double (let ((sc0 arg-0)) (let ((e-1 (vector-ref sc0 1))) e-1)) (let ((sc0 arg-1)) (let ((e-1 (vector-ref sc0 1))) e-1)))))
(define AStar-__Impl_Ord_Path (lambda () (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (AStar-C-61C-61_Eq_Path arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (AStar-C-47C-61_Eq_Path arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (AStar-compare_Ord_Path arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (AStar-C-60_Ord_Path arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (AStar-C-62_Ord_Path arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (AStar-C-60C-61_Ord_Path arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (AStar-C-62C-61_Ord_Path arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (AStar-max_Ord_Path arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (AStar-min_Ord_Path arg-383 arg-384))))))
(define AStar-__Impl_Eq_Path (lambda () (vector 0 (lambda (arg-2) (lambda (arg-3) (AStar-C-61C-61_Eq_Path arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (AStar-C-47C-61_Eq_Path arg-4 arg-5))))))
(define AStar-C-62_Ord_Path (lambda (arg-0 arg-1) (PreludeC-45EqOrd-C-61C-61_Eq_Ordering (AStar-compare_Ord_Path arg-0 arg-1) 2)))
(define AStar-C-62C-61_Ord_Path (lambda (arg-0 arg-1) (PreludeC-45EqOrd-C-47C-61_Eq_Ordering (AStar-compare_Ord_Path arg-0 arg-1) 0)))
(define AStar-C-61C-61_Eq_Path (lambda (arg-0 arg-1) (PreludeC-45EqOrd-C-61C-61_Eq_Double (let ((sc0 arg-0)) (let ((e-1 (vector-ref sc0 1))) e-1)) (let ((sc0 arg-1)) (let ((e-1 (vector-ref sc0 1))) e-1)))))
(define AStar-C-60_Ord_Path (lambda (arg-0 arg-1) (PreludeC-45EqOrd-C-61C-61_Eq_Ordering (AStar-compare_Ord_Path arg-0 arg-1) 0)))
(define AStar-C-60C-61_Ord_Path (lambda (arg-0 arg-1) (PreludeC-45EqOrd-C-47C-61_Eq_Ordering (AStar-compare_Ord_Path arg-0 arg-1) 2)))
(define AStar-C-47C-61_Eq_Path (lambda (arg-0 arg-1) (PreludeC-45Basics-not (AStar-C-61C-61_Eq_Path arg-0 arg-1))))
(define AStar-test (lambda (ext-0) (let ((act-24 ((PreludeC-45Interfaces-C-60C-36C-62 'erased 'erased 'erased (Builtin-fst 'erased 'erased (vector 0 (lambda (eta-0) (lambda (eta-1) (lambda (eta-2) (lambda (eta-3) (lambda (eta-4) (PreludeC-45IO-map_Functor_IO 'erased 'erased eta-2 eta-3 eta-4)))))) (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45Interfaces-C-60C-43C-62_Semigroup_C-40C-124UnitC-44MkUnitC-124C-41 arg-2 arg-3))) (PreludeC-45Interfaces-neutral_Monoid_C-40C-124UnitC-44MkUnitC-124C-41)))) (lambda (lamc-0) (let ((sc0 lamc-0)) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (vector 0 e-6 e-7))))))) (lambda (eta-0) (AStar-step (AStar-exParams) (vector 0 (DataC-45SortedSet-fromList 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (DataC-45Vect-C-61C-61_Eq_C-40C-40VectC-32C-36nC-41C-32C-36aC-41 'erased 'erased (vector 0 (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-4 arg-5))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (DataC-45Vect-C-47C-61_Eq_C-40C-40VectC-32C-36nC-41C-32C-36aC-41 'erased 'erased (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-6) (lambda (arg-7) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-6 arg-7)))) arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (DataC-45Vect-compare_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-compare_Ord_Integer arg-373 arg-374))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (DataC-45Vect-C-60_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-60_Ord_Integer arg-375 arg-376))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (DataC-45Vect-C-62_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-62_Ord_Integer arg-377 arg-378))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (DataC-45Vect-C-60C-61_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (DataC-45Vect-C-62C-61_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-381 arg-382))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (DataC-45Vect-max_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-max_Ord_Integer arg-383 arg-384))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (DataC-45Vect-min_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-385) (lambda (arg-386) (PreludeC-45EqOrd-min_Ord_Integer arg-385 arg-386)))) arg-383 arg-384)))) (vector 0 )) (vector 0 (vector 1 (vector 0 5.0 (vector 1 (vector 1 0 (vector 1 0 (vector 0 ))) (vector 0 ))) (vector 0 )) (DataC-45SortedSet-fromList 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (DataC-45Vect-C-61C-61_Eq_C-40C-40VectC-32C-36nC-41C-32C-36aC-41 'erased 'erased (vector 0 (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-4 arg-5))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (DataC-45Vect-C-47C-61_Eq_C-40C-40VectC-32C-36nC-41C-32C-36aC-41 'erased 'erased (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-6) (lambda (arg-7) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-6 arg-7)))) arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (DataC-45Vect-compare_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-compare_Ord_Integer arg-373 arg-374))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (DataC-45Vect-C-60_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-60_Ord_Integer arg-375 arg-376))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (DataC-45Vect-C-62_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-62_Ord_Integer arg-377 arg-378))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (DataC-45Vect-C-60C-61_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (DataC-45Vect-C-62C-61_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-381 arg-382))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (DataC-45Vect-max_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-max_Ord_Integer arg-383 arg-384))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (DataC-45Vect-min_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-385) (lambda (arg-386) (PreludeC-45EqOrd-min_Ord_Integer arg-385 arg-386)))) arg-383 arg-384)))) (vector 1 (vector 1 0 (vector 1 0 (vector 0 ))) (vector 0 ))))) (let ((sc0 (Builtin-snd 'erased 'erased (vector 0 (lambda (eta-1) (lambda (eta-2) (lambda (eta-3) (lambda (eta-4) (lambda (eta-5) (PreludeC-45IO-map_Functor_IO 'erased 'erased eta-3 eta-4 eta-5)))))) (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45Interfaces-C-60C-43C-62_Semigroup_C-40C-124UnitC-44MkUnitC-124C-41 arg-2 arg-3))) (PreludeC-45Interfaces-neutral_Monoid_C-40C-124UnitC-44MkUnitC-124C-41)))))) (let ((e-2 (vector-ref sc0 2))) e-2)) eta-0))) ext-0))) (let ((sc0 act-24)) (let ((e-2 (vector-ref sc0 1))) (let ((sc1 (let ((sc2 e-2)) (let ((e-1 (vector-ref sc2 2))) e-1)))) (let ((e-0 (vector-ref sc1 1))) e-0)))))))
(define AStar-succs (lambda (arg-0) (let ((sc0 arg-0)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (let ((sc2 e-7)) (vector 1 (vector 1 (+ e-2 1) (vector 1 e-6 (vector 0 ))) (vector 1 (vector 1 (PreludeC-45Num-C-45_Neg_Integer e-2 1) (vector 1 e-6 (vector 0 ))) (vector 1 (vector 1 e-2 (vector 1 (+ e-6 1) (vector 0 ))) (vector 1 (vector 1 e-2 (vector 1 (PreludeC-45Num-C-45_Neg_Integer e-6 1) (vector 0 ))) (vector 0 ))))))))))))))
(define AStar-step (lambda (ext-0 ext-1 ext-2 ext-3) (let ((act-24 (vector 0 ext-1 (vector 0 ext-1 ext-2)))) (let ((sc0 act-24)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (((((AStar-case--step-1134 e-2 (let ((sc2 (let ((sc3 e-2)) (let ((e-1 (vector-ref sc3 2))) e-1)))) (let ((e-0 (vector-ref sc2 1))) e-0))) ext-0) e-6) e-7) ext-3))))))))))
(define AStar-queueInsert (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (case (vector-ref sc0 0) ((0) (vector 1 arg-0 (vector 0 ))) (else (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (AStar-case--queueInsert-913 e-2 e-3 arg-0 (AStar-C-60_Ord_Path arg-0 e-2)))))))))
(define AStar-push (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (vector 0 (AStar-queueInsert arg-0 (let ((sc1 arg-1)) (let ((e-3 (vector-ref sc1 1))) e-3))) (DataC-45SortedSet-insert 'erased (DataC-45Vect-head 'erased 'erased (let ((sc1 arg-0)) (let ((e-2 (vector-ref sc1 2))) e-2))) (let ((sc1 arg-1)) (let ((e-2 (vector-ref sc1 2))) e-2)))))))
(define AStar-explore (lambda (arg-0 ext-0 ext-1 ext-2 ext-3) (let ((act-24 (vector 0 ext-0 (vector 0 ext-1 ext-2)))) (let ((sc0 act-24)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (let ((act-25 (vector 0 e-6 (vector 0 e-6 e-7)))) (let ((sc2 act-25)) (let ((e-5 (vector-ref sc2 1))) (let ((e-4 (vector-ref sc2 2))) (let ((sc3 e-4)) (let ((e-8 (vector-ref sc3 2))) (let ((ps (AStar-n--3694-952-paths arg-0 e-2 e-5))) (vector 0 (vector 0 ) (vector 0 (let ((sc4 e-5)) (let ((e-0 (vector-ref sc4 1))) (vector 0 e-0 (DataC-45Vect-foldl_Foldable_C-40VectC-32C-36nC-41 'erased 'erased 'erased (lambda (eta-0) (lambda (eta-1) (PreludeC-45Basics-flip 'erased 'erased 'erased (lambda (eta-2) (lambda (eta-3) (AStar-push eta-2 eta-3))) eta-0 eta-1))) (let ((sc5 e-5)) (let ((e-10 (vector-ref sc5 2))) e-10)) (let ((sc5 ps)) (let ((e-10 (vector-ref sc5 2))) e-10)))))) e-8))))))))))))))))))
(define AStar-exParams (lambda () (vector 0 (DataC-45SortedSet-fromList 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (DataC-45Vect-C-61C-61_Eq_C-40C-40VectC-32C-36nC-41C-32C-36aC-41 'erased 'erased (vector 0 (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-4 arg-5))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (DataC-45Vect-C-47C-61_Eq_C-40C-40VectC-32C-36nC-41C-32C-36aC-41 'erased 'erased (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-6) (lambda (arg-7) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-6 arg-7)))) arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (DataC-45Vect-compare_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-compare_Ord_Integer arg-373 arg-374))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (DataC-45Vect-C-60_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-60_Ord_Integer arg-375 arg-376))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (DataC-45Vect-C-62_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-62_Ord_Integer arg-377 arg-378))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (DataC-45Vect-C-60C-61_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (DataC-45Vect-C-62C-61_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-381 arg-382))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (DataC-45Vect-max_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-max_Ord_Integer arg-383 arg-384))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384)))) arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (DataC-45Vect-min_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-385) (lambda (arg-386) (PreludeC-45EqOrd-min_Ord_Integer arg-385 arg-386)))) arg-383 arg-384)))) (vector 0 )) (vector 1 0 (vector 1 0 (vector 0 ))) (vector 1 0 (vector 1 5 (vector 0 ))) (lambda (eta-0) (lambda (eta-1) (AStar-euclidean eta-0 eta-1))))))
(define AStar-euclidean (lambda (arg-0 arg-1) (let ((sc0 arg-0)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (let ((sc2 e-7)) (let ((sc3 arg-1)) (let ((e-11 (vector-ref sc3 1))) (let ((e-12 (vector-ref sc3 2))) (let ((sc4 e-12)) (let ((e-15 (vector-ref sc4 1))) (let ((e-16 (vector-ref sc4 2))) (let ((sc5 e-16)) (PreludeC-45Num-fromInteger_Num_Double (+ (* (PreludeC-45Num-C-45_Neg_Integer e-2 e-11) (PreludeC-45Num-C-45_Neg_Integer e-2 e-11)) (* (PreludeC-45Num-C-45_Neg_Integer e-6 e-15) (PreludeC-45Num-C-45_Neg_Integer e-6 e-15))))))))))))))))))))
(define AStar-appendTile (lambda (arg-0 arg-1 arg-2) (vector 0 arg-0 (vector 1 arg-1 (let ((sc0 arg-2)) (let ((e-2 (vector-ref sc0 2))) e-2))))))
(define DataC-45Vect-case--caseC-32blockC-32inC-32filter-3057 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8) (let ((sc0 arg-8)) (cond ((equal? sc0 0) (vector 0 (+ 1 arg-5) (vector 1 arg-6 arg-7))) (else (vector 0 arg-5 arg-7))))))
(define DataC-45Vect-case--filter-3030 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6) (let ((sc0 arg-6)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (DataC-45Vect-case--caseC-32blockC-32inC-32filter-3057 'erased 'erased arg-3 arg-4 'erased e-2 arg-2 e-3 (arg-4 arg-2)))))))
(define DataC-45Vect-case--compare-1861 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8) (let ((sc0 arg-8)) (cond ((equal? sc0 1) (DataC-45Vect-compare_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased arg-1 arg-4 arg-6))(else arg-8)))))
(define DataC-45Vect-case--max-1747 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5) (let ((sc0 arg-5)) (cond ((equal? sc0 0) arg-4) (else arg-3)))))
(define DataC-45Vect-case--min-1712 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5) (let ((sc0 arg-5)) (cond ((equal? sc0 0) arg-4) (else arg-3)))))
(define DataC-45Vect-null_Foldable_C-40VectC-32C-36nC-41 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (case (vector-ref sc0 0) ((0) (lambda () 0))(else (lambda () 1))))))
(define DataC-45Vect-min_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (DataC-45Vect-case--min-1712 'erased 'erased arg-2 arg-4 arg-3 (DataC-45Vect-C-60_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased arg-2 arg-3 arg-4))))
(define DataC-45Vect-max_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (DataC-45Vect-case--max-1747 'erased 'erased arg-2 arg-4 arg-3 (DataC-45Vect-C-62_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased arg-2 arg-3 arg-4))))
(define DataC-45Vect-map_Functor_C-40VectC-32C-36nC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (let ((sc0 arg-4)) (case (vector-ref sc0 0) ((0) (vector 0 )) (else (let ((e-3 (vector-ref sc0 1))) (let ((e-4 (vector-ref sc0 2))) (vector 1 (arg-3 e-3) (DataC-45Vect-map_Functor_C-40VectC-32C-36nC-41 'erased 'erased 'erased arg-3 e-4)))))))))
(define DataC-45Vect-foldr_Foldable_C-40VectC-32C-36nC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5) (DataC-45Vect-foldrImpl 'erased 'erased 'erased arg-3 arg-4 (lambda (eta-0) eta-0) arg-5)))
(define DataC-45Vect-foldl_Foldable_C-40VectC-32C-36nC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5) ((DataC-45Vect-foldr_Foldable_C-40VectC-32C-36nC-41 'erased 'erased 'erased (lambda (eta-0) (lambda (eta-1) (PreludeC-45Basics-flip 'erased 'erased 'erased (lambda (eta-2) (lambda (eta-3) (lambda (eta-4) (eta-2 (eta-3 eta-4))))) (lambda (eta-2) (PreludeC-45Basics-flip 'erased 'erased 'erased arg-3 eta-0 eta-2)) eta-1))) (lambda (eta-0) eta-0) arg-5) arg-4)))
(define DataC-45Vect-compare_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (let ((sc0 arg-3)) (case (vector-ref sc0 0) ((0) (let ((sc1 arg-4)) 1)) (else (let ((e-3 (vector-ref sc0 1))) (let ((e-4 (vector-ref sc0 2))) (let ((sc1 arg-4)) (let ((e-8 (vector-ref sc1 1))) (let ((e-9 (vector-ref sc1 2))) (DataC-45Vect-case--compare-1861 'erased arg-2 'erased e-3 e-4 e-8 e-9 'erased (let ((sc2 arg-2)) (let ((e-2 (vector-ref sc2 2))) ((e-2 e-3) e-8))))))))))))))
(define DataC-45Vect-__Impl_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 (lambda (arg-0 arg-1 arg-2) (vector 0 (vector 0 (lambda (arg-3) (lambda (arg-4) (DataC-45Vect-C-61C-61_Eq_C-40C-40VectC-32C-36nC-41C-32C-36aC-41 'erased 'erased (let ((sc0 arg-2)) (let ((e-1 (vector-ref sc0 1))) e-1)) arg-3 arg-4))) (lambda (arg-4) (lambda (arg-5) (DataC-45Vect-C-47C-61_Eq_C-40C-40VectC-32C-36nC-41C-32C-36aC-41 'erased 'erased (let ((sc0 arg-2)) (let ((e-1 (vector-ref sc0 1))) e-1)) arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (DataC-45Vect-compare_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased arg-2 arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (DataC-45Vect-C-60_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased arg-2 arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (DataC-45Vect-C-62_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased arg-2 arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (DataC-45Vect-C-60C-61_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased arg-2 arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (DataC-45Vect-C-62C-61_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased arg-2 arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (DataC-45Vect-max_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased arg-2 arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (DataC-45Vect-min_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased arg-2 arg-383 arg-384))))))
(define DataC-45Vect-__Impl_Functor_C-40VectC-32C-36nC-41 (lambda (arg-0 ext-0 ext-1 ext-2 ext-3) (DataC-45Vect-map_Functor_C-40VectC-32C-36nC-41 'erased 'erased 'erased ext-2 ext-3)))
(define DataC-45Vect-__Impl_Foldable_C-40VectC-32C-36nC-41 (lambda (arg-0) (vector 0 (lambda (acc) (lambda (elem) (lambda (func) (lambda (init) (lambda (input) (DataC-45Vect-foldr_Foldable_C-40VectC-32C-36nC-41 'erased 'erased 'erased func init input)))))) (lambda (elem) (lambda (acc) (lambda (func) (lambda (init) (lambda (input) (DataC-45Vect-foldl_Foldable_C-40VectC-32C-36nC-41 'erased 'erased 'erased func init input)))))) (lambda (elem) (lambda (arg-1123) (DataC-45Vect-null_Foldable_C-40VectC-32C-36nC-41 'erased 'erased arg-1123))))))
(define DataC-45Vect-__Impl_Eq_C-40C-40VectC-32C-36nC-41C-32C-36aC-41 (lambda (arg-0 arg-1 arg-2) (vector 0 (lambda (arg-3) (lambda (arg-4) (DataC-45Vect-C-61C-61_Eq_C-40C-40VectC-32C-36nC-41C-32C-36aC-41 'erased 'erased arg-2 arg-3 arg-4))) (lambda (arg-4) (lambda (arg-5) (DataC-45Vect-C-47C-61_Eq_C-40C-40VectC-32C-36nC-41C-32C-36aC-41 'erased 'erased arg-2 arg-4 arg-5))))))
(define DataC-45Vect-C-62_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (PreludeC-45EqOrd-C-61C-61_Eq_Ordering (DataC-45Vect-compare_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased arg-2 arg-3 arg-4) 2)))
(define DataC-45Vect-C-62C-61_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (PreludeC-45EqOrd-C-47C-61_Eq_Ordering (DataC-45Vect-compare_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased arg-2 arg-3 arg-4) 0)))
(define DataC-45Vect-C-61C-61_Eq_C-40C-40VectC-32C-36nC-41C-32C-36aC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (let ((sc0 arg-3)) (case (vector-ref sc0 0) ((0) (let ((sc1 arg-4)) 0)) (else (let ((e-3 (vector-ref sc0 1))) (let ((e-4 (vector-ref sc0 2))) (let ((sc1 arg-4)) (let ((e-8 (vector-ref sc1 1))) (let ((e-9 (vector-ref sc1 2))) (PreludeC-45Basics-C-38C-38 (let ((sc2 arg-2)) (let ((e-1 (vector-ref sc2 1))) ((e-1 e-3) e-8))) (lambda () (DataC-45Vect-C-61C-61_Eq_C-40C-40VectC-32C-36nC-41C-32C-36aC-41 'erased 'erased arg-2 e-4 e-9)))))))))))))
(define DataC-45Vect-C-60_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (PreludeC-45EqOrd-C-61C-61_Eq_Ordering (DataC-45Vect-compare_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased arg-2 arg-3 arg-4) 0)))
(define DataC-45Vect-C-60C-61_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (PreludeC-45EqOrd-C-47C-61_Eq_Ordering (DataC-45Vect-compare_Ord_C-40C-40VectC-32C-36lenC-41C-32C-36elemC-41 'erased 'erased arg-2 arg-3 arg-4) 2)))
(define DataC-45Vect-C-47C-61_Eq_C-40C-40VectC-32C-36nC-41C-32C-36aC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (PreludeC-45Basics-not (DataC-45Vect-C-61C-61_Eq_C-40C-40VectC-32C-36nC-41C-32C-36aC-41 'erased 'erased arg-2 arg-3 arg-4))))
(define DataC-45Vect-head (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (let ((e-2 (vector-ref sc0 1))) e-2))))
(define DataC-45Vect-foldrImpl (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6) (let ((sc0 arg-6)) (case (vector-ref sc0 0) ((0) (arg-5 arg-4)) (else (let ((e-3 (vector-ref sc0 1))) (let ((e-4 (vector-ref sc0 2))) (DataC-45Vect-foldrImpl 'erased 'erased 'erased arg-3 arg-4 (lambda (eta-0) (arg-5 ((arg-3 e-3) eta-0))) e-4))))))))
(define DataC-45Vect-filter (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 arg-3)) (case (vector-ref sc0 0) ((0) (vector 0 0 (vector 0 ))) (else (let ((e-3 (vector-ref sc0 1))) (let ((e-4 (vector-ref sc0 2))) (DataC-45Vect-case--filter-3030 'erased 'erased e-3 e-4 arg-2 'erased (DataC-45Vect-filter 'erased 'erased arg-2 e-4)))))))))
(define PreludeC-45Basics-uncurry (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (let ((sc0 arg-4)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) ((arg-3 e-2) e-3))))))
(define PreludeC-45Basics-not (lambda (arg-0) (let ((sc0 arg-0)) (cond ((equal? sc0 0) 1) (else 0)))))
(define PreludeC-45Basics-intToBool (lambda (arg-0) (let ((sc0 arg-0)) (cond ((equal? sc0 0) 1)(else 0)))))
(define PreludeC-45Basics-id (lambda (arg-0 arg-1) arg-1))
(define PreludeC-45Basics-flip (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5) ((arg-3 arg-5) arg-4)))
(define PreludeC-45Basics-C-46 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 ext-0) (arg-3 (arg-4 ext-0))))
(define PreludeC-45Basics-C-38C-38 (lambda (arg-0 arg-1) (let ((sc0 arg-0)) (cond ((equal? sc0 0) (arg-1)) (else 1)))))
(define BuiltinC-45DPairC-45DPair-snd (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (let ((e-3 (vector-ref sc0 2))) e-3))))
(define Builtin-snd (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (let ((e-3 (vector-ref sc0 2))) e-3))))
(define Builtin-fst (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (let ((e-2 (vector-ref sc0 1))) e-2))))
(define Builtin-believe_me (lambda (arg-0 arg-1 ext-0) ext-0))
(define PreludeC-45Types-case--prim__integerToNat-340 (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (cond ((equal? sc0 0) (Builtin-believe_me 'erased 'erased arg-0)) (else 0)))))
(define PreludeC-45Types-null_Foldable_List (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (case (vector-ref sc0 0) ((0) (lambda () 0)) (else (lambda () 1))))))
(define PreludeC-45Types-map_Functor_List (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 arg-3)) (case (vector-ref sc0 0) ((0) (vector 0 )) (else (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (vector 1 (arg-2 e-2) (PreludeC-45Types-map_Functor_List 'erased 'erased arg-2 e-3)))))))))
(define PreludeC-45Types-foldr_Foldable_List (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (let ((sc0 arg-4)) (case (vector-ref sc0 0) ((0) arg-3) (else (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) ((arg-2 e-2) (PreludeC-45Types-foldr_Foldable_List 'erased 'erased arg-2 arg-3 e-3)))))))))
(define PreludeC-45Types-foldl_Foldable_List (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (let ((sc0 arg-4)) (case (vector-ref sc0 0) ((0) arg-3) (else (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (PreludeC-45Types-foldl_Foldable_List 'erased 'erased arg-2 ((arg-2 arg-3) e-2) e-3))))))))
(define PreludeC-45Types-__Impl_Functor_List (lambda (ext-0 ext-1 ext-2 ext-3) (PreludeC-45Types-map_Functor_List 'erased 'erased ext-2 ext-3)))
(define PreludeC-45Types-__Impl_Foldable_List (lambda () (vector 0 (lambda (acc) (lambda (elem) (lambda (func) (lambda (init) (lambda (input) (PreludeC-45Types-foldr_Foldable_List 'erased 'erased func init input)))))) (lambda (elem) (lambda (acc) (lambda (func) (lambda (init) (lambda (input) (PreludeC-45Types-foldl_Foldable_List 'erased 'erased func init input)))))) (lambda (elem) (lambda (arg-1123) (PreludeC-45Types-null_Foldable_List 'erased arg-1123))))))
(define PreludeC-45Types-prim__integerToNat (lambda (arg-0) (PreludeC-45Types-case--prim__integerToNat-340 arg-0 (let ((sc0 (or (and (<= 0 arg-0) 1) 0))) (cond ((equal? sc0 0) 1)(else 0))))))
(define PreludeC-45Num-fromInteger_Num_Double (lambda (ext-0) (exact->inexact ext-0)))
(define PreludeC-45Num-C-45_Neg_Integer (lambda (ext-0 ext-1) (- ext-0 ext-1)))
(define PreludeC-45Num-C-43_Num_Integer (lambda (ext-0 ext-1) (+ ext-0 ext-1)))
(define PreludeC-45Num-C-42_Num_Integer (lambda (ext-0 ext-1) (* ext-0 ext-1)))
(define PreludeC-45EqOrd-case--caseC-32blockC-32inC-32compare-1879 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) 1) (else 2)))))
(define PreludeC-45EqOrd-case--compare-1862 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) 0) (else (PreludeC-45EqOrd-case--caseC-32blockC-32inC-32compare-1879 arg-0 arg-1 (PreludeC-45EqOrd-C-61C-61_Eq_Double arg-1 arg-0)))))))
(define PreludeC-45EqOrd-case--max-1845 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) arg-1) (else arg-0)))))
(define PreludeC-45EqOrd-case--min-1831 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) arg-1) (else arg-0)))))
(define PreludeC-45EqOrd-case--caseC-32blockC-32inC-32compare-1309 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) 1) (else 2)))))
(define PreludeC-45EqOrd-case--compare-1292 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) 0) (else (PreludeC-45EqOrd-case--caseC-32blockC-32inC-32compare-1309 arg-0 arg-1 (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-1 arg-0)))))))
(define PreludeC-45EqOrd-case--max-1275 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) arg-1) (else arg-0)))))
(define PreludeC-45EqOrd-case--min-1261 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) arg-1) (else arg-0)))))
(define PreludeC-45EqOrd-min_Ord_Integer (lambda (arg-0 arg-1) (PreludeC-45EqOrd-case--min-1261 arg-1 arg-0 (PreludeC-45EqOrd-C-60_Ord_Integer arg-0 arg-1))))
(define PreludeC-45EqOrd-min_Ord_Double (lambda (arg-0 arg-1) (PreludeC-45EqOrd-case--min-1831 arg-1 arg-0 (PreludeC-45EqOrd-C-60_Ord_Double arg-0 arg-1))))
(define PreludeC-45EqOrd-max_Ord_Integer (lambda (arg-0 arg-1) (PreludeC-45EqOrd-case--max-1275 arg-1 arg-0 (PreludeC-45EqOrd-C-62_Ord_Integer arg-0 arg-1))))
(define PreludeC-45EqOrd-max_Ord_Double (lambda (arg-0 arg-1) (PreludeC-45EqOrd-case--max-1845 arg-1 arg-0 (PreludeC-45EqOrd-C-62_Ord_Double arg-0 arg-1))))
(define PreludeC-45EqOrd-compare_Ord_Integer (lambda (arg-0 arg-1) (PreludeC-45EqOrd-case--compare-1292 arg-1 arg-0 (PreludeC-45EqOrd-C-60_Ord_Integer arg-0 arg-1))))
(define PreludeC-45EqOrd-compare_Ord_Double (lambda (arg-0 arg-1) (PreludeC-45EqOrd-case--compare-1862 arg-1 arg-0 (PreludeC-45EqOrd-C-60_Ord_Double arg-0 arg-1))))
(define PreludeC-45EqOrd-__Ord_C-40EqC-32tyC-41 (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-1 (vector-ref sc0 1))) e-1))))
(define PreludeC-45EqOrd-__Impl_Ord_Integer (lambda () (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Integer arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Integer arg-383 arg-384))))))
(define PreludeC-45EqOrd-__Impl_Ord_Double (lambda () (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Double arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Double arg-4 arg-5)))) (lambda (arg-371) (lambda (arg-372) (PreludeC-45EqOrd-compare_Ord_Double arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (PreludeC-45EqOrd-C-60_Ord_Double arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (PreludeC-45EqOrd-C-62_Ord_Double arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (PreludeC-45EqOrd-C-60C-61_Ord_Double arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (PreludeC-45EqOrd-C-62C-61_Ord_Double arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (PreludeC-45EqOrd-max_Ord_Double arg-381 arg-382))) (lambda (arg-383) (lambda (arg-384) (PreludeC-45EqOrd-min_Ord_Double arg-383 arg-384))))))
(define PreludeC-45EqOrd-__Impl_Eq_Ordering (lambda () (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Ordering arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Ordering arg-4 arg-5))))))
(define PreludeC-45EqOrd-__Impl_Eq_Integer (lambda () (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Integer arg-4 arg-5))))))
(define PreludeC-45EqOrd-__Impl_Eq_Double (lambda () (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45EqOrd-C-61C-61_Eq_Double arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (PreludeC-45EqOrd-C-47C-61_Eq_Double arg-4 arg-5))))))
(define PreludeC-45EqOrd-C-62_Ord_Integer (lambda (arg-0 arg-1) (let ((sc0 (or (and (> arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define PreludeC-45EqOrd-C-62_Ord_Double (lambda (arg-0 arg-1) (let ((sc0 (or (and (> arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define PreludeC-45EqOrd-C-62C-61_Ord_Integer (lambda (arg-0 arg-1) (let ((sc0 (or (and (>= arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define PreludeC-45EqOrd-C-62C-61_Ord_Double (lambda (arg-0 arg-1) (let ((sc0 (or (and (>= arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define PreludeC-45EqOrd-C-61C-61_Eq_Ordering (lambda (arg-0 arg-1) (let ((sc0 arg-0)) (cond ((equal? sc0 0) (let ((sc1 arg-1)) (cond ((equal? sc1 0) 0)(else 1)))) ((equal? sc0 1) (let ((sc1 arg-1)) (cond ((equal? sc1 1) 0)(else 1)))) ((equal? sc0 2) (let ((sc1 arg-1)) (cond ((equal? sc1 2) 0)(else 1))))(else 1)))))
(define PreludeC-45EqOrd-C-61C-61_Eq_Integer (lambda (arg-0 arg-1) (let ((sc0 (or (and (= arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define PreludeC-45EqOrd-C-61C-61_Eq_Double (lambda (arg-0 arg-1) (let ((sc0 (or (and (= arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define PreludeC-45EqOrd-C-60_Ord_Integer (lambda (arg-0 arg-1) (let ((sc0 (or (and (< arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define PreludeC-45EqOrd-C-60_Ord_Double (lambda (arg-0 arg-1) (let ((sc0 (or (and (< arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define PreludeC-45EqOrd-C-60C-61_Ord_Integer (lambda (arg-0 arg-1) (let ((sc0 (or (and (<= arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define PreludeC-45EqOrd-C-60C-61_Ord_Double (lambda (arg-0 arg-1) (let ((sc0 (or (and (<= arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define PreludeC-45EqOrd-C-47C-61_Eq_Ordering (lambda (arg-0 arg-1) (PreludeC-45Basics-not (PreludeC-45EqOrd-C-61C-61_Eq_Ordering arg-0 arg-1))))
(define PreludeC-45EqOrd-C-47C-61_Eq_Integer (lambda (arg-0 arg-1) (PreludeC-45Basics-not (PreludeC-45EqOrd-C-61C-61_Eq_Integer arg-0 arg-1))))
(define PreludeC-45EqOrd-C-47C-61_Eq_Double (lambda (arg-0 arg-1) (PreludeC-45Basics-not (PreludeC-45EqOrd-C-61C-61_Eq_Double arg-0 arg-1))))
(define PreludeC-45EqOrd-compare (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-2 (vector-ref sc0 2))) (lambda (arg-2) (lambda (arg-3) ((e-2 arg-2) arg-3)))))))
(define PreludeC-45EqOrd-C-62 (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-4 (vector-ref sc0 4))) (lambda (arg-2) (lambda (arg-3) ((e-4 arg-2) arg-3)))))))
(define PreludeC-45EqOrd-C-61C-61 (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-1 (vector-ref sc0 1))) (lambda (arg-2) (lambda (arg-3) ((e-1 arg-2) arg-3)))))))
(define PreludeC-45EqOrd-C-60C-61 (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-5 (vector-ref sc0 5))) (lambda (arg-2) (lambda (arg-3) ((e-5 arg-2) arg-3)))))))
(define PreludeC-45EqOrd-C-60 (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-3 (vector-ref sc0 3))) (lambda (arg-2) (lambda (arg-3) ((e-3 arg-2) arg-3)))))))
(define PreludeC-45Interfaces-neutral_Monoid_C-40C-124UnitC-44MkUnitC-124C-41 (lambda () (vector 0 )))
(define PreludeC-45Interfaces-__Monad_C-40ApplicativeC-32mC-41 (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-1 (vector-ref sc0 1))) e-1))))
(define PreludeC-45Interfaces-__Impl_Semigroup_C-40C-124UnitC-44MkUnitC-124C-41 (lambda (ext-0 ext-1) (PreludeC-45Interfaces-C-60C-43C-62_Semigroup_C-40C-124UnitC-44MkUnitC-124C-41 ext-0 ext-1)))
(define PreludeC-45Interfaces-__Impl_Monoid_C-40C-124UnitC-44MkUnitC-124C-41 (lambda () (vector 0 (lambda (arg-2) (lambda (arg-3) (PreludeC-45Interfaces-C-60C-43C-62_Semigroup_C-40C-124UnitC-44MkUnitC-124C-41 arg-2 arg-3))) (PreludeC-45Interfaces-neutral_Monoid_C-40C-124UnitC-44MkUnitC-124C-41))))
(define PreludeC-45Interfaces-__Applicative_C-40FunctorC-32fC-41 (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-1 (vector-ref sc0 1))) e-1))))
(define PreludeC-45Interfaces-C-60C-43C-62_Semigroup_C-40C-124UnitC-44MkUnitC-124C-41 (lambda (arg-0 arg-1) (vector 0 )))
(define PreludeC-45Interfaces-pure (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (let ((e-2 (vector-ref sc0 2))) (lambda (arg-3) ((e-2 'erased) arg-3))))))
(define PreludeC-45Interfaces-neutral (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-2 (vector-ref sc0 2))) e-2))))
(define PreludeC-45Interfaces-map (lambda (arg-0 arg-1 arg-2 arg-3 ext-0 ext-1) ((((arg-3 'erased) 'erased) ext-0) ext-1)))
(define PreludeC-45Interfaces-foldr (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 arg-3)) (let ((e-1 (vector-ref sc0 1))) (lambda (arg-4) (lambda (arg-5) (lambda (arg-6) (((((e-1 'erased) 'erased) arg-4) arg-5) arg-6))))))))
(define PreludeC-45Interfaces-foldl (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 arg-3)) (let ((e-2 (vector-ref sc0 2))) (lambda (arg-4) (lambda (arg-5) (lambda (arg-6) (((((e-2 'erased) 'erased) arg-4) arg-5) arg-6))))))))
(define PreludeC-45Interfaces-all (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 arg-2)) (let ((e-2 (vector-ref sc0 2))) (lambda (arg-4) (((((e-2 'erased) 'erased) (lambda (x) (lambda (y) (PreludeC-45Basics-C-38C-38 x (lambda () (arg-3 y)))))) 0) arg-4))))))
(define PreludeC-45Interfaces-C-62C-62C-61 (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 arg-3)) (let ((e-2 (vector-ref sc0 2))) (lambda (arg-4) (lambda (arg-5) ((((e-2 'erased) 'erased) arg-4) arg-5)))))))
(define PreludeC-45Interfaces-C-62C-62 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (let ((sc0 arg-2)) (let ((e-2 (vector-ref sc0 2))) ((((e-2 'erased) 'erased) arg-3) (lambda (_-995) (arg-4)))))))
(define PreludeC-45Interfaces-C-60C-36C-62 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5) ((((arg-3 'erased) 'erased) arg-4) arg-5)))
(define PrimIO-case--unsafePerformIO-532 (lambda (arg-0 arg-1 arg-2 arg-3) (PrimIO-unsafeDestroyWorld 'erased 'erased arg-3)))
(define PrimIO-case--caseC-32blockC-32inC-32io_bind-453 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7) (arg-7 arg-6)))
(define PrimIO-case--io_bind-431 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5) (PrimIO-case--caseC-32blockC-32inC-32io_bind-453 'erased 'erased 'erased 'erased 'erased arg-5 'erased (arg-3 arg-5))))
(define PrimIO-unsafePerformIO (lambda (arg-0 arg-1) (PrimIO-unsafeCreateWorld 'erased (lambda (w) (PrimIO-case--unsafePerformIO-532 'erased arg-1 'erased (arg-1 w))))))
(define PrimIO-unsafeDestroyWorld (lambda (arg-0 arg-1 arg-2) arg-2))
(define PrimIO-unsafeCreateWorld (lambda (arg-0 arg-1) (arg-1 #f)))
(define PrimIO-io_pure (lambda (arg-0 arg-1 ext-0) arg-1))
(define PrimIO-io_bind (lambda (arg-0 arg-1 arg-2 arg-3 ext-0) (PrimIO-case--io_bind-431 'erased 'erased 'erased arg-3 'erased (arg-2 ext-0))))
(define PreludeC-45IO-pure_Applicative_IO (lambda (arg-0 arg-1 ext-0) arg-1))
(define PreludeC-45IO-map_Functor_IO (lambda (arg-0 arg-1 arg-2 arg-3 ext-0) (let ((act-3 (arg-3 ext-0))) (arg-2 act-3))))
(define PreludeC-45IO-join_Monad_IO (lambda (arg-0 arg-1 ext-0) (let ((act-2 (arg-1 ext-0))) (act-2 ext-0))))
(define PreludeC-45IO-__Impl_Monad_IO (lambda () (vector 0 (vector 0 (lambda (b) (lambda (a) (lambda (func) (lambda (arg-143) (lambda (eta-0) (PreludeC-45IO-map_Functor_IO 'erased 'erased func arg-143 eta-0)))))) (lambda (a) (lambda (arg-556) (lambda (eta-0) arg-556))) (lambda (b) (lambda (a) (lambda (arg-557) (lambda (arg-559) (lambda (eta-0) (let ((act-17 (arg-557 eta-0))) (let ((act-16 (arg-559 eta-0))) (act-17 act-16))))))))) (lambda (b) (lambda (a) (lambda (arg-855) (lambda (arg-856) (lambda (eta-0) (let ((act-24 (arg-855 eta-0))) ((arg-856 act-24) eta-0))))))) (lambda (a) (lambda (arg-858) (lambda (eta-0) (let ((act-29 (arg-858 eta-0))) (act-29 eta-0))))))))
(define PreludeC-45IO-__Impl_Functor_IO (lambda (ext-4 ext-1 ext-2 ext-3 ext-0) (PreludeC-45IO-map_Functor_IO 'erased 'erased ext-2 ext-3 ext-0)))
(define PreludeC-45IO-__Impl_Applicative_IO (lambda () (vector 0 (lambda (b) (lambda (a) (lambda (func) (lambda (arg-143) (lambda (eta-0) (PreludeC-45IO-map_Functor_IO 'erased 'erased func arg-143 eta-0)))))) (lambda (a) (lambda (arg-556) (lambda (eta-0) arg-556))) (lambda (b) (lambda (a) (lambda (arg-557) (lambda (arg-559) (lambda (eta-0) (let ((act-17 (arg-557 eta-0))) (let ((act-16 (arg-559 eta-0))) (act-17 act-16)))))))))))
(define PreludeC-45IO-C-62C-62C-61_Monad_IO (lambda (arg-0 arg-1 arg-2 arg-3 ext-0) (let ((act-1 (arg-2 ext-0))) ((arg-3 act-1) ext-0))))
(define PreludeC-45IO-C-60C-42C-62_Applicative_IO (lambda (arg-0 arg-1 arg-2 arg-3 ext-0) (let ((act-6 (arg-2 ext-0))) (let ((act-5 (arg-3 ext-0))) (act-6 act-5)))))
(define DataC-45Maybe-isJust (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (case (vector-ref sc0 0) ((0) 1) (else 0)))))
(define DataC-45SortedSet-insert (lambda (arg-0 arg-1 arg-2) (DataC-45SortedMap-insert 'erased 'erased arg-1 (vector 0 ) arg-2)))
(define DataC-45SortedSet-fromList (lambda (arg-0 arg-1 arg-2) (DataC-45SortedMap-fromList 'erased 'erased arg-1 (PreludeC-45Types-map_Functor_List 'erased 'erased (lambda (i) (vector 0 i (vector 0 ))) arg-2))))
(define DataC-45SortedSet-delete (lambda (arg-0 arg-1 arg-2) (DataC-45SortedMap-delete 'erased 'erased arg-1 arg-2)))
(define DataC-45SortedSet-contains (lambda (arg-0 arg-1 arg-2) (DataC-45Maybe-isJust 'erased (DataC-45SortedMap-lookup 'erased 'erased arg-1 arg-2))))
(define DataC-45SortedMap-case--delete-4237 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6) (let ((sc0 arg-6)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 1 arg-2 (+ 1 arg-4) e-2))) (else (let ((e-5 (vector-ref sc0 1))) (vector 1 arg-2 arg-4 e-5)))))))
(define DataC-45SortedMap-case--delete-4175 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5) (let ((sc0 arg-5)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 1 arg-2 0 e-2))) (else (let ((e-5 (vector-ref sc0 1))) (let ((sc1 e-5)) (vector 0 arg-2))))))))
(define DataC-45SortedMap-case--insert-4023 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7) (let ((sc0 arg-7)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 1 arg-2 arg-3 e-2))) (else (let ((e-5 (vector-ref sc0 1))) (vector 1 arg-2 (+ 1 arg-3) e-5)))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32caseC-32blockC-32inC-32treeDelete-3765 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11) (let ((sc0 arg-11)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 (vector 2 arg-7 arg-9 arg-6 arg-10 e-2)))) (else (let ((e-5 (vector-ref sc0 1))) (vector 0 (DataC-45SortedMap-merge3 'erased 'erased 'erased 'erased arg-7 arg-9 arg-6 arg-10 e-5))))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32caseC-32blockC-32inC-32treeDelete-3648 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11) (let ((sc0 arg-11)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 (vector 2 arg-7 arg-9 e-2 arg-10 arg-5)))) (else (let ((e-5 (vector-ref sc0 1))) (vector 0 (DataC-45SortedMap-merge2 'erased 'erased 'erased 'erased arg-7 arg-9 e-5 arg-10 arg-5))))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32treeDelete-3609 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11) (let ((sc0 arg-11)) (cond ((equal? sc0 0) (DataC-45SortedMap-case--caseC-32blockC-32inC-32caseC-32blockC-32inC-32treeDelete-3648 'erased 'erased 'erased arg-3 arg-4 arg-5 arg-6 arg-7 arg-10 arg-9 arg-8 (DataC-45SortedMap-treeDelete 'erased 'erased 'erased arg-3 (+ 1 arg-4) arg-10 arg-6))) (else (DataC-45SortedMap-case--caseC-32blockC-32inC-32caseC-32blockC-32inC-32treeDelete-3765 'erased 'erased 'erased arg-3 arg-4 arg-5 arg-6 arg-7 arg-10 arg-9 arg-8 (DataC-45SortedMap-treeDelete 'erased 'erased 'erased arg-3 (+ 1 arg-4) arg-10 arg-5)))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32treeDelete-3495 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11) (let ((sc0 arg-11)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 (vector 2 e-2 arg-9 arg-6 arg-8 arg-5)))) (else (let ((e-5 (vector-ref sc0 1))) (vector 0 (DataC-45SortedMap-merge1 'erased 'erased 'erased 'erased e-5 arg-9 arg-6 arg-8 arg-5))))))))
(define DataC-45SortedMap-case--treeDelete-3456 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11) (let ((sc0 arg-11)) (cond ((equal? sc0 0) (DataC-45SortedMap-case--caseC-32blockC-32inC-32treeDelete-3495 'erased 'erased 'erased arg-3 arg-4 arg-8 arg-9 arg-10 arg-7 arg-6 arg-5 (DataC-45SortedMap-treeDelete 'erased 'erased 'erased arg-3 (+ 1 arg-4) arg-5 arg-10))) (else (DataC-45SortedMap-case--caseC-32blockC-32inC-32treeDelete-3609 'erased 'erased 'erased arg-3 arg-4 arg-8 arg-9 arg-10 arg-7 arg-6 arg-5 (let ((sc1 arg-3)) (let ((e-5 (vector-ref sc1 5))) ((e-5 arg-5) arg-7)))))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32caseC-32blockC-32inC-32treeDelete-3356 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9) (let ((sc0 arg-6)) (case (vector-ref sc0 0) ((1) (let ((e-4 (vector-ref sc0 1))) (let ((e-5 (vector-ref sc0 2))) (let ((e-6 (vector-ref sc0 3))) (vector 1 (vector 2 e-4 e-5 e-6 arg-8 arg-9)))))) (else (let ((e-11 (vector-ref sc0 1))) (let ((e-12 (vector-ref sc0 2))) (let ((e-13 (vector-ref sc0 3))) (let ((e-14 (vector-ref sc0 4))) (let ((e-15 (vector-ref sc0 5))) (vector 0 (DataC-45SortedMap-branch4 'erased 'erased 'erased 'erased e-11 e-12 e-13 e-14 e-15 arg-8 arg-9))))))))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32treeDelete-3290 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9) (let ((sc0 arg-9)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 (vector 1 arg-6 arg-7 e-2)))) (else (let ((e-5 (vector-ref sc0 1))) (let ((sc1 arg-6)) (case (vector-ref sc1 0) ((1) (let ((e-4 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (let ((e-6 (vector-ref sc1 3))) (vector 1 (vector 2 e-4 e-7 e-6 arg-7 e-5)))))) (else (let ((e-11 (vector-ref sc1 1))) (let ((e-12 (vector-ref sc1 2))) (let ((e-13 (vector-ref sc1 3))) (let ((e-14 (vector-ref sc1 4))) (let ((e-15 (vector-ref sc1 5))) (vector 0 (DataC-45SortedMap-branch4 'erased 'erased 'erased 'erased e-11 e-12 e-13 e-14 e-15 arg-7 e-5))))))))))))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32caseC-32blockC-32inC-32treeDelete-3185 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9) (let ((sc0 arg-5)) (case (vector-ref sc0 0) ((1) (let ((e-4 (vector-ref sc0 1))) (let ((e-5 (vector-ref sc0 2))) (let ((e-6 (vector-ref sc0 3))) (vector 1 (vector 2 arg-9 arg-8 e-4 e-5 e-6)))))) (else (let ((e-11 (vector-ref sc0 1))) (let ((e-12 (vector-ref sc0 2))) (let ((e-13 (vector-ref sc0 3))) (let ((e-14 (vector-ref sc0 4))) (let ((e-15 (vector-ref sc0 5))) (vector 0 (DataC-45SortedMap-branch4 'erased 'erased 'erased 'erased arg-9 arg-8 e-11 e-12 e-13 e-14 e-15))))))))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32treeDelete-3119 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9) (let ((sc0 arg-9)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 (vector 1 e-2 arg-7 arg-5)))) (else (let ((e-5 (vector-ref sc0 1))) (let ((sc1 arg-5)) (case (vector-ref sc1 0) ((1) (let ((e-4 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (let ((e-6 (vector-ref sc1 3))) (vector 1 (vector 2 e-5 arg-7 e-4 e-7 e-6)))))) (else (let ((e-11 (vector-ref sc1 1))) (let ((e-12 (vector-ref sc1 2))) (let ((e-13 (vector-ref sc1 3))) (let ((e-14 (vector-ref sc1 4))) (let ((e-15 (vector-ref sc1 5))) (vector 0 (DataC-45SortedMap-branch4 'erased 'erased 'erased 'erased e-5 arg-7 e-11 e-12 e-13 e-14 e-15))))))))))))))))
(define DataC-45SortedMap-case--treeDelete-3086 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9) (let ((sc0 arg-9)) (cond ((equal? sc0 0) (DataC-45SortedMap-case--caseC-32blockC-32inC-32treeDelete-3119 'erased 'erased 'erased arg-3 arg-4 arg-7 arg-8 arg-6 arg-5 (DataC-45SortedMap-treeDelete 'erased 'erased 'erased arg-3 (+ 1 arg-4) arg-5 arg-8))) (else (DataC-45SortedMap-case--caseC-32blockC-32inC-32treeDelete-3290 'erased 'erased 'erased arg-3 arg-4 arg-7 arg-8 arg-6 arg-5 (DataC-45SortedMap-treeDelete 'erased 'erased 'erased arg-3 (+ 1 arg-4) arg-5 arg-7)))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32caseC-32blockC-32inC-32treeDelete-2986 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10) (let ((sc0 arg-10)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 (vector 2 arg-6 arg-8 arg-5 arg-9 e-2)))) (else (let ((e-5 (vector-ref sc0 1))) (let ((sc1 e-5)) (vector 0 (vector 1 arg-6 arg-8 arg-5)))))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32caseC-32blockC-32inC-32treeDelete-2875 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10) (let ((sc0 arg-10)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 (vector 2 arg-6 arg-8 e-2 arg-9 arg-4)))) (else (let ((e-5 (vector-ref sc0 1))) (let ((sc1 e-5)) (vector 0 (vector 1 arg-6 arg-8 arg-4)))))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32treeDelete-2838 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10) (let ((sc0 arg-10)) (cond ((equal? sc0 0) (DataC-45SortedMap-case--caseC-32blockC-32inC-32caseC-32blockC-32inC-32treeDelete-2875 'erased 'erased 'erased arg-3 arg-4 arg-5 arg-6 arg-9 arg-8 arg-7 (DataC-45SortedMap-treeDelete 'erased 'erased 'erased arg-3 0 arg-9 arg-5))) (else (DataC-45SortedMap-case--caseC-32blockC-32inC-32caseC-32blockC-32inC-32treeDelete-2986 'erased 'erased 'erased arg-3 arg-4 arg-5 arg-6 arg-9 arg-8 arg-7 (DataC-45SortedMap-treeDelete 'erased 'erased 'erased arg-3 0 arg-9 arg-4)))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32treeDelete-2730 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10) (let ((sc0 arg-10)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 (vector 2 e-2 arg-8 arg-5 arg-7 arg-4)))) (else (let ((e-5 (vector-ref sc0 1))) (let ((sc1 e-5)) (vector 0 (vector 1 arg-5 arg-7 arg-4)))))))))
(define DataC-45SortedMap-case--treeDelete-2693 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10) (let ((sc0 arg-10)) (cond ((equal? sc0 0) (DataC-45SortedMap-case--caseC-32blockC-32inC-32treeDelete-2730 'erased 'erased 'erased arg-3 arg-7 arg-8 arg-9 arg-6 arg-5 arg-4 (DataC-45SortedMap-treeDelete 'erased 'erased 'erased arg-3 0 arg-4 arg-9))) (else (DataC-45SortedMap-case--caseC-32blockC-32inC-32treeDelete-2838 'erased 'erased 'erased arg-3 arg-7 arg-8 arg-9 arg-6 arg-5 arg-4 (let ((sc1 arg-3)) (let ((e-5 (vector-ref sc1 5))) ((e-5 arg-4) arg-6)))))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32treeDelete-2607 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8) (let ((sc0 arg-8)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 (vector 1 arg-5 arg-6 e-2)))) (else (let ((e-5 (vector-ref sc0 1))) (let ((sc1 e-5)) (vector 1 arg-5))))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32treeDelete-2518 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8) (let ((sc0 arg-8)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 (vector 1 e-2 arg-6 arg-4)))) (else (let ((e-5 (vector-ref sc0 1))) (let ((sc1 e-5)) (vector 1 arg-4))))))))
(define DataC-45SortedMap-case--treeDelete-2487 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8) (let ((sc0 arg-8)) (cond ((equal? sc0 0) (DataC-45SortedMap-case--caseC-32blockC-32inC-32treeDelete-2518 'erased 'erased 'erased arg-3 arg-6 arg-7 arg-5 arg-4 (DataC-45SortedMap-treeDelete 'erased 'erased 'erased arg-3 0 arg-4 arg-7))) (else (DataC-45SortedMap-case--caseC-32blockC-32inC-32treeDelete-2607 'erased 'erased 'erased arg-3 arg-6 arg-7 arg-5 arg-4 (DataC-45SortedMap-treeDelete 'erased 'erased 'erased arg-3 0 arg-4 arg-6)))))))
(define DataC-45SortedMap-case--treeDelete-2408 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7) (let ((sc0 arg-7)) (cond ((equal? sc0 0) (vector 1 (vector 0 ))) (else (vector 0 (vector 0 arg-5 arg-6)))))))
(define DataC-45SortedMap-case--treeInsert-2305 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8) (let ((sc0 arg-8)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 e-2))) (else (let ((e-5 (vector-ref sc0 1))) (let ((sc1 e-5)) (let ((e-8 (vector-ref sc1 1))) (let ((e-9 (vector-ref sc1 2))) (let ((sc2 e-9)) (let ((e-12 (vector-ref sc2 1))) (let ((e-13 (vector-ref sc2 2))) (vector 1 (vector 1 e-8 e-12 e-13))))))))))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32caseC-32blockC-32inC-32treeInsertC-39-2164 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11 arg-12 arg-13) (let ((sc0 arg-13)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 (vector 2 arg-7 arg-11 arg-6 arg-12 e-2)))) (else (let ((e-5 (vector-ref sc0 1))) (let ((sc1 e-5)) (let ((e-8 (vector-ref sc1 1))) (let ((e-9 (vector-ref sc1 2))) (let ((sc2 e-9)) (let ((e-12 (vector-ref sc2 1))) (let ((e-13 (vector-ref sc2 2))) (vector 1 (vector 0 (vector 1 arg-7 arg-11 arg-6) (vector 0 arg-12 (vector 1 e-8 e-12 e-13))))))))))))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32caseC-32blockC-32inC-32treeInsertC-39-2006 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11 arg-12 arg-13) (let ((sc0 arg-13)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 (vector 2 arg-7 arg-11 e-2 arg-12 arg-5)))) (else (let ((e-5 (vector-ref sc0 1))) (let ((sc1 e-5)) (let ((e-8 (vector-ref sc1 1))) (let ((e-9 (vector-ref sc1 2))) (let ((sc2 e-9)) (let ((e-12 (vector-ref sc2 1))) (let ((e-13 (vector-ref sc2 2))) (vector 1 (vector 0 (vector 1 arg-7 arg-11 e-8) (vector 0 e-12 (vector 1 e-13 arg-12 arg-5))))))))))))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32treeInsertC-39-1959 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11 arg-12 arg-13) (let ((sc0 arg-13)) (cond ((equal? sc0 0) (DataC-45SortedMap-case--caseC-32blockC-32inC-32caseC-32blockC-32inC-32treeInsertC-39-2006 'erased 'erased 'erased arg-3 'erased arg-5 arg-6 arg-7 'erased arg-12 arg-11 arg-10 arg-9 (DataC-45SortedMap-treeInsertC-39 'erased 'erased 'erased 'erased arg-3 arg-12 arg-11 arg-6))) (else (DataC-45SortedMap-case--caseC-32blockC-32inC-32caseC-32blockC-32inC-32treeInsertC-39-2164 'erased 'erased 'erased arg-3 'erased arg-5 arg-6 arg-7 'erased arg-12 arg-11 arg-10 arg-9 (DataC-45SortedMap-treeInsertC-39 'erased 'erased 'erased 'erased arg-3 arg-12 arg-11 arg-5)))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32treeInsertC-39-1806 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11 arg-12 arg-13) (let ((sc0 arg-13)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 (vector 2 e-2 arg-10 arg-6 arg-9 arg-5)))) (else (let ((e-5 (vector-ref sc0 1))) (let ((sc1 e-5)) (let ((e-8 (vector-ref sc1 1))) (let ((e-9 (vector-ref sc1 2))) (let ((sc2 e-9)) (let ((e-12 (vector-ref sc2 1))) (let ((e-13 (vector-ref sc2 2))) (vector 1 (vector 0 (vector 1 e-8 e-12 e-13) (vector 0 arg-10 (vector 1 arg-6 arg-9 arg-5))))))))))))))))
(define DataC-45SortedMap-case--treeInsertC-39-1759 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11 arg-12 arg-13) (let ((sc0 arg-13)) (cond ((equal? sc0 0) (DataC-45SortedMap-case--caseC-32blockC-32inC-32treeInsertC-39-1806 'erased 'erased 'erased arg-3 'erased arg-9 arg-10 arg-11 'erased arg-8 arg-7 arg-6 arg-5 (DataC-45SortedMap-treeInsertC-39 'erased 'erased 'erased 'erased arg-3 arg-5 arg-6 arg-11))) (else (DataC-45SortedMap-case--caseC-32blockC-32inC-32treeInsertC-39-1959 'erased 'erased 'erased arg-3 'erased arg-9 arg-10 arg-11 'erased arg-8 arg-7 arg-6 arg-5 (let ((sc1 arg-3)) (let ((e-5 (vector-ref sc1 5))) ((e-5 arg-5) arg-8)))))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32treeInsertC-39-1642 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11) (let ((sc0 arg-11)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 (vector 1 arg-6 arg-8 e-2)))) (else (let ((e-5 (vector-ref sc0 1))) (let ((sc1 e-5)) (let ((e-8 (vector-ref sc1 1))) (let ((e-9 (vector-ref sc1 2))) (let ((sc2 e-9)) (let ((e-12 (vector-ref sc2 1))) (let ((e-13 (vector-ref sc2 2))) (vector 0 (vector 2 arg-6 arg-8 e-8 e-12 e-13))))))))))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32treeInsertC-39-1514 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11) (let ((sc0 arg-11)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 (vector 1 e-2 arg-8 arg-5)))) (else (let ((e-5 (vector-ref sc0 1))) (let ((sc1 e-5)) (let ((e-8 (vector-ref sc1 1))) (let ((e-9 (vector-ref sc1 2))) (let ((sc2 e-9)) (let ((e-12 (vector-ref sc2 1))) (let ((e-13 (vector-ref sc2 2))) (vector 0 (vector 2 e-8 e-12 e-13 arg-8 arg-5))))))))))))))
(define DataC-45SortedMap-case--treeInsertC-39-1473 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11) (let ((sc0 arg-11)) (cond ((equal? sc0 0) (DataC-45SortedMap-case--caseC-32blockC-32inC-32treeInsertC-39-1514 'erased 'erased 'erased arg-3 'erased arg-8 arg-9 'erased arg-7 arg-6 arg-5 (DataC-45SortedMap-treeInsertC-39 'erased 'erased 'erased 'erased arg-3 arg-5 arg-6 arg-9))) (else (DataC-45SortedMap-case--caseC-32blockC-32inC-32treeInsertC-39-1642 'erased 'erased 'erased arg-3 'erased arg-8 arg-9 'erased arg-7 arg-6 arg-5 (DataC-45SortedMap-treeInsertC-39 'erased 'erased 'erased 'erased arg-3 arg-5 arg-6 arg-8)))))))
(define DataC-45SortedMap-case--treeInsertC-39-1314 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9) (let ((sc0 arg-9)) (cond ((equal? sc0 0) (vector 1 (vector 0 (vector 0 arg-4 arg-5) (vector 0 arg-4 (vector 0 arg-6 arg-7))))) ((equal? sc0 1) (vector 0 (vector 0 arg-4 arg-5))) (else (vector 1 (vector 0 (vector 0 arg-6 arg-7) (vector 0 arg-6 (vector 0 arg-4 arg-5)))))))))
(define DataC-45SortedMap-case--caseC-32blockC-32inC-32treeLookup-1208 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11 arg-12) (let ((sc0 arg-12)) (cond ((equal? sc0 0) (DataC-45SortedMap-treeLookup 'erased 'erased 'erased 'erased arg-3 arg-11 arg-6)) (else (DataC-45SortedMap-treeLookup 'erased 'erased 'erased 'erased arg-3 arg-11 arg-5))))))
(define DataC-45SortedMap-case--treeLookup-1133 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11 arg-12) (let ((sc0 arg-12)) (cond ((equal? sc0 0) (DataC-45SortedMap-treeLookup 'erased 'erased 'erased 'erased arg-3 arg-5 arg-10)) (else (DataC-45SortedMap-case--caseC-32blockC-32inC-32treeLookup-1208 'erased 'erased 'erased arg-3 'erased arg-8 arg-9 arg-10 'erased arg-7 arg-6 arg-5 (let ((sc1 arg-3)) (let ((e-5 (vector-ref sc1 5))) ((e-5 arg-5) arg-7)))))))))
(define DataC-45SortedMap-case--treeLookup-1043 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10) (let ((sc0 arg-10)) (cond ((equal? sc0 0) (DataC-45SortedMap-treeLookup 'erased 'erased 'erased 'erased arg-3 arg-5 arg-8)) (else (DataC-45SortedMap-treeLookup 'erased 'erased 'erased 'erased arg-3 arg-5 arg-7))))))
(define DataC-45SortedMap-case--treeLookup-968 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8) (let ((sc0 arg-8)) (cond ((equal? sc0 0) (vector 1 arg-6)) (else (vector 0 ))))))
(define DataC-45SortedMap-treeLookup (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6) (let ((sc0 arg-6)) (case (vector-ref sc0 0) ((0) (let ((e-3 (vector-ref sc0 1))) (let ((e-4 (vector-ref sc0 2))) (DataC-45SortedMap-case--treeLookup-968 'erased 'erased 'erased arg-4 arg-5 e-3 e-4 'erased (let ((sc1 (let ((sc2 arg-4)) (let ((e-1 (vector-ref sc2 1))) e-1)))) (let ((e-1 (vector-ref sc1 1))) ((e-1 arg-5) e-3))))))) ((1) (let ((e-9 (vector-ref sc0 1))) (let ((e-10 (vector-ref sc0 2))) (let ((e-11 (vector-ref sc0 3))) (DataC-45SortedMap-case--treeLookup-1043 'erased 'erased 'erased arg-4 'erased arg-5 e-10 e-11 e-9 'erased (let ((sc1 arg-4)) (let ((e-5 (vector-ref sc1 5))) ((e-5 arg-5) e-10)))))))) (else (let ((e-16 (vector-ref sc0 1))) (let ((e-17 (vector-ref sc0 2))) (let ((e-18 (vector-ref sc0 3))) (let ((e-19 (vector-ref sc0 4))) (let ((e-20 (vector-ref sc0 5))) (DataC-45SortedMap-case--treeLookup-1133 'erased 'erased 'erased arg-4 'erased arg-5 e-17 e-19 e-20 e-18 e-16 'erased (let ((sc1 arg-4)) (let ((e-5 (vector-ref sc1 5))) ((e-5 arg-5) e-17))))))))))))))
(define DataC-45SortedMap-treeInsertC-39 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7) (let ((sc0 arg-7)) (case (vector-ref sc0 0) ((0) (let ((e-3 (vector-ref sc0 1))) (let ((e-4 (vector-ref sc0 2))) (DataC-45SortedMap-case--treeInsertC-39-1314 'erased 'erased 'erased arg-4 arg-5 arg-6 e-3 e-4 'erased (let ((sc1 arg-4)) (let ((e-2 (vector-ref sc1 2))) ((e-2 arg-5) e-3))))))) ((1) (let ((e-9 (vector-ref sc0 1))) (let ((e-10 (vector-ref sc0 2))) (let ((e-11 (vector-ref sc0 3))) (DataC-45SortedMap-case--treeInsertC-39-1473 'erased 'erased 'erased arg-4 'erased arg-5 arg-6 e-10 e-11 e-9 'erased (let ((sc1 arg-4)) (let ((e-5 (vector-ref sc1 5))) ((e-5 arg-5) e-10)))))))) (else (let ((e-16 (vector-ref sc0 1))) (let ((e-17 (vector-ref sc0 2))) (let ((e-18 (vector-ref sc0 3))) (let ((e-19 (vector-ref sc0 4))) (let ((e-20 (vector-ref sc0 5))) (DataC-45SortedMap-case--treeInsertC-39-1759 'erased 'erased 'erased arg-4 'erased arg-5 arg-6 e-17 e-19 e-20 e-18 e-16 'erased (let ((sc1 arg-4)) (let ((e-5 (vector-ref sc1 5))) ((e-5 arg-5) e-17))))))))))))))
(define DataC-45SortedMap-treeInsert (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7) (DataC-45SortedMap-case--treeInsert-2305 'erased 'erased 'erased 'erased arg-4 arg-5 arg-6 arg-7 (DataC-45SortedMap-treeInsertC-39 'erased 'erased 'erased 'erased arg-4 arg-5 arg-6 arg-7))))
(define DataC-45SortedMap-treeDelete (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6) (let ((sc0 arg-4)) (cond ((equal? sc0 0) (let ((sc1 arg-6)) (let ((e-4 (vector-ref sc1 1))) (let ((e-5 (vector-ref sc1 2))) (DataC-45SortedMap-case--treeDelete-2408 'erased 'erased 'erased arg-3 arg-5 e-4 e-5 (let ((sc2 (let ((sc3 arg-3)) (let ((e-1 (vector-ref sc3 1))) e-1)))) (let ((e-1 (vector-ref sc2 1))) ((e-1 arg-5) e-4))))))))(else (let ((e-0 (- arg-4 1))) (let ((sc0 e-0)) (cond ((equal? sc0 0) (let ((sc1 arg-6)) (case (vector-ref sc1 0) ((1) (let ((e-11 (vector-ref sc1 1))) (let ((e-12 (vector-ref sc1 2))) (let ((e-13 (vector-ref sc1 3))) (DataC-45SortedMap-case--treeDelete-2487 'erased 'erased 'erased arg-3 arg-5 e-12 e-13 e-11 (let ((sc2 arg-3)) (let ((e-5 (vector-ref sc2 5))) ((e-5 arg-5) e-12)))))))) (else (let ((e-18 (vector-ref sc1 1))) (let ((e-19 (vector-ref sc1 2))) (let ((e-20 (vector-ref sc1 3))) (let ((e-21 (vector-ref sc1 4))) (let ((e-22 (vector-ref sc1 5))) (DataC-45SortedMap-case--treeDelete-2693 'erased 'erased 'erased arg-3 arg-5 e-19 e-21 e-22 e-20 e-18 (let ((sc2 arg-3)) (let ((e-5 (vector-ref sc2 5))) ((e-5 arg-5) e-19)))))))))))))(else (let ((e-6 (- e-0 1))) (let ((sc0 arg-6)) (case (vector-ref sc0 0) ((1) (let ((e-27 (vector-ref sc0 1))) (let ((e-28 (vector-ref sc0 2))) (let ((e-29 (vector-ref sc0 3))) (DataC-45SortedMap-case--treeDelete-3086 'erased 'erased 'erased arg-3 e-6 arg-5 e-28 e-29 e-27 (let ((sc1 arg-3)) (let ((e-5 (vector-ref sc1 5))) ((e-5 arg-5) e-28)))))))) (else (let ((e-34 (vector-ref sc0 1))) (let ((e-35 (vector-ref sc0 2))) (let ((e-36 (vector-ref sc0 3))) (let ((e-37 (vector-ref sc0 4))) (let ((e-38 (vector-ref sc0 5))) (DataC-45SortedMap-case--treeDelete-3456 'erased 'erased 'erased arg-3 e-6 arg-5 e-35 e-37 e-38 e-36 e-34 (let ((sc1 arg-3)) (let ((e-5 (vector-ref sc1 5))) ((e-5 arg-5) e-35))))))))))))))))))))))
(define DataC-45SortedMap-merge3 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8) (let ((sc0 arg-4)) (case (vector-ref sc0 0) ((1) (let ((e-4 (vector-ref sc0 1))) (let ((e-5 (vector-ref sc0 2))) (let ((e-6 (vector-ref sc0 3))) (let ((sc1 arg-6)) (case (vector-ref sc1 0) ((1) (let ((e-20 (vector-ref sc1 1))) (let ((e-21 (vector-ref sc1 2))) (let ((e-22 (vector-ref sc1 3))) (DataC-45SortedMap-branch5 'erased 'erased 'erased 'erased e-4 e-5 e-6 arg-5 e-20 e-21 e-22 arg-7 arg-8))))) (else (let ((e-27 (vector-ref sc1 1))) (let ((e-28 (vector-ref sc1 2))) (let ((e-29 (vector-ref sc1 3))) (let ((e-30 (vector-ref sc1 4))) (let ((e-31 (vector-ref sc1 5))) (DataC-45SortedMap-branch6 'erased 'erased 'erased 'erased e-4 e-5 e-6 arg-5 e-27 e-28 e-29 e-30 e-31 arg-7 arg-8))))))))))))) (else (let ((e-11 (vector-ref sc0 1))) (let ((e-12 (vector-ref sc0 2))) (let ((e-13 (vector-ref sc0 3))) (let ((e-14 (vector-ref sc0 4))) (let ((e-15 (vector-ref sc0 5))) (let ((sc1 arg-6)) (case (vector-ref sc1 0) ((1) (let ((e-36 (vector-ref sc1 1))) (let ((e-37 (vector-ref sc1 2))) (let ((e-38 (vector-ref sc1 3))) (DataC-45SortedMap-branch6 'erased 'erased 'erased 'erased e-11 e-12 e-13 e-14 e-15 arg-5 e-36 e-37 e-38 arg-7 arg-8))))) (else (let ((e-43 (vector-ref sc1 1))) (let ((e-44 (vector-ref sc1 2))) (let ((e-45 (vector-ref sc1 3))) (let ((e-46 (vector-ref sc1 4))) (let ((e-47 (vector-ref sc1 5))) (DataC-45SortedMap-branch7 'erased 'erased 'erased 'erased e-11 e-12 e-13 e-14 e-15 arg-5 e-43 e-44 e-45 e-46 e-47 arg-7 arg-8)))))))))))))))))))
(define DataC-45SortedMap-merge2 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8) (let ((sc0 arg-4)) (case (vector-ref sc0 0) ((1) (let ((e-4 (vector-ref sc0 1))) (let ((e-5 (vector-ref sc0 2))) (let ((e-6 (vector-ref sc0 3))) (let ((sc1 arg-8)) (case (vector-ref sc1 0) ((1) (let ((e-20 (vector-ref sc1 1))) (let ((e-21 (vector-ref sc1 2))) (let ((e-22 (vector-ref sc1 3))) (DataC-45SortedMap-branch5 'erased 'erased 'erased 'erased e-4 e-5 e-6 arg-5 arg-6 arg-7 e-20 e-21 e-22))))) (else (let ((e-27 (vector-ref sc1 1))) (let ((e-28 (vector-ref sc1 2))) (let ((e-29 (vector-ref sc1 3))) (let ((e-30 (vector-ref sc1 4))) (let ((e-31 (vector-ref sc1 5))) (DataC-45SortedMap-branch6 'erased 'erased 'erased 'erased e-4 e-5 e-6 arg-5 arg-6 arg-7 e-27 e-28 e-29 e-30 e-31))))))))))))) (else (let ((e-11 (vector-ref sc0 1))) (let ((e-12 (vector-ref sc0 2))) (let ((e-13 (vector-ref sc0 3))) (let ((e-14 (vector-ref sc0 4))) (let ((e-15 (vector-ref sc0 5))) (let ((sc1 arg-8)) (case (vector-ref sc1 0) ((1) (let ((e-36 (vector-ref sc1 1))) (let ((e-37 (vector-ref sc1 2))) (let ((e-38 (vector-ref sc1 3))) (DataC-45SortedMap-branch6 'erased 'erased 'erased 'erased e-11 e-12 e-13 e-14 e-15 arg-5 arg-6 arg-7 e-36 e-37 e-38))))) (else (let ((e-43 (vector-ref sc1 1))) (let ((e-44 (vector-ref sc1 2))) (let ((e-45 (vector-ref sc1 3))) (let ((e-46 (vector-ref sc1 4))) (let ((e-47 (vector-ref sc1 5))) (DataC-45SortedMap-branch7 'erased 'erased 'erased 'erased e-11 e-12 e-13 e-14 e-15 arg-5 arg-6 arg-7 e-43 e-44 e-45 e-46 e-47)))))))))))))))))))
(define DataC-45SortedMap-merge1 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8) (let ((sc0 arg-6)) (case (vector-ref sc0 0) ((1) (let ((e-4 (vector-ref sc0 1))) (let ((e-5 (vector-ref sc0 2))) (let ((e-6 (vector-ref sc0 3))) (let ((sc1 arg-8)) (case (vector-ref sc1 0) ((1) (let ((e-20 (vector-ref sc1 1))) (let ((e-21 (vector-ref sc1 2))) (let ((e-22 (vector-ref sc1 3))) (DataC-45SortedMap-branch5 'erased 'erased 'erased 'erased arg-4 arg-5 e-4 e-5 e-6 arg-7 e-20 e-21 e-22))))) (else (let ((e-27 (vector-ref sc1 1))) (let ((e-28 (vector-ref sc1 2))) (let ((e-29 (vector-ref sc1 3))) (let ((e-30 (vector-ref sc1 4))) (let ((e-31 (vector-ref sc1 5))) (DataC-45SortedMap-branch6 'erased 'erased 'erased 'erased arg-4 arg-5 e-4 e-5 e-6 arg-7 e-27 e-28 e-29 e-30 e-31))))))))))))) (else (let ((e-11 (vector-ref sc0 1))) (let ((e-12 (vector-ref sc0 2))) (let ((e-13 (vector-ref sc0 3))) (let ((e-14 (vector-ref sc0 4))) (let ((e-15 (vector-ref sc0 5))) (let ((sc1 arg-8)) (case (vector-ref sc1 0) ((1) (let ((e-36 (vector-ref sc1 1))) (let ((e-37 (vector-ref sc1 2))) (let ((e-38 (vector-ref sc1 3))) (DataC-45SortedMap-branch6 'erased 'erased 'erased 'erased arg-4 arg-5 e-11 e-12 e-13 e-14 e-15 arg-7 e-36 e-37 e-38))))) (else (let ((e-43 (vector-ref sc1 1))) (let ((e-44 (vector-ref sc1 2))) (let ((e-45 (vector-ref sc1 3))) (let ((e-46 (vector-ref sc1 4))) (let ((e-47 (vector-ref sc1 5))) (DataC-45SortedMap-branch7 'erased 'erased 'erased 'erased arg-4 arg-5 e-11 e-12 e-13 e-14 e-15 arg-7 e-43 e-44 e-45 e-46 e-47)))))))))))))))))))
(define DataC-45SortedMap-lookup (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 arg-3)) (case (vector-ref sc0 0) ((0) (vector 0 )) (else (let ((e-5 (vector-ref sc0 1))) (let ((e-7 (vector-ref sc0 3))) (DataC-45SortedMap-treeLookup 'erased 'erased 'erased 'erased e-5 arg-2 e-7))))))))
(define DataC-45SortedMap-insert (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (let ((sc0 arg-4)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 1 e-2 0 (vector 0 arg-2 arg-3)))) (else (let ((e-5 (vector-ref sc0 1))) (let ((e-6 (vector-ref sc0 2))) (let ((e-7 (vector-ref sc0 3))) (DataC-45SortedMap-case--insert-4023 'erased 'erased e-5 e-6 arg-2 arg-3 e-7 (DataC-45SortedMap-treeInsert 'erased 'erased 'erased 'erased e-5 arg-2 arg-3 e-7))))))))))
(define DataC-45SortedMap-fromList (lambda (arg-0 arg-1 arg-2 arg-3) (PreludeC-45Types-foldl_Foldable_List 'erased 'erased (lambda (eta-0) (lambda (eta-1) (PreludeC-45Basics-flip 'erased 'erased 'erased (lambda (eta-2) (PreludeC-45Basics-uncurry 'erased 'erased 'erased (lambda (eta-3) (lambda (eta-4) (lambda (eta-5) (DataC-45SortedMap-insert 'erased 'erased eta-3 eta-4 eta-5)))) eta-2)) eta-0 eta-1))) (DataC-45SortedMap-empty 'erased 'erased arg-2) arg-3)))
(define DataC-45SortedMap-empty (lambda (arg-0 arg-1 arg-2) (vector 0 arg-2)))
(define DataC-45SortedMap-delete (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 arg-3)) (case (vector-ref sc0 0) ((0) (let ((e-2 (vector-ref sc0 1))) (vector 0 e-2))) (else (let ((e-5 (vector-ref sc0 1))) (let ((e-6 (vector-ref sc0 2))) (let ((e-7 (vector-ref sc0 3))) (let ((sc1 e-6)) (cond ((equal? sc1 0) (DataC-45SortedMap-case--delete-4175 'erased 'erased e-5 arg-2 e-7 (DataC-45SortedMap-treeDelete 'erased 'erased 'erased e-5 0 arg-2 e-7)))(else (let ((e-8 (- e-6 1))) (DataC-45SortedMap-case--delete-4237 'erased 'erased e-5 arg-2 e-8 e-7 (DataC-45SortedMap-treeDelete 'erased 'erased 'erased e-5 (+ 1 e-8) arg-2 e-7))))))))))))))
(define DataC-45SortedMap-branch7 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11 arg-12 arg-13 arg-14 arg-15 arg-16) (vector 2 (vector 2 arg-4 arg-5 arg-6 arg-7 arg-8) arg-9 (vector 1 arg-10 arg-11 arg-12) arg-13 (vector 1 arg-14 arg-15 arg-16))))
(define DataC-45SortedMap-branch6 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11 arg-12 arg-13 arg-14) (vector 2 (vector 1 arg-4 arg-5 arg-6) arg-7 (vector 1 arg-8 arg-9 arg-10) arg-11 (vector 1 arg-12 arg-13 arg-14))))
(define DataC-45SortedMap-branch5 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11 arg-12) (vector 1 (vector 1 arg-4 arg-5 arg-6) arg-7 (vector 2 arg-8 arg-9 arg-10 arg-11 arg-12))))
(define DataC-45SortedMap-branch4 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10) (vector 1 (vector 1 arg-4 arg-5 arg-6) arg-7 (vector 1 arg-8 arg-9 arg-10))))
(define ControlC-45MonadC-45StateC-45Interface-case--state-2160 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10) (let ((sc0 arg-10)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 (let ((sc2 arg-5)) (let ((e-1 (vector-ref sc2 1))) e-1)))) (let ((e-5 (vector-ref sc1 2))) ((e-5 'erased) (vector 0 e-3 (vector 0 e-2 arg-9))))))))))
(define ControlC-45MonadC-45StateC-45Interface-state_MonadState_C-36s_C-40C-40C-40C-40RWSTC-32C-36rC-41C-32C-36wC-41C-32C-36sC-41C-32C-36mC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 ext-0 ext-1 ext-2) (ControlC-45MonadC-45StateC-45Interface-case--state-2160 'erased 'erased 'erased 'erased 'erased arg-5 arg-6 ext-0 ext-1 ext-2 (arg-6 ext-1))))
(define ControlC-45MonadC-45StateC-45Interface-put_MonadState_C-36s_C-40C-40C-40C-40RWSTC-32C-36rC-41C-32C-36wC-41C-32C-36sC-41C-32C-36mC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 ext-0 ext-1 ext-2) (let ((sc0 (let ((sc1 arg-4)) (let ((e-1 (vector-ref sc1 1))) e-1)))) (let ((e-2 (vector-ref sc0 2))) ((e-2 'erased) (vector 0 (vector 0 ) (vector 0 arg-5 ext-2)))))))
(define ControlC-45MonadC-45StateC-45Interface-get_MonadState_C-36s_C-40C-40C-40C-40RWSTC-32C-36rC-41C-32C-36wC-41C-32C-36sC-41C-32C-36mC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 ext-0 ext-1 ext-2) (let ((sc0 (let ((sc1 arg-4)) (let ((e-1 (vector-ref sc1 1))) e-1)))) (let ((e-2 (vector-ref sc0 2))) ((e-2 'erased) (vector 0 ext-1 (vector 0 ext-1 ext-2)))))))
(define ControlC-45MonadC-45StateC-45Interface-__MonadState_C-40MonadC-32mC-41 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (let ((e-2 (vector-ref sc0 1))) e-2))))
(define ControlC-45MonadC-45StateC-45Interface-__Impl_MonadState_C-36s_C-40C-40C-40C-40RWSTC-32C-36rC-41C-32C-36wC-41C-32C-36sC-41C-32C-36mC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (vector 0 (vector 0 (vector 0 (lambda (b) (lambda (a) (lambda (func) (lambda (arg-143) (lambda (r) (lambda (s) (lambda (w) (PreludeC-45Interfaces-C-60C-36C-62 'erased 'erased 'erased (let ((sc0 (let ((sc1 arg-4)) (let ((e-1 (vector-ref sc1 1))) e-1)))) (let ((e-1 (vector-ref sc0 1))) e-1)) (lambda (lamc-0) (let ((sc0 lamc-0)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (vector 0 (func e-2) (vector 0 e-6 e-7))))))))) (((arg-143 r) s) w))))))))) (lambda (a) (lambda (arg-556) (lambda (_-1415) (lambda (s) (lambda (w) (let ((sc0 (let ((sc1 arg-4)) (let ((e-1 (vector-ref sc1 1))) e-1)))) (let ((e-2 (vector-ref sc0 2))) ((e-2 'erased) (vector 0 arg-556 (vector 0 s w)))))))))) (lambda (b) (lambda (a) (lambda (arg-557) (lambda (arg-559) (lambda (r) (lambda (s) (lambda (w) (let ((sc0 arg-4)) (let ((e-2 (vector-ref sc0 2))) ((((e-2 'erased) 'erased) (((arg-557 r) s) w)) (lambda (_-0) (let ((sc1 _-0)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (let ((sc3 arg-4)) (let ((e-9 (vector-ref sc3 2))) ((((e-9 'erased) 'erased) (((arg-559 r) e-6) e-7)) (lambda (_-1) (let ((sc4 _-1)) (let ((e-12 (vector-ref sc4 1))) (let ((e-11 (vector-ref sc4 2))) (let ((sc5 e-11)) (let ((e-14 (vector-ref sc5 1))) (let ((e-13 (vector-ref sc5 2))) (let ((sc6 (let ((sc7 arg-4)) (let ((e-17 (vector-ref sc7 1))) e-17)))) (let ((e-16 (vector-ref sc6 2))) ((e-16 'erased) (vector 0 (e-5 e-12) (vector 0 e-14 e-13))))))))))))))))))))))))))))))))) (lambda (b) (lambda (a) (lambda (arg-855) (lambda (arg-856) (lambda (r) (lambda (s) (lambda (w) (let ((sc0 arg-4)) (let ((e-2 (vector-ref sc0 2))) ((((e-2 'erased) 'erased) (((arg-855 r) s) w)) (lambda (_-0) (let ((sc1 _-0)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) ((((arg-856 e-5) r) e-6) e-7)))))))))))))))))) (lambda (a) (lambda (arg-858) (lambda (r) (lambda (s) (lambda (w) (let ((sc0 arg-4)) (let ((e-2 (vector-ref sc0 2))) ((((e-2 'erased) 'erased) (((arg-858 r) s) w)) (lambda (_-0) (let ((sc1 _-0)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (((e-5 r) e-6) e-7))))))))))))))))) (lambda (_-2064) (lambda (s) (lambda (w) (let ((sc0 (let ((sc1 arg-4)) (let ((e-1 (vector-ref sc1 1))) e-1)))) (let ((e-2 (vector-ref sc0 2))) ((e-2 'erased) (vector 0 s (vector 0 s w)))))))) (lambda (arg-1404) (lambda (_-2106) (lambda (_-2108) (lambda (w) (let ((sc0 (let ((sc1 arg-4)) (let ((e-1 (vector-ref sc1 1))) e-1)))) (let ((e-2 (vector-ref sc0 2))) ((e-2 'erased) (vector 0 (vector 0 ) (vector 0 arg-1404 w))))))))) (lambda (a) (lambda (arg-1405) (lambda (_-2153) (lambda (s) (lambda (w) (ControlC-45MonadC-45StateC-45Interface-case--state-2160 'erased 'erased 'erased 'erased 'erased arg-4 arg-1405 _-2153 s w (arg-1405 s))))))))))
(define ControlC-45MonadC-45StateC-45Interface-put (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (let ((e-4 (vector-ref sc0 3))) (lambda (arg-3) (e-4 arg-3))))))
(define ControlC-45MonadC-45StateC-45Interface-modify (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 (let ((sc1 arg-2)) (let ((e-2 (vector-ref sc1 1))) e-2)))) (let ((e-2 (vector-ref sc0 2))) ((((e-2 'erased) 'erased) (let ((sc1 arg-2)) (let ((e-6 (vector-ref sc1 2))) e-6))) (lambda (s) (let ((sc1 arg-2)) (let ((e-4 (vector-ref sc1 3))) (e-4 (arg-3 s))))))))))
(define ControlC-45MonadC-45StateC-45Interface-get (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (let ((e-3 (vector-ref sc0 2))) e-3))))
(define ControlC-45MonadC-45RWSC-45CPS-case--C-62C-62C-61-1894 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11 arg-12) (let ((sc0 arg-12)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) ((((arg-7 e-2) arg-9) e-6) e-7)))))))))
(define ControlC-45MonadC-45RWSC-45CPS-case--caseC-32blockC-32inC-32C-60C-42C-62-1557 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11 arg-12 arg-13 arg-14 arg-15 arg-16) (let ((sc0 arg-16)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (let ((sc2 (let ((sc3 arg-6)) (let ((e-1 (vector-ref sc3 1))) e-1)))) (let ((e-5 (vector-ref sc2 2))) ((e-5 'erased) (vector 0 (arg-12 e-2) (vector 0 e-6 e-7)))))))))))))
(define ControlC-45MonadC-45RWSC-45CPS-case--C-60C-42C-62-1496 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11 arg-12) (let ((sc0 arg-12)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (let ((sc2 arg-6)) (let ((e-5 (vector-ref sc2 2))) ((((e-5 'erased) 'erased) (((arg-8 arg-9) e-6) e-7)) (lambda (_-0) (let ((sc3 _-0)) (let ((e-9 (vector-ref sc3 1))) (let ((e-8 (vector-ref sc3 2))) (let ((sc4 e-8)) (let ((e-11 (vector-ref sc4 1))) (let ((e-10 (vector-ref sc4 2))) (let ((sc5 (let ((sc6 arg-6)) (let ((e-14 (vector-ref sc6 1))) e-14)))) (let ((e-13 (vector-ref sc5 2))) ((e-13 'erased) (vector 0 (e-2 e-9) (vector 0 e-11 e-10)))))))))))))))))))))))
(define ControlC-45MonadC-45RWSC-45CPS-case--map-1284 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9 arg-10 arg-11 arg-12) (let ((sc0 arg-12)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (vector 0 (arg-8 e-2) (vector 0 e-6 e-7))))))))))
(define ControlC-45MonadC-45RWSC-45CPS-case--execRWST-683 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9) (let ((sc0 arg-9)) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (vector 0 e-6 e-7))))))))
(define ControlC-45MonadC-45RWSC-45CPS-pure_Applicative_C-40C-40C-40C-40RWSTC-32C-36rC-41C-32C-36wC-41C-32C-36sC-41C-32C-36mC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 ext-0 ext-1 ext-2) (let ((sc0 (let ((sc1 arg-5)) (let ((e-1 (vector-ref sc1 1))) e-1)))) (let ((e-2 (vector-ref sc0 2))) ((e-2 'erased) (vector 0 arg-6 (vector 0 ext-1 ext-2)))))))
(define ControlC-45MonadC-45RWSC-45CPS-map_Functor_C-40C-40C-40C-40RWSTC-32C-36rC-41C-32C-36wC-41C-32C-36sC-41C-32C-36mC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 ext-0 ext-1 ext-2) (PreludeC-45Interfaces-C-60C-36C-62 'erased 'erased 'erased arg-6 (lambda (lamc-0) (let ((sc0 lamc-0)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (vector 0 (arg-7 e-2) (vector 0 e-6 e-7))))))))) (((arg-8 ext-0) ext-1) ext-2))))
(define ControlC-45MonadC-45RWSC-45CPS-join_Monad_C-40C-40C-40C-40RWSTC-32C-36rC-41C-32C-36wC-41C-32C-36sC-41C-32C-36mC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 ext-0 ext-1 ext-2) (let ((sc0 arg-5)) (let ((e-2 (vector-ref sc0 2))) ((((e-2 'erased) 'erased) (((arg-6 ext-0) ext-1) ext-2)) (lambda (_-0) (let ((sc1 _-0)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (((e-5 ext-0) e-6) e-7)))))))))))))
(define ControlC-45MonadC-45RWSC-45CPS-__Impl_Monad_C-40C-40C-40C-40RWSTC-32C-36rC-41C-32C-36wC-41C-32C-36sC-41C-32C-36mC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (vector 0 (vector 0 (lambda (b) (lambda (a) (lambda (func) (lambda (arg-143) (lambda (r) (lambda (s) (lambda (w) (PreludeC-45Interfaces-C-60C-36C-62 'erased 'erased 'erased (let ((sc0 (let ((sc1 arg-4)) (let ((e-1 (vector-ref sc1 1))) e-1)))) (let ((e-1 (vector-ref sc0 1))) e-1)) (lambda (lamc-0) (let ((sc0 lamc-0)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (vector 0 (func e-2) (vector 0 e-6 e-7))))))))) (((arg-143 r) s) w))))))))) (lambda (a) (lambda (arg-556) (lambda (_-1415) (lambda (s) (lambda (w) (let ((sc0 (let ((sc1 arg-4)) (let ((e-1 (vector-ref sc1 1))) e-1)))) (let ((e-2 (vector-ref sc0 2))) ((e-2 'erased) (vector 0 arg-556 (vector 0 s w)))))))))) (lambda (b) (lambda (a) (lambda (arg-557) (lambda (arg-559) (lambda (r) (lambda (s) (lambda (w) (let ((sc0 arg-4)) (let ((e-2 (vector-ref sc0 2))) ((((e-2 'erased) 'erased) (((arg-557 r) s) w)) (lambda (_-0) (let ((sc1 _-0)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (let ((sc3 arg-4)) (let ((e-9 (vector-ref sc3 2))) ((((e-9 'erased) 'erased) (((arg-559 r) e-6) e-7)) (lambda (_-1) (let ((sc4 _-1)) (let ((e-12 (vector-ref sc4 1))) (let ((e-11 (vector-ref sc4 2))) (let ((sc5 e-11)) (let ((e-14 (vector-ref sc5 1))) (let ((e-13 (vector-ref sc5 2))) (let ((sc6 (let ((sc7 arg-4)) (let ((e-17 (vector-ref sc7 1))) e-17)))) (let ((e-16 (vector-ref sc6 2))) ((e-16 'erased) (vector 0 (e-5 e-12) (vector 0 e-14 e-13))))))))))))))))))))))))))))))))) (lambda (b) (lambda (a) (lambda (arg-855) (lambda (arg-856) (lambda (r) (lambda (s) (lambda (w) (let ((sc0 arg-4)) (let ((e-2 (vector-ref sc0 2))) ((((e-2 'erased) 'erased) (((arg-855 r) s) w)) (lambda (_-0) (let ((sc1 _-0)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) ((((arg-856 e-5) r) e-6) e-7)))))))))))))))))) (lambda (a) (lambda (arg-858) (lambda (r) (lambda (s) (lambda (w) (let ((sc0 arg-4)) (let ((e-2 (vector-ref sc0 2))) ((((e-2 'erased) 'erased) (((arg-858 r) s) w)) (lambda (_-0) (let ((sc1 _-0)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (((e-5 r) e-6) e-7)))))))))))))))))))
(define ControlC-45MonadC-45RWSC-45CPS-__Impl_Functor_C-40C-40C-40C-40RWSTC-32C-36rC-41C-32C-36wC-41C-32C-36sC-41C-32C-36mC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 ext-0 ext-1 ext-2 ext-3 ext-4 ext-5 ext-6) (PreludeC-45Interfaces-C-60C-36C-62 'erased 'erased 'erased arg-4 (lambda (lamc-0) (let ((sc0 lamc-0)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (vector 0 (ext-2 e-2) (vector 0 e-6 e-7))))))))) (((ext-3 ext-4) ext-5) ext-6))))
(define ControlC-45MonadC-45RWSC-45CPS-__Impl_Applicative_C-40C-40C-40C-40RWSTC-32C-36rC-41C-32C-36wC-41C-32C-36sC-41C-32C-36mC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (vector 0 (lambda (b) (lambda (a) (lambda (func) (lambda (arg-143) (lambda (r) (lambda (s) (lambda (w) (PreludeC-45Interfaces-C-60C-36C-62 'erased 'erased 'erased (let ((sc0 (let ((sc1 arg-4)) (let ((e-1 (vector-ref sc1 1))) e-1)))) (let ((e-1 (vector-ref sc0 1))) e-1)) (lambda (lamc-0) (let ((sc0 lamc-0)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (vector 0 (func e-2) (vector 0 e-6 e-7))))))))) (((arg-143 r) s) w))))))))) (lambda (a) (lambda (arg-556) (lambda (_-1415) (lambda (s) (lambda (w) (let ((sc0 (let ((sc1 arg-4)) (let ((e-1 (vector-ref sc1 1))) e-1)))) (let ((e-2 (vector-ref sc0 2))) ((e-2 'erased) (vector 0 arg-556 (vector 0 s w)))))))))) (lambda (b) (lambda (a) (lambda (arg-557) (lambda (arg-559) (lambda (r) (lambda (s) (lambda (w) (let ((sc0 arg-4)) (let ((e-2 (vector-ref sc0 2))) ((((e-2 'erased) 'erased) (((arg-557 r) s) w)) (lambda (_-0) (let ((sc1 _-0)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (let ((sc3 arg-4)) (let ((e-9 (vector-ref sc3 2))) ((((e-9 'erased) 'erased) (((arg-559 r) e-6) e-7)) (lambda (_-1) (let ((sc4 _-1)) (let ((e-12 (vector-ref sc4 1))) (let ((e-11 (vector-ref sc4 2))) (let ((sc5 e-11)) (let ((e-14 (vector-ref sc5 1))) (let ((e-13 (vector-ref sc5 2))) (let ((sc6 (let ((sc7 arg-4)) (let ((e-17 (vector-ref sc7 1))) e-17)))) (let ((e-16 (vector-ref sc6 2))) ((e-16 'erased) (vector 0 (e-5 e-12) (vector 0 e-14 e-13)))))))))))))))))))))))))))))))))))
(define ControlC-45MonadC-45RWSC-45CPS-C-62C-62C-61_Monad_C-40C-40C-40C-40RWSTC-32C-36rC-41C-32C-36wC-41C-32C-36sC-41C-32C-36mC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 ext-0 ext-1 ext-2) (let ((sc0 arg-6)) (let ((e-2 (vector-ref sc0 2))) ((((e-2 'erased) 'erased) (((arg-7 ext-0) ext-1) ext-2)) (lambda (_-0) (let ((sc1 _-0)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) ((((arg-8 e-5) ext-0) e-6) e-7)))))))))))))
(define ControlC-45MonadC-45RWSC-45CPS-C-60C-42C-62_Applicative_C-40C-40C-40C-40RWSTC-32C-36rC-41C-32C-36wC-41C-32C-36sC-41C-32C-36mC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 ext-0 ext-1 ext-2) (let ((sc0 arg-6)) (let ((e-2 (vector-ref sc0 2))) ((((e-2 'erased) 'erased) (((arg-7 ext-0) ext-1) ext-2)) (lambda (_-0) (let ((sc1 _-0)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (let ((sc3 arg-6)) (let ((e-9 (vector-ref sc3 2))) ((((e-9 'erased) 'erased) (((arg-8 ext-0) e-6) e-7)) (lambda (_-1) (let ((sc4 _-1)) (let ((e-12 (vector-ref sc4 1))) (let ((e-11 (vector-ref sc4 2))) (let ((sc5 e-11)) (let ((e-14 (vector-ref sc5 1))) (let ((e-13 (vector-ref sc5 2))) (let ((sc6 (let ((sc7 arg-6)) (let ((e-17 (vector-ref sc7 1))) e-17)))) (let ((e-16 (vector-ref sc6 2))) ((e-16 'erased) (vector 0 (e-5 e-12) (vector 0 e-14 e-13)))))))))))))))))))))))))))
(define ControlC-45MonadC-45RWSC-45CPSC-45RWST-unRWST (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5) arg-5))
(define ControlC-45MonadC-45RWSC-45CPS-runRWST (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8) (((arg-6 arg-7) arg-8) (let ((sc0 arg-5)) (let ((e-2 (vector-ref sc0 2))) e-2)))))
(define ControlC-45MonadC-45RWSC-45CPS-execRWST (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8) (PreludeC-45Interfaces-C-60C-36C-62 'erased 'erased 'erased (Builtin-fst 'erased 'erased arg-5) (lambda (lamc-0) (let ((sc0 lamc-0)) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (vector 0 e-6 e-7))))))) (((arg-6 arg-7) arg-8) (let ((sc0 (Builtin-snd 'erased 'erased arg-5))) (let ((e-2 (vector-ref sc0 2))) e-2))))))
(define ControlC-45MonadC-45ReaderC-45Interface-local_MonadReader_C-36r_C-40C-40C-40C-40RWSTC-32C-36rC-41C-32C-36wC-41C-32C-36sC-41C-32C-36mC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 ext-0 ext-1 ext-2) (((arg-8 (arg-7 ext-0)) ext-1) ext-2)))
(define ControlC-45MonadC-45ReaderC-45Interface-ask_MonadReader_C-36r_C-40C-40C-40C-40RWSTC-32C-36rC-41C-32C-36wC-41C-32C-36sC-41C-32C-36mC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 ext-0 ext-1 ext-2) (let ((sc0 (let ((sc1 arg-4)) (let ((e-1 (vector-ref sc1 1))) e-1)))) (let ((e-2 (vector-ref sc0 2))) ((e-2 'erased) (vector 0 ext-0 (vector 0 ext-1 ext-2)))))))
(define ControlC-45MonadC-45ReaderC-45Interface-__Impl_MonadReader_C-36r_C-40C-40C-40C-40RWSTC-32C-36rC-41C-32C-36wC-41C-32C-36sC-41C-32C-36mC-41 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (vector 0 (vector 0 (vector 0 (lambda (b) (lambda (a) (lambda (func) (lambda (arg-143) (lambda (r) (lambda (s) (lambda (w) (PreludeC-45Interfaces-C-60C-36C-62 'erased 'erased 'erased (let ((sc0 (let ((sc1 arg-4)) (let ((e-1 (vector-ref sc1 1))) e-1)))) (let ((e-1 (vector-ref sc0 1))) e-1)) (lambda (lamc-0) (let ((sc0 lamc-0)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (let ((sc1 e-3)) (let ((e-6 (vector-ref sc1 1))) (let ((e-7 (vector-ref sc1 2))) (vector 0 (func e-2) (vector 0 e-6 e-7))))))))) (((arg-143 r) s) w))))))))) (lambda (a) (lambda (arg-556) (lambda (_-1415) (lambda (s) (lambda (w) (let ((sc0 (let ((sc1 arg-4)) (let ((e-1 (vector-ref sc1 1))) e-1)))) (let ((e-2 (vector-ref sc0 2))) ((e-2 'erased) (vector 0 arg-556 (vector 0 s w)))))))))) (lambda (b) (lambda (a) (lambda (arg-557) (lambda (arg-559) (lambda (r) (lambda (s) (lambda (w) (let ((sc0 arg-4)) (let ((e-2 (vector-ref sc0 2))) ((((e-2 'erased) 'erased) (((arg-557 r) s) w)) (lambda (_-0) (let ((sc1 _-0)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (let ((sc3 arg-4)) (let ((e-9 (vector-ref sc3 2))) ((((e-9 'erased) 'erased) (((arg-559 r) e-6) e-7)) (lambda (_-1) (let ((sc4 _-1)) (let ((e-12 (vector-ref sc4 1))) (let ((e-11 (vector-ref sc4 2))) (let ((sc5 e-11)) (let ((e-14 (vector-ref sc5 1))) (let ((e-13 (vector-ref sc5 2))) (let ((sc6 (let ((sc7 arg-4)) (let ((e-17 (vector-ref sc7 1))) e-17)))) (let ((e-16 (vector-ref sc6 2))) ((e-16 'erased) (vector 0 (e-5 e-12) (vector 0 e-14 e-13))))))))))))))))))))))))))))))))) (lambda (b) (lambda (a) (lambda (arg-855) (lambda (arg-856) (lambda (r) (lambda (s) (lambda (w) (let ((sc0 arg-4)) (let ((e-2 (vector-ref sc0 2))) ((((e-2 'erased) 'erased) (((arg-855 r) s) w)) (lambda (_-0) (let ((sc1 _-0)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) ((((arg-856 e-5) r) e-6) e-7)))))))))))))))))) (lambda (a) (lambda (arg-858) (lambda (r) (lambda (s) (lambda (w) (let ((sc0 arg-4)) (let ((e-2 (vector-ref sc0 2))) ((((e-2 'erased) 'erased) (((arg-858 r) s) w)) (lambda (_-0) (let ((sc1 _-0)) (let ((e-5 (vector-ref sc1 1))) (let ((e-4 (vector-ref sc1 2))) (let ((sc2 e-4)) (let ((e-6 (vector-ref sc2 1))) (let ((e-7 (vector-ref sc2 2))) (((e-5 r) e-6) e-7))))))))))))))))) (lambda (r) (lambda (s) (lambda (w) (let ((sc0 (let ((sc1 arg-4)) (let ((e-1 (vector-ref sc1 1))) e-1)))) (let ((e-2 (vector-ref sc0 2))) ((e-2 'erased) (vector 0 r (vector 0 s w)))))))) (lambda (a) (lambda (i_con-0) (lambda (arg-1404) (lambda (arg-1406) (lambda (r) (lambda (s) (lambda (w) (((arg-1406 (arg-1404 r)) s) w)))))))))))
(define ControlC-45MonadC-45ReaderC-45Interface-ask (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (let ((e-3 (vector-ref sc0 2))) e-3))))
(collect-request-handler (lambda () (collect) (blodwen-run-finalisers)))
(PrimIO-unsafePerformIO 'erased (lambda (eta-0) (AStar-test eta-0)))(collect 4)
(blodwen-run-finalisers))
