;; --- Day 1: No Time for a Taxicab ---

;; Santa's sleigh uses a very high-precision clock to guide its movements, and the clock's oscillator is regulated by stars. Unfortunately, the stars have been stolen... by the Easter Bunny. To save Christmas, Santa needs you to retrieve all fifty stars by December 25th.

;; Collect stars by solving puzzles. Two puzzles will be made available on each day in the advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

;; You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near", unfortunately, is as close as you can get - the instructions on the Easter Bunny Recruiting Document the Elves intercepted start here, and nobody had time to work them out further.

;; The Document indicates that you should start at the given coordinates (where you just landed) and face North. Then, follow the provided sequence: either turn left (L) or right (R) 90 degrees, then walk forward the given number of blocks, ending at a new intersection.

;; There's no time to follow such ridiculous instructions on foot, though, so you take a moment and work out the destination. Given that you can only walk on the street grid of the city, how far is the shortest path to the destination?

;; For example:

;;     Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
;;     R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away.
;;     R5, L5, R5, R3 leaves you 12 blocks away.

;; How many blocks away is Easter Bunny HQ?

;; Input:
(defvar *directions*
  '(R2 L1 R2 R1 R1 L3 R3 L5 L5 L2 L1 R4 R1 R3 L5 L5 R3 L4 L4 R5 R4 R3 L1 L2 R5 R4 L2 R1 R4 R4 L2 L1 L1 R190 R3 L4 R52 R5 R3 L5 R3 R2 R1 L5 L5 L4 R2 L3 R3 L1 L3 R5 L3 L4 R3 R77 R3 L2 R189 R4 R2 L2 R2 L1 R5 R4 R4 R2 L2 L2 L5 L1 R1 R2 L3 L4 L5 R1 L1 L2 L2 R2 L3 R3 L4 L1 L5 L4 L4 R3 R5 L2 R4 R5 R3 L2 L2 L4 L2 R2 L5 L4 R3 R1 L2 R2 R4 L1 L4 L4 L2 R2 L4 L1 L1 R4 L1 L3 L2 L2 L5 R5 R2 R5 L1 L5 R2 R4 R4 L2 R5 L5 R5 R5 L4 R2 R1 R1 R3 L3 L3 L4 L3 L2 L2 L2 R2 L1 L3 R2 R5 R5 L4 R3 L3 L4 R2 L5 R5))

;; Azimuth pointer. Indicates current direction you're facing
;;    0
;; 3     1
;;    2
(defvar *azimuth* 0)

;; Position relative to start. car - x, cdr - y
(defvar *position* (cons 0 0))

;;;;;;;;;;;;
;; Part 1 ;;
;;;;;;;;;;;;

(defun change-azimuth (rotation)
  (cond
   ((member rotation '(r R "r" "R"))
    (setf *azimuth* (mod (1+ *azimuth*) 4)))
   ((member rotation '(l L "l" "L"))
    (setf *azimuth* (mod (1- *azimuth*) 4)))
   (t (error "Invalid direction: %s" rotation))))

(defun split-direction (direction)
  (let* ((direction-string (symbol-name direction))
         (direction-symbol (substring direction-string 0 1))
         (direction-value (string-to-number (substring direction-string 1))))
    (cons direction-symbol direction-value)))

(defun produce-interval (a b)
  (if (< a b)
      (cl-loop for i from (1+ a) to b collect i)
    (cl-loop for i downfrom (1- a) to b collect i)))

(defun change-position (direction)
  (let ((dir (split-direction direction))
        (old-pos (copy-list *position*)))
    (change-azimuth (car dir))
    (if (zerop (mod *azimuth* 2))
        ;; y-direction
        (incf (cdr *position*) (* (- 1 *azimuth*) (cdr dir)))
      ;; x-direction
      (incf (car *position*) (* (- 2 *azimuth*) (cdr dir))))
    (message "Old: %s New: %s" old-pos *position*)
    (cond
     ((= (car old-pos) (car *position*))
      (mapcar (lambda (y) (cons (car *position*) y))
              (produce-interval (cdr old-pos) (cdr *position*))))
     ((= (cdr old-pos) (cdr *position*))
      (mapcar (lambda (x) (cons x (cdr *position*)))
              (produce-interval (car old-pos) (car *position*))))
     (t (error "Invalid position change: %s" direction)))))

(defun solve-puzzle (&optional directions)
  ;; reset *position* and *azimuth*
  (setf *azimuth* 0
        *position* (cons 0 0))
  ;; *position* update
  (mapcar #'change-position (or directions *directions*))
  ;; Calculate block-metric distance
  (+ (abs (car *position*)) (abs (cdr *position*))))

(defun test-solution ()
  (and
   (= (solve-puzzle '(rpp2 l3)) 5)
   (= (solve-puzzle '(r2 r2 r2)) 2)
   (= (solve-puzzle '(r5 l5 r5 r3)) 12)))

;;;;;;;;;;;;
;; Part 2 ;;
;;;;;;;;;;;;

(defun solve-puzzle-2 (&optional directions)
  (setf *azimuth* 0
        *position* (cons 0 0))
  (cl-loop
   with positions = nil
   for direction in (or directions *directions*)
   for position-list = (change-position direction)
   append position-list into positions
   finally (return (cl-loop
                    for i on positions
                    when (find (car i) (cdr i) :test #'equal)
                    do (return (+ (abs (car (car i)))
                                  (abs (cdr (car i)))))))))

(defun test-solution-2 ()
  (and
   (= (solve-puzzle-2 '(r8 r4 r4 r8)) 4)))
