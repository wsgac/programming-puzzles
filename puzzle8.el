;; --- Day 8: Two-Factor Authentication ---

;; You come across a door implementing what you can only assume is an implementation of two-factor authentication after a long game of requirements telephone.

;; To get past the door, you first swipe a keycard (no problem; there was one on a nearby desk). Then, it displays a code on a little screen, and you type that code on a keypad. Then, presumably, the door unlocks.

;; Unfortunately, the screen has been smashed. After a few minutes, you've taken everything apart and figured out how it works. Now you just have to work out what the screen would have displayed.

;; The magnetic strip on the card you swiped encodes a series of instructions for the screen; these instructions are your puzzle input. The screen is 50 pixels wide and 6 pixels tall, all of which start off, and is capable of three somewhat peculiar operations:

;; rect AxB turns on all of the pixels in a rectangle at the top-left of the screen which is A wide and B tall.
;; rotate row y=A by B shifts all of the pixels in row A (0 is the top row) right by B pixels. Pixels that would fall off the right end appear at the left end of the row.
;; rotate column x=A by B shifts all of the pixels in column A (0 is the left column) down by B pixels. Pixels that would fall off the bottom appear at the top of the column.
;; For example, here is a simple sequence on a smaller screen:

;; rect 3x2 creates a small rectangle in the top-left corner:

;; ###....
;; ###....
;; .......
;; rotate column x=1 by 1 rotates the second column down by one pixel:

;; #.#....
;; ###....
;; .#.....
;; rotate row y=0 by 4 rotates the top row right by four pixels:

;; ....#.#
;; ###....
;; .#.....
;; rotate column x=1 by 1 again rotates the second column down by one pixel, causing the bottom pixel to wrap back to the top:

;; .#..#.#
;; #.#....
;; .#.....
;; As you can see, this display technology is extremely powerful, and will soon dominate the tiny-code-displaying-screen market. That's what the advertisement on the back of the display tries to convince you, anyway.

;; There seems to be an intermediate check of the voltage used by the display: after you swipe your card, if the screen did work, how many pixels should be lit?

(defvar *input*
  '("rect 1x1"
    "rotate row y=0 by 2"
    "rect 1x1"
    "rotate row y=0 by 5"
    "rect 1x1"
    "rotate row y=0 by 3"
    "rect 1x1"
    "rotate row y=0 by 3"
    "rect 2x1"
    "rotate row y=0 by 5"
    "rect 1x1"
    "rotate row y=0 by 5"
    "rect 4x1"
    "rotate row y=0 by 2"
    "rect 1x1"
    "rotate row y=0 by 2"
    "rect 1x1"
    "rotate row y=0 by 5"
    "rect 4x1"
    "rotate row y=0 by 3"
    "rect 2x1"
    "rotate row y=0 by 5"
    "rect 4x1"
    "rotate row y=0 by 2"
    "rect 1x2"
    "rotate row y=1 by 6"
    "rotate row y=0 by 2"
    "rect 1x2"
    "rotate column x=32 by 1"
    "rotate column x=23 by 1"
    "rotate column x=13 by 1"
    "rotate row y=0 by 6"
    "rotate column x=0 by 1"
    "rect 5x1"
    "rotate row y=0 by 2"
    "rotate column x=30 by 1"
    "rotate row y=1 by 20"
    "rotate row y=0 by 18"
    "rotate column x=13 by 1"
    "rotate column x=10 by 1"
    "rotate column x=7 by 1"
    "rotate column x=2 by 1"
    "rotate column x=0 by 1"
    "rect 17x1"
    "rotate column x=16 by 3"
    "rotate row y=3 by 7"
    "rotate row y=0 by 5"
    "rotate column x=2 by 1"
    "rotate column x=0 by 1"
    "rect 4x1"
    "rotate column x=28 by 1"
    "rotate row y=1 by 24"
    "rotate row y=0 by 21"
    "rotate column x=19 by 1"
    "rotate column x=17 by 1"
    "rotate column x=16 by 1"
    "rotate column x=14 by 1"
    "rotate column x=12 by 2"
    "rotate column x=11 by 1"
    "rotate column x=9 by 1"
    "rotate column x=8 by 1"
    "rotate column x=7 by 1"
    "rotate column x=6 by 1"
    "rotate column x=4 by 1"
    "rotate column x=2 by 1"
    "rotate column x=0 by 1"
    "rect 20x1"
    "rotate column x=47 by 1"
    "rotate column x=40 by 2"
    "rotate column x=35 by 2"
    "rotate column x=30 by 2"
    "rotate column x=10 by 3"
    "rotate column x=5 by 3"
    "rotate row y=4 by 20"
    "rotate row y=3 by 10"
    "rotate row y=2 by 20"
    "rotate row y=1 by 16"
    "rotate row y=0 by 9"
    "rotate column x=7 by 2"
    "rotate column x=5 by 2"
    "rotate column x=3 by 2"
    "rotate column x=0 by 2"
    "rect 9x2"
    "rotate column x=22 by 2"
    "rotate row y=3 by 40"
    "rotate row y=1 by 20"
    "rotate row y=0 by 20"
    "rotate column x=18 by 1"
    "rotate column x=17 by 2"
    "rotate column x=16 by 1"
    "rotate column x=15 by 2"
    "rotate column x=13 by 1"
    "rotate column x=12 by 1"
    "rotate column x=11 by 1"
    "rotate column x=10 by 1"
    "rotate column x=8 by 3"
    "rotate column x=7 by 1"
    "rotate column x=6 by 1"
    "rotate column x=5 by 1"
    "rotate column x=3 by 1"
    "rotate column x=2 by 1"
    "rotate column x=1 by 1"
    "rotate column x=0 by 1"
    "rect 19x1"
    "rotate column x=44 by 2"
    "rotate column x=40 by 3"
    "rotate column x=29 by 1"
    "rotate column x=27 by 2"
    "rotate column x=25 by 5"
    "rotate column x=24 by 2"
    "rotate column x=22 by 2"
    "rotate column x=20 by 5"
    "rotate column x=14 by 3"
    "rotate column x=12 by 2"
    "rotate column x=10 by 4"
    "rotate column x=9 by 3"
    "rotate column x=7 by 3"
    "rotate column x=3 by 5"
    "rotate column x=2 by 2"
    "rotate row y=5 by 10"
    "rotate row y=4 by 8"
    "rotate row y=3 by 8"
    "rotate row y=2 by 48"
    "rotate row y=1 by 47"
    "rotate row y=0 by 40"
    "rotate column x=47 by 5"
    "rotate column x=46 by 5"
    "rotate column x=45 by 4"
    "rotate column x=43 by 2"
    "rotate column x=42 by 3"
    "rotate column x=41 by 2"
    "rotate column x=38 by 5"
    "rotate column x=37 by 5"
    "rotate column x=36 by 5"
    "rotate column x=33 by 1"
    "rotate column x=28 by 1"
    "rotate column x=27 by 5"
    "rotate column x=26 by 5"
    "rotate column x=25 by 1"
    "rotate column x=23 by 5"
    "rotate column x=22 by 1"
    "rotate column x=21 by 2"
    "rotate column x=18 by 1"
    "rotate column x=17 by 3"
    "rotate column x=12 by 2"
    "rotate column x=11 by 2"
    "rotate column x=7 by 5"
    "rotate column x=6 by 5"
    "rotate column x=5 by 4"
    "rotate column x=3 by 5"
    "rotate column x=2 by 5"
    "rotate column x=1 by 3"
    "rotate column x=0 by 4"))

(defvar *screen* (cl-loop
		  with row = (make-vector 50 0)
		  repeat 6
		  collect (copy-tree row t) into rows
		  finally (return (apply #'vector rows))))

(defvar *test-screen* (cl-loop
		       with row = (make-vector 7 0)
		       repeat 3
		       collect (copy-tree row t) into rows
		       finally (return (apply #'vector rows))))

(defun parse (instruction &optional screen)
  (let ((scr (or screen *screen*)))
    (cond
     ((string-match "rotate column x=\\([0-9]+\\) by \\([0-9]+\\)"
		    instruction)
      (let ((column (string-to-number (match-string 1 instruction)))
	    (rotation (string-to-number (match-string 2 instruction))))
	(insert-column column
		       (rotate-list (extract-column column scr)
				    rotation) scr)))
     ((string-match "rotate row y=\\([0-9]+\\) by \\([0-9]+\\)"
		    instruction)
      (let ((row (string-to-number (match-string 1 instruction)))
	    (rotation (string-to-number (match-string 2 instruction))))
	(insert-row row
		    (rotate-list (extract-row row scr)
				 rotation) scr)))
     ((string-match "rect \\([0-9]+\\)x\\([0-9]+\\)"
		    instruction)
      (let ((width (string-to-number (match-string 1 instruction)))
	    (height (string-to-number (match-string 2 instruction))))
	(set-rectangle width height scr)))
     (t
      (error "Instruction not recognized: %s" instruction)))))

(defun rotate-list (list n)
  (let ((n-mod (mod n (length list))))
    (if (zerop n)
	list
      (rotate-list (append (last list) (butlast list)) (1- n)))))

(defun extract-row (row &optional screen)
  (append (elt (or screen *screen*) row) nil))

(defun extract-column (column &optional screen)
  (cl-loop
   for row across (or screen *screen*)
   collect (elt row column)))

(defun insert-row (row elements &optional screen)
  (cl-loop
   for col from 0
   for el in elements
   do (setf (aref (aref (or screen *screen*) row) col) el)))

(defun insert-column (col-num col &optional screen)
  (cl-loop
   for i from 0
   for new-el in col
   do (setf (aref (aref (or screen *screen*) i) col-num) new-el)))

(defun set-rectangle (width height &optional screen)
  (cl-loop
   for i from 0 below width
   do (cl-loop
       for j from 0 below height
       do (setf (aref (aref (or screen *screen*) j) i) 1))))

(defun solve-puzzle (&optional screen)
  (let ((scr (or screen *screen*)))
    (cl-loop
     for instr in *input*
     do (parse instr scr))
    (cl-loop
     for row across scr
     sum (cl-loop
	  for col across row
	  count col))))
	  
