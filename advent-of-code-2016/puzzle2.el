;; --- Day 2: Bathroom Security ---

;; You arrive at Easter Bunny Headquarters under cover of darkness. However, you left in such a rush that you forgot to use the bathroom! Fancy office buildings like this one usually have keypad locks on their bathrooms, so you search the front desk for the code.

;; "In order to improve security," the document you find says, "bathroom codes will no longer be written down. Instead, please memorize and follow the procedure below to access the bathrooms."

;; The document goes on to explain that each button to be pressed can be found by starting on the previous button and moving to adjacent buttons on the keypad: U moves up, D moves down, L moves left, and R moves right. Each line of instructions corresponds to one button, starting at the previous button (or, for the first line, the "5" button); press whatever button you're on at the end of each line. If a move doesn't lead to a button, ignore it.

;; You can't hold it much longer, so you decide to figure out the code as you walk to the bathroom. You picture a keypad like this:

;; 1 2 3
;; 4 5 6
;; 7 8 9
;; Suppose your instructions are:

;; ULL
;; RRDDD
;; LURDL
;; UUUUD
;; You start at "5" and move up (to "2"), left (to "1"), and left (you can't, and stay on "1"), so the first button is 1.
;; Starting from the previous button ("1"), you move right twice (to "3") and then down three times (stopping at "9" after two moves and ignoring the third), ending up with 9.
;; Continuing from "9", you move left, up, right, down, and left, ending with 8.
;; Finally, you move up four times (stopping at "2"), then down once, ending with 5.
;; So, in this example, the bathroom code is 1985.

;; Your puzzle input is the instructions from the document you found at the front desk. What is the bathroom code?

(defvar *instructions* '("RDLULDLDDRLLLRLRULDRLDDRRRRURLRLDLULDLDLDRULDDLLDRDRUDLLDDRDULLLULLDULRRLDURULDRUULLLUUDURURRDDLDLDRRDDLRURLLDRRRDULDRULURURURURLLRRLUDULDRULLDURRRLLDURDRRUUURDRLLDRURULRUDULRRRRRDLRLLDRRRDLDUUDDDUDLDRUURRLLUDUDDRRLRRDRUUDUUULDUUDLRDLDLLDLLLLRRURDLDUURRLLDLDLLRLLRULDDRLDLUDLDDLRDRRDLULRLLLRUDDURLDLLULRDUUDRRLDUDUDLUURDURRDDLLDRRRLUDULDULDDLLULDDDRRLLDURURURUUURRURRUUDUUURULDLRULRURDLDRDDULDDULLURDDUDDRDRRULRUURRDDRLLUURDRDDRUDLUUDURRRLLRR"
			 "RDRRLURDDDDLDUDLDRURRLDLLLDDLURLLRULLULUUURLDURURULDLURRLRULDDUULULLLRLLRDRRUUDLUUDDUDDDRDURLUDDRULRULDDDLULRDDURRUURLRRLRULLURRDURRRURLDULULURULRRLRLUURRRUDDLURRDDUUDRDLLDRLRURUDLDLLLLDLRURDLLRDDUDDLDLDRRDLRDRDLRRRRUDUUDDRDLULUDLUURLDUDRRRRRLUUUDRRDLULLRRLRLDDDLLDLLRDDUUUUDDULUDDDUULDDUUDURRDLURLLRUUUUDUDRLDDDURDRLDRLRDRULRRDDDRDRRRLRDULUUULDLDDDUURRURLDLDLLDLUDDLDLRUDRLRLDURUDDURLDRDDLLDDLDRURRULLURULUUUUDLRLUUUDLDRUDURLRULLRLLUUULURLLLDULLUDLLRULRRLURRRRLRDRRLLULLLDURDLLDLUDLDUDURLURDLUURRRLRLLDRLDLDRLRUUUDRLRUDUUUR"
			 "LLLLULRDUUDUUDRDUUURDLLRRLUDDDRLDUUDDURLDUDULDRRRDDLLLRDDUDDLLLRRLURDULRUUDDRRDLRLRUUULDDULDUUUDDLLDDDDDURLDRLDDDDRRDURRDRRRUUDUUDRLRRRUURUDURLRLDURDDDUDDUDDDUUDRUDULDDRDLULRURDUUDLRRDDRRDLRDLRDLULRLLRLRLDLRULDDDDRLDUURLUUDLLRRLLLUUULURUUDULRRRULURUURLDLLRURUUDUDLLUDLDRLLRRUUDDRLUDUDRDDRRDDDURDRUDLLDLUUDRURDLLULLLLUDLRRRUULLRRDDUDDDUDDRDRRULURRUUDLUDLDRLLLLDLUULLULLDDUDLULRDRLDRDLUDUDRRRRLRDLLLDURLULUDDRURRDRUDLLDRURRUUDDDRDUUULDURRULDLLDLDLRDUDURRRRDLDRRLUDURLUDRRLUDDLLDUULLDURRLRDRLURURLUUURRLUDRRLLULUULUDRUDRDLUL"
			 "LRUULRRUDUDDLRRDURRUURDURURLULRDUUDUDLDRRULURUDURURDRLDDLRUURLLRDLURRULRRRUDULRRULDLUULDULLULLDUDLLUUULDLRDRRLUURURLLUUUDDLLURDUDURULRDLDUULDDRULLUUUURDDRUURDDDRUUUDRUULDLLULDLURLRRLRULRLDLDURLRLDLRRRUURLUUDULLLRRURRRLRULLRLUUDULDULRDDRDRRURDDRRLULRDURDDDDDLLRRDLLUUURUULUDLLDDULDUDUUDDRURDDURDDRLURUDRDRRULLLURLUULRLUDUDDUUULDRRRRDLRLDLLDRRDUDUUURLRURDDDRURRUDRUURUUDLRDDDLUDLRUURULRRLDDULRULDRLRLLDRLURRUUDRRRLRDDRLDDLLURLLUDL"
			 "ULURLRDLRUDLLDUDDRUUULULUDDDDDRRDRULUDRRUDLRRRLUDLRUULRDDRRLRUDLUDULRULLUURLLRLLLLDRDUURDUUULLRULUUUDRDRDRUULURDULDLRRULUURURDULULDRRURDLRUDLULULULUDLLUURULDLLLRDUDDRRLULUDDRLLLRURDDLDLRLLLRDLDRRUUULRLRDDDDRUDRUULDDRRULLDRRLDDRRUDRLLDUDRRUDDRDLRUDDRDDDRLLRDUULRDRLDUDRLDDLLDDDUUDDRULLDLLDRDRRUDDUUURLLUURDLULUDRUUUDURURLRRDULLDRDDRLRDULRDRURRUDLDDRRRLUDRLRRRRLLDDLLRLDUDUDDRRRUULDRURDLLDLUULDLDLDUUDDULUDUDRRDRLDRDURDUULDURDRRDRRLLRLDLU"))

(defvar *position* 5)

(defun move (direction)
  (cond
   ((char-equal direction ?u)
    (when (plusp (- *position* 3))
      (decf *position* 3)))
   ((char-equal direction ?d)
    (when (< (+ *position* 3) 10)
      (incf *position* 3)))
   ((char-equal direction ?r)
    (unless (= 1 (mod (1+ *position*) 3))
      (incf *position*)))
   ((char-equal direction ?l)
    (unless (zerop (mod (1- *position*) 3))
      (decf *position*)))
   (t (error "Incorrect direction: %s" direction)))
  *position*)

(defun solve-puzzle (&optional instructions)
  (cl-loop
   for instruction in (or instructions *instructions*)
   collect (cl-loop
	    for char across instruction
	    do (move char)
	    finally (return *position*))))

(defun test-solution ()
  (and
   (equal (solve-puzzle '("ULL" "RRDDD" "LURDL" "UUUUD")) '(1 9 8 5))))

;; --- Part Two ---

;; You finally arrive at the bathroom (it's a several minute walk from the lobby so visitors can behold the many fancy conference rooms and water coolers on this floor) and go to punch in the code. Much to your bladder's dismay, the keypad is not at all like you imagined it. Instead, you are confronted with the result of hundreds of man-hours of bathroom-keypad-design meetings:

;;     1
;;   2 3 4
;; 5 6 7 8 9
;;   A B C
;;     D
;; You still start at "5" and stop when you're at an edge, but given the same instructions as above, the outcome is very different:

;; You start at "5" and don't move at all (up and left are both edges), ending at 5.
;; Continuing from "5", you move right twice and down three times (through "6", "7", "B", "D", "D"), ending at D.
;; Then, from "D", you move five more times (through "D", "B", "C", "C", "B"), ending at B.
;; Finally, after five more moves, you end at 3.
;; So, given the actual keypad layout, the code would be 5DB3.

(defvar *map*
  '(1 ("u" 1 "d" 3 "l" 1 "r" 1)
      2 ("u" 2 "d" 6 "l" 2 "r" 3)
      3 ("u" 1 "d" 7 "l" 2 "r" 4)
      4 ("u" 4 "d" 8 "l" 3 "r" 4)
      5 ("u" 5 "d" 5 "l" 5 "r" 6)
      6 ("u" 2 "d" 10 "l" 5 "r" 7)
      7 ("u" 3 "d" 11 "l" 6 "r" 8)
      8 ("u" 4 "d" 12 "l" 7 "r" 9)
      9 ("u" 9 "d" 9 "l" 8 "r" 9)
      10 ("u" 6 "d" 10 "l" 10 "r" 11)
      11 ("u" 7 "d" 13 "l" 10 "r" 12)
      12 ("u" 8 "d" 12 "l" 11 "r" 12)
      13 ("u" 11 "d" 13 "l" 13 "r" 13)))

(defun move-2-alt (direction)
  "Data-driven move function using a map"
  (assert (and (characterp direction)
               (find direction '(?u ?d ?r ?l) :test #'char-equal)))
  (setf *position* (lax-plist-get (lax-plist-get *map* *position*)
                                  (char-to-string (downcase direction)))))

(defun move-2 (direction)
  (cond
   ((char-equal direction ?u)
    (unless (or (and (oddp *position*) (< *position* 10) (= 1 (mod *position* 4)))
                (and (evenp *position*) (<= *position* 4)))
      (if (evenp *position*)
          (decf *position* 4)
        (decf *position* (if (member *position* '(3 13)) 2 4)))))
   ((char-equal direction ?d)
    (unless (or (and (oddp *position*) (> *position* 1) (= 1 (mod *position* 4)))
                (and (evenp *position*) (>= *position* 10)))
      (if (evenp *position*)
          (incf *position* 4)
        (incf *position* (if (member *position* '(1 11)) 2 4)))))
   ((char-equal direction ?r)
    (unless (member *position* '(1 4 9 12 13))
      (incf *position*)))
   ((char-equal direction ?l)
    (unless (member *position* '(1 2 5 10 13))
      (decf *position*)))
   (t (error "Incorrect direction: %s" direction)))
  (format "%x" *position*))

(defun solve-puzzle-2 (&optional instructions)
  (cl-loop
   for instruction in (or instructions *instructions*)
   collect (cl-loop
	    for char across instruction
	    do (move-2-alt char)
	    finally (return (format "%x" *position*)))))
