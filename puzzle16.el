;; --- Day 16: Dragon Checksum ---

;; You're done scanning this part of the network, but you've left traces of your presence. You need to overwrite some disks with random-looking data to cover your tracks and update the local security system with a new checksum for those disks.

;; For the data to not be suspiscious, it needs to have certain properties; purely random data will be detected as tampering. To generate appropriate random data, you'll need to use a modified dragon curve.

;; Start with an appropriate initial state (your puzzle input). Then, so long as you don't have enough data yet to fill the disk, repeat the following steps:

;; Call the data you have at this point "a".
;; Make a copy of "a"; call this copy "b".
;; Reverse the order of the characters in "b".
;; In "b", replace all instances of 0 with 1 and all 1s with 0.
;; The resulting data is "a", then a single 0, then "b".
;; For example, after a single step of this process,

;; 1 becomes 100.
;; 0 becomes 001.
;; 11111 becomes 11111000000.
;; 111100001010 becomes 1111000010100101011110000.
;; Repeat these steps until you have enough data to fill the desired disk.

;; Once the data has been generated, you also need to create a checksum of that data. Calculate the checksum only for the data that fits on the disk, even if you generated more data than that in the previous step.

;; The checksum for some given data is created by considering each non-overlapping pair of characters in the input data. If the two characters match (00 or 11), the next checksum character is a 1. If the characters do not match (01 or 10), the next checksum character is a 0. This should produce a new string which is exactly half as long as the original. If the length of the checksum is even, repeat the process until you end up with a checksum with an odd length.

;; For example, suppose we want to fill a disk of length 12, and when we finally generate a string of at least length 12, the first 12 characters are 110010110100. To generate its checksum:

;; Consider each pair: 11, 00, 10, 11, 01, 00.
;; These are same, same, different, same, different, same, producing 110101.
;; The resulting string has length 6, which is even, so we repeat the process.
;; The pairs are 11 (same), 01 (different), 01 (different).
;; This produces the checksum 100, which has an odd length, so we stop.
;; Therefore, the checksum for 110010110100 is 100.

;; Combining all of these steps together, suppose you want to fill a disk of length 20 using an initial state of 10000:

;; Because 10000 is too short, we first use the modified dragon curve to make it longer.
;; After one round, it becomes 10000011110 (11 characters), still too short.
;; After two rounds, it becomes 10000011110010000111110 (23 characters), which is enough.
;; Since we only need 20, but we have 23, we get rid of all but the first 20 characters: 10000011110010000111.
;; Next, we start calculating the checksum; after one round, we have 0111110101, which 10 characters long (even), so we continue.
;; After two rounds, we have 01100, which is 5 characters long (odd), so we are done.
;; In this example, the correct checksum would therefore be 01100.

;; The first disk you have to fill has length 272. Using the initial state in your puzzle input, what is the correct checksum?

(defvar *input* "11011110011011101")

(defun dragon (data)
  (let* ((rev (reverse data))
	 (step1 (replace-regexp-in-string "1" "x" rev))
	 (step2 (replace-regexp-in-string "0" "1" step1))
	 (step3 (replace-regexp-in-string "x" "0" step2)))
    (concat data "0" step3)))

(defun v-dragon (data)
  (vconcat data [nil] (map 'vector #'not (reverse data))))

(defun checksum-step (input)
  (cl-loop
   for (x y) on (string-to-list input) by #'cddr
   if (char-equal x y)
   collect ?1 into output
   else
   collect ?0 into output
   finally (return (apply #'string output))))

(defun v-checksum-step (input)
  (cl-loop
   with output = []
   for i = 0 then (+ i 2)
   until (= i (length input))
   if (eq (aref input i) (aref input (1+ i)))
   do (setf output (vconcat output [t]))
   else
   do (setf output (vconcat output [nil]))
   finally (return output)))

(defun checksum (input)
  (cl-loop
   for data = input then (checksum-step data)
   until (oddp (length data))
   finally (return data)))

(defun v-checksum (input)
  (cl-loop
   for data = input then (v-checksum-step data)
   until (oddp (length data))
   finally (return data)))

(defun solve-puzzle (&optional input)
  (cl-loop
   for data = (or input *input*) then (dragon data)
   until
   (>= (length data) 272)
   finally (return (checksum (subseq data 0 272)))))

;; --- Part Two ---

;; The second disk you have to fill has length 35651584. Again using the initial state in your puzzle input, what is the correct checksum for this disk?

(defun string-to-bit (string)
  (map 'vector (lambda (c) (not (char-equal c ?0))) string))

(defun solve-puzzle-2 (&optional input)
  (cl-loop
   for i from 1
   for data = (string-to-bit (or input *input*)) then (v-dragon data)
   do (print (format "Iteration: %d Data length: %d" i (length data)))
   until
   (>= (length data) 35651584)
   finally (return (v-checksum (subseq data 0 35651584)))))
