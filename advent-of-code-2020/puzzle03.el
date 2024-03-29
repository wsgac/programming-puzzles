;;;;;;;;;;;
;; Day 3 ;;
;;;;;;;;;;;

(defun day-03-parse-input (input)
  "Parse the INPUT string encoding the terrain map
  pattern. Because this will be random-accessed multiple times,
  make it a 2D vector of chars."
  (apply #'vector (split-string input)))

(defun day-03-problem-1 (input dx dy)
  ""
  (let ((in (day-03-parse-input input)))
    (loop
     with rows = (length in)
     with cols = (length (aref in 0))
     for x = 0 then (mod (+ x dx) cols)
     for y = 0 then (+ y dy)
     while (< y rows)
     count (eql ?# (aref (aref in y) x)))))

(defun day-03-problem-2 (input)
  ""
  (reduce #'*
    (loop
     for (dx dy) in '((1 1) (3 1) (5 1) (7 1) (1 2))
     collect (day-03-problem-1 input dx dy))))

(defvar *day-03-input*
  ".....#....#...#.#..........#...
...#.................#.........
......#...#.#.#.....#..#.....#.
.........#.#.............##....
#..####..#..#.##.....#.........
...#........#..##...........#..
..##.#.#..#....#..#......#.#...
.......#.##....#.....###....#.#
.#..#...#......#..#....##.#....
...#...............#.#.....#...
..#............#..#.........#.#
#.#.#.............##.#......#..
#...#..............##...#.#....
...#....#................#.#..#
..#.#..#.#.#..#.....#.........#
#.............#..#..........#..
.#...#.#........###.#....#...#.
#......#....#.#..#.##..#.......
.##......###.#......#..##...#..
#..#....#........#......#.#....
.#..................#.....#.###
.#......#.#.#.....#...#.#......
....#.............#.#..........
....#..#.#........#..###.......
.....#....#...#........#.......
...........#.###....##.......#.
....#...........#.#..#...#.....
...........#.....#.#...........
.....#........#.....#.#..#....#
#.#......#.......#.......#.....
..........#.............#.#.#..
#...#..........#....#....#.....
.#.#......#..##...#.....#...##.
.#....#...#.#.......#.#........
....#....##.##...#.............
#...#....#..#.........#........
...#.....#.#...#.......#..#....
#.......#...#....##........#..#
####........#........#....#.#..
............##..........#......
.......#.....#.#.#.##..#...#...
..........#....#...##.........#
..##..####.......##......#.##..
.....#.#......##...#..#...#....
....#.#.#.........#........#...
...........###...#.........##..
.......#.#....#......#.##...##.
.................#...##.#...##.
.......#.......#..#.#..........
.#....#..#....#........#.......
...............#.##..#...##..#.
.###.#....#......#...#.#.....#.
.#.....##.......#.......#......
....#..#.....#.....#...........
.......#....#.................#
.......#.##...#...#......#.....
.#.....#...####.............#..
......#.........#..........#...
.........#....#....#........###
....#.........#......##.....#..
....#........##...##.....##...#
.#..#....#..........#...#.###.#
#..#......#...#........#.......
...#.........................#.
.............#........#........
.......#.#.#.....##.....#..#...
..##..##.........#.............
.#...#..#......#...##..##..###.
.....#....#...#...##.##........
.#.#..#...........#..#..#......
##..#...#..#...##..#....#......
...#...#...#.........#....###..
...##..#....#.#.#.......#...#..
..#.#.....#..#....#..##.......#
.....#.#.....#......#....#.#...
.......##....#.....#...#.....#.
..##..#.................#.#....
..............##....##.#..##...
.#..#.....#....#.#.#...........
......#.#.#..#..#...#.....#..##
..#.........#.#.......###...##.
#.....#...........#.....#.##.#.
#..........#....#....#..#....#.
.#.....#...#.......###......#..
....##..##......#....#....#....
.......#.#.............#....#.#
.#..#.##.##.##....#.#.....##.##
....#..##.#..#.............##..
....#...........#...#....#..#..
...........#..#....#....##.#.##
......#....#....#.....#......#.
.##.##....#.....#.#......#...#.
.....##.......#.#.#........##..
#..........##..#....#..#.#....#
...#...........................
...#..#...#..#.#.#.#.......#.#.
.....#.........#..###..........
...#.#......##....#......#..###
#..............#....#.......#..
.........##......###..###......
..#......##...........#.##.....
#.#..#......#...##.............
......#.#.............#....#..#
#.....##..#.#.................#
..##....#.....#....#.....###...
.#.#.##.....#..................
.#......#.#.#.....#..#....#....
..#.#.....##.#...#..#.#.##.....
..#.#..#......##.#.#..........#
.......##.....#..#...#....##.#.
...#.....#..........#..........
......................##......#
...###.........##.........#....
....#..................#.....#.
.##..#.............#........##.
....#....#...###..........#....
.....#.#..........###..........
..#......##......#.#.##.#..#...
##...........#.#..#.....#..#...
.........#......#..........#.#.
...#.##.#..#..###..#...........
....##.#.##...........#.....##.
....#...................###....
#.......#......#......#.....#..
#..........##..................
...#..#.#....#..#.........##.#.
......#...##.#...............#.
.........#....#.#...#..#..#....
...#......###..#......#.....#.#
#..###.#.............#.........
......#...........#............
..#..#.##.....#......#.#..#...#
.........#..............#......
........#.....#..#...#.....#..#
.....................#........#
.##.......##...#.###.........#.
.#...#.......#.#....##....#....
........#......#...........#.#.
....#......##...#.....#...#...#
..#.........#.#...............#
....#.....#......#.............
.............##.....#....#.....
........#......#.#.....#....#..
#.........#...#......#....#...#
.#........###...#.#.#...#....#.
.###...........#..#............
....##.........#..#...##.#..###
.####..#.#...............##.#..
#.....#...#....#.......##....#.
..#.....##...##.#...#..#.......
..#.###.......#.....#.......#..
...........#.......#....##....#
..#...#....##........###......#
...#..#..............#...#.....
##.#.............#....##.#..##.
##.#..#..............#.#.......
.......#....#....##............
.##..##.#..........#.#...#.#...
.........##.......##...#...#...
............#...##....#...#....
........#...#..#...#.##......#.
..#...#.#.........#.#....#.....
..#...#.#..#.......#.#.........
.......#.....#...#.#..###....#.
.#......#.#....#.#.####....#...
.......##..#......#...#......##
#####.....#........#.#.......#.
.....#...#..#...#.#.....#..#...
....#...#....##.....##....#.#..
.#..#......#.####.....#....#..#
...#.......#..#.....##........#
.#.....#.#.#.....##...#..#.....
.............#...#..#.....#....
...#.....##.......#...##..#...#
..#.............#...#..#..##...
...#........#........#...#...#.
##..........#.#.#.............#
....#....#..............#..#...
....#..####..##....#.......#.#.
.#..#.....##....#.#.....#...#..
#............##..#..#.#......#.
....#..........##..#...........
..#.##.#.......#...#.##....#...
....#.#.............#.#.##....#
...#..#.#.#......#..#....#....#
.............#...........#..#..
#.............##.......#..###..
..##....#.#.#...#...#....#...#.
##.......####........##..#.....
.###..#..#..#..#...#.#.........
............#............#.....
#...#.....#.#.##.##...#.......#
#........#....#...#.........#..
#....#.#......#.............#..
....#............#......##...#.
.......#........#..#.......#..#
#.#...#.#.#..#..#........#....#
#.#.##...........#.....#.....#.
.#...##.#..#...................
###...#.#.....................#
.#....##...##.#....#..#........
........###...#.#....##...#..##
...#..#..........###..#.......#
..#..##.............#.....#....
....##..#..............#.......
...##...##.#....#.#...#...#.#.#
..#..........#.....##........#.
.#.#.....#......#..#....#......
...##.#....#.......#......#....
...##..#........##......#..##..
#..###...#...........#.#.......
..##...#...#.#.#...#.#.....#...
..#.....##.#....#.....#..#.....
..#.#......#.......#...........
....##......##.............#..#
.......#.........##...#..#...#.
.#........#.##.#.....#.#.......
#..#...#..#.....##...........#.
..##..............#....#.......
.#..#....#.#...........#..##.#.
#....#..###..........#...#.....
.......#...##........#.#...###.
....#..#......##......#.....#..
.#..##..#..#......#......#.##..
....#.#...........#..#.#.##.#..
.....#......#.....#.....#..#...
..........#...........#...##...
#..#.#.#..#....#.....##......##
..#.....#.....#................
...#.#..##.........#..#..##....
.#.....##..#.#.#.#.....#.......
...........##...#..............
...#.#....##..#.............###
...#.#...........#.........#...
#.....#.....##..#.#.#.#....#...
##..................##.##......
......#.....#....#.....#..#...#
.............#.......#....#..##
.#..#.##..#..#.........##...#..
..#.#....#........#....#....##.
.#.#.#.#.#.......#.......#..#..
#.....#..##..#.........#.......
.............#.#..............#
.........#......#....#.#......#
.........#.#...##..#.#.........
...........#..........#........
.......#...#...#......#..#.....
#.....#...............#.....#.#
..#....#..........#.#...#..#...
#....##..#..#.....#.#..#.#.....
.#....#..###............##.....
......#.##...........#....#..#.
...#........##....#...#...#....
..#.#.#.....#..#.#..........##.
..................#...........#
##........#.#......#.#.......#.
......#..#.............##......
.#..###..#...###......#....#..#
..#...........#...#...##..#...#
..#..........#..............#..
.....#.........................
..#.#..##...........##...#.....
...........#......##.....##....
......#.......#................
.........#.......#.#...........
#......#...#........##.....##..
...#.....#....#..#.....#.......
....#.#......#...#..#.##.##...#
..#..#.#.....#...#...........#.
.#....##.####.....#..........##
...##.##.....##..###...#.......
.......#.#...#....#.......#..#.
.#..#.###.#.............#......
.###.........####..#...........
#..#.#.###.....#.......#.......
.#.....#.....#.....#.........#.
..#...#......#.......##.###....
.......##.............##.#.....
.....................#.....##.#
##.#...#........#..##........#.
...#..........#.#.#..#......###
.#....#.#.#..........##........
....#....####....#.#....#..#.#.
..#.........#....##..........##
...##.#.......##.#.......#.#...
........#..#......#...#.#.....#
.....##............#.#.......#.
.........##...##..#.....#..#...
#...#....#........#...#....##.#
..#.....#..........#...##.....#
.##..#.........#...........#...
.....##.#.#.#.#..#...#.....#.#.
.#..#..##.........#.......#...#
#....#.....#...#....#.........#
...#..#.......#.........#......
.#....##..#......##.#.#......#.
....##.##...........#...#......
..#.#....#.##...#......#.......
...#........#.............#....
...##....................#.###.
.#.......#.........#......##...
....#..#..............#....#...
....##.#............#..........
.#...#....#...##..........#....
....#............#.....#.......
.......#........#..............
....#.#....#.#..#..#...........
......................#.#......
#......##.....#..#.......##....
...#........#........#.#...##..
##.#....##....#................
#..#....#..............#.##....
......#........#...........#...
#....##.##...#...#..#...##.....
............#............#..#..
#....#...#..#..#.#...........#.
.......#..........#..........##
.....#......#....##.#..........
.#....#....#....#....#..#...#..
.....###....#...#.#.#........#.
.......#...#..........##..#...#
..##........................##.
.....#....#..............#....#")
