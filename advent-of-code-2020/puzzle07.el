;;;;;;;;;;;
;; Day 7 ;;
;;;;;;;;;;;

(defun day-07-parse-input (input)
  "Parse the list of rules in INPUT into an alist whose CARs
represent enclosing bags and CDRs are bags contained within
those."
  (labels ((parse-container (x)
			    (string-join
			     (butlast (split-string x)) "-"))
	   (parse-contents (x)
			   (unless (string= x "no other bags.")
			     (mapcar (lambda (item)
				       (string-join
					(cdr (butlast (split-string item))) "-"))
				     (split-string x ", ")))))
      (mapcar (lambda (line)
	     (destructuring-bind (container contents)
		 (split-string line " contain ")
	       (cons (parse-container container)
		     (parse-contents contents))))
	      (split-string input "\n"))))

(defun day-07-problem-1 (bag input)
  "Look recursively across all bag specifications and count all
unique bag types from INPUT that can eventually contain BAG, with
the stipulation that it has to be inside at least one other
bag (i.e. it cannot be considered by itself)."
  (let ((alist (day-07-parse-input input)))
    (labels ((contains (bag contents)
		       (or (member bag contents)
			   (some (lambda (c)
				   (contains bag (cdr (assoc c alist))))
				 contents))))
      (loop
       for (container . contents) in alist
       if (contains bag contents)
       count container))))

(defun day-07-problem-2 (bag input)
  ""
  (let ((alist (day-07-parse-input input)))
    (labels)))

(defvar *day-07-test-input*
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(defvar *day-07-input*
  "shiny purple bags contain 2 pale blue bags, 1 wavy fuchsia bag, 5 pale salmon bags.
bright gray bags contain 4 dotted coral bags.
clear chartreuse bags contain 3 dark magenta bags, 3 dull gray bags, 4 dark silver bags.
posh maroon bags contain 5 bright brown bags, 3 posh brown bags, 4 clear bronze bags.
wavy plum bags contain 2 dull turquoise bags, 2 dotted yellow bags, 2 drab silver bags, 5 wavy violet bags.
bright plum bags contain 5 clear silver bags, 5 striped coral bags.
light coral bags contain 3 striped bronze bags, 1 bright turquoise bag.
muted coral bags contain 1 dim blue bag.
clear tan bags contain 2 light turquoise bags, 2 faded fuchsia bags, 5 posh orange bags.
light maroon bags contain 4 dotted purple bags.
dull orange bags contain 4 dull gray bags.
vibrant gold bags contain 2 striped coral bags, 3 light beige bags.
light fuchsia bags contain 2 striped cyan bags, 1 light magenta bag, 2 dim plum bags.
bright teal bags contain 2 drab fuchsia bags, 5 light silver bags, 1 pale orange bag.
clear brown bags contain 3 dull yellow bags, 3 mirrored violet bags, 1 plaid tan bag.
posh brown bags contain 2 posh purple bags, 2 pale indigo bags, 1 wavy green bag.
dotted violet bags contain 1 shiny violet bag, 3 striped crimson bags.
dark tomato bags contain 4 pale indigo bags, 4 dull white bags, 5 shiny gray bags, 3 light blue bags.
drab red bags contain 1 dotted beige bag, 1 shiny white bag, 5 bright teal bags, 4 drab indigo bags.
dotted red bags contain 4 vibrant fuchsia bags, 5 clear chartreuse bags.
clear magenta bags contain 2 striped plum bags, 3 wavy silver bags.
vibrant turquoise bags contain 1 bright purple bag.
wavy beige bags contain 2 pale orange bags, 1 dull olive bag.
mirrored crimson bags contain 2 wavy gray bags, 1 bright gray bag, 3 posh green bags, 5 wavy magenta bags.
dark green bags contain 1 dotted teal bag, 5 light lime bags, 2 mirrored tan bags.
light salmon bags contain 1 dull violet bag, 2 muted chartreuse bags, 4 clear black bags, 3 dark plum bags.
mirrored indigo bags contain 4 clear beige bags, 5 posh purple bags, 5 pale tan bags.
dim turquoise bags contain 1 muted cyan bag.
striped tomato bags contain 5 shiny olive bags, 2 dark lime bags, 2 wavy lavender bags, 3 striped tan bags.
striped white bags contain 3 dark violet bags, 2 posh green bags, 2 plaid white bags.
drab lavender bags contain 2 light plum bags, 3 vibrant purple bags, 4 light turquoise bags.
muted olive bags contain 4 clear gold bags, 1 dim plum bag, 5 vibrant silver bags.
posh lime bags contain 4 vibrant gray bags, 1 striped magenta bag, 5 drab tomato bags, 2 mirrored tomato bags.
clear aqua bags contain 4 posh green bags, 2 dim teal bags, 1 faded teal bag, 3 bright aqua bags.
vibrant aqua bags contain 4 striped plum bags, 3 dull gray bags, 1 muted tomato bag.
wavy gold bags contain 1 dotted violet bag, 2 dim turquoise bags, 3 light orange bags.
pale aqua bags contain 3 muted lavender bags.
muted plum bags contain 1 dim silver bag.
dull olive bags contain 3 striped fuchsia bags, 1 vibrant aqua bag, 2 mirrored black bags, 1 shiny gray bag.
faded black bags contain 4 light purple bags.
posh fuchsia bags contain 4 striped black bags, 1 dark silver bag, 4 dim beige bags.
posh cyan bags contain 1 clear orange bag, 4 dull plum bags, 5 clear chartreuse bags, 5 pale silver bags.
light tomato bags contain 2 dotted orange bags, 2 dotted maroon bags, 2 dim silver bags.
clear orange bags contain 4 pale silver bags.
dim tan bags contain 3 posh orange bags.
striped indigo bags contain 4 dull plum bags, 1 clear magenta bag.
shiny lime bags contain 2 dull yellow bags.
light turquoise bags contain 3 striped lime bags, 1 striped plum bag, 5 dim salmon bags, 5 wavy maroon bags.
shiny aqua bags contain 5 dim cyan bags, 1 bright tomato bag.
shiny silver bags contain 3 drab lime bags, 4 drab green bags, 2 bright beige bags, 1 muted gray bag.
plaid aqua bags contain 1 dull plum bag, 5 posh orange bags, 1 dim indigo bag.
mirrored silver bags contain 2 dark white bags, 2 vibrant yellow bags, 5 plaid crimson bags, 1 vibrant orange bag.
dark black bags contain 4 wavy salmon bags.
dim tomato bags contain 1 clear brown bag, 2 dark violet bags.
wavy teal bags contain 4 pale coral bags, 4 vibrant lime bags.
pale brown bags contain 2 drab salmon bags, 2 light silver bags, 2 plaid orange bags, 1 dull plum bag.
wavy silver bags contain 5 clear teal bags.
striped plum bags contain 2 light blue bags, 4 dull gray bags, 2 wavy lavender bags.
shiny fuchsia bags contain 2 muted plum bags, 3 bright bronze bags, 5 striped tan bags.
muted turquoise bags contain 2 dark lime bags, 5 clear orange bags, 4 clear chartreuse bags.
posh black bags contain 2 posh tomato bags, 5 posh red bags.
muted cyan bags contain 1 drab bronze bag, 4 pale silver bags, 4 dotted brown bags.
dark violet bags contain 5 wavy gray bags, 5 dark white bags, 2 mirrored teal bags.
drab orange bags contain 3 vibrant purple bags, 2 clear indigo bags, 4 clear gold bags, 2 striped tomato bags.
light aqua bags contain 5 light plum bags, 5 clear gold bags.
plaid green bags contain 4 dim teal bags, 5 striped lavender bags, 1 mirrored yellow bag, 3 bright beige bags.
mirrored chartreuse bags contain 4 faded crimson bags, 3 dim salmon bags.
faded turquoise bags contain 1 dim tomato bag, 3 clear violet bags, 3 bright tan bags, 4 posh bronze bags.
pale crimson bags contain 1 drab tomato bag, 5 striped purple bags, 2 wavy brown bags, 3 dotted white bags.
posh aqua bags contain 5 mirrored plum bags.
striped fuchsia bags contain 1 mirrored yellow bag, 5 faded black bags.
clear green bags contain 4 drab teal bags, 1 shiny orange bag, 5 muted salmon bags, 4 dark magenta bags.
bright white bags contain 2 shiny green bags, 2 muted turquoise bags, 5 striped plum bags, 5 vibrant aqua bags.
dark bronze bags contain 4 shiny purple bags.
pale lime bags contain 4 dotted aqua bags, 5 wavy silver bags.
posh turquoise bags contain 2 muted beige bags, 5 light coral bags, 2 posh purple bags, 5 striped gold bags.
drab indigo bags contain 2 shiny magenta bags, 2 vibrant yellow bags.
striped violet bags contain 4 muted magenta bags, 5 bright green bags, 1 light lavender bag.
dull black bags contain 5 dull orange bags, 5 light magenta bags, 1 dull brown bag.
clear cyan bags contain 5 dull fuchsia bags.
dim lavender bags contain 4 clear teal bags, 5 striped magenta bags, 5 mirrored violet bags, 3 dotted magenta bags.
mirrored teal bags contain 1 vibrant yellow bag, 1 dim plum bag, 1 drab fuchsia bag, 1 shiny yellow bag.
dotted yellow bags contain 4 clear coral bags, 3 pale chartreuse bags.
light plum bags contain 5 light blue bags, 1 wavy brown bag.
faded crimson bags contain 2 posh aqua bags.
dull crimson bags contain 4 muted lavender bags, 1 dark lime bag, 3 clear indigo bags, 5 wavy teal bags.
pale olive bags contain 1 plaid beige bag, 2 dark cyan bags, 3 mirrored blue bags, 5 dull lime bags.
dull silver bags contain 1 faded purple bag.
faded blue bags contain 4 dull olive bags, 1 wavy maroon bag, 1 muted turquoise bag, 5 mirrored violet bags.
shiny beige bags contain 1 wavy brown bag, 5 muted magenta bags, 1 clear yellow bag, 3 muted salmon bags.
bright beige bags contain 4 posh cyan bags.
dim indigo bags contain 1 light plum bag, 1 drab olive bag.
wavy violet bags contain 3 light lavender bags, 3 pale salmon bags, 2 pale blue bags, 5 vibrant white bags.
faded red bags contain 1 clear yellow bag, 4 dotted maroon bags, 4 wavy aqua bags, 4 bright teal bags.
mirrored red bags contain 4 bright maroon bags.
drab maroon bags contain 2 dim beige bags, 5 drab tomato bags, 2 clear coral bags.
bright maroon bags contain 2 dull gray bags.
shiny lavender bags contain 1 dark orange bag.
mirrored beige bags contain 5 pale white bags, 1 posh plum bag.
muted black bags contain 3 light lavender bags, 1 striped tomato bag, 2 posh green bags.
vibrant coral bags contain 4 shiny red bags.
muted bronze bags contain 2 dotted turquoise bags.
drab blue bags contain 3 mirrored purple bags, 4 clear bronze bags.
shiny salmon bags contain 4 dim beige bags, 2 faded plum bags.
dull tomato bags contain 3 dark chartreuse bags, 5 muted chartreuse bags, 3 mirrored purple bags.
vibrant plum bags contain 1 faded lime bag, 3 mirrored gold bags, 4 mirrored aqua bags.
dark blue bags contain 3 dull salmon bags, 1 dim black bag, 1 wavy gray bag, 3 muted aqua bags.
posh beige bags contain 4 light lime bags, 5 pale crimson bags, 4 clear lavender bags, 2 dotted chartreuse bags.
dark indigo bags contain 5 wavy salmon bags.
drab turquoise bags contain 5 muted red bags.
drab silver bags contain 3 plaid white bags, 3 clear brown bags.
clear indigo bags contain 4 wavy gray bags, 5 dim plum bags.
dotted cyan bags contain 4 striped lime bags, 2 bright tomato bags.
faded gray bags contain 1 dim maroon bag, 4 muted brown bags, 2 clear plum bags, 1 plaid olive bag.
dotted turquoise bags contain 1 mirrored purple bag, 2 striped salmon bags.
pale tan bags contain 4 pale orange bags.
clear tomato bags contain 4 clear turquoise bags, 1 bright tan bag, 2 shiny coral bags, 3 posh crimson bags.
dark orange bags contain 4 vibrant yellow bags, 5 vibrant lavender bags.
striped cyan bags contain 3 vibrant yellow bags, 5 dull tomato bags, 4 dotted black bags, 5 striped plum bags.
drab black bags contain 3 muted salmon bags, 5 faded cyan bags.
mirrored green bags contain 2 dark brown bags, 2 shiny gold bags, 1 clear plum bag, 5 faded teal bags.
dotted orange bags contain 5 wavy brown bags.
vibrant magenta bags contain 5 muted tomato bags.
light indigo bags contain 4 light olive bags, 1 dull maroon bag.
dotted black bags contain 1 posh orange bag, 4 pale silver bags, 4 clear plum bags, 1 dotted white bag.
vibrant lime bags contain 5 light teal bags, 2 wavy lime bags.
faded yellow bags contain 1 faded black bag, 3 wavy fuchsia bags, 4 light olive bags, 3 wavy indigo bags.
dim chartreuse bags contain 2 bright gray bags.
mirrored lime bags contain 1 bright teal bag, 5 mirrored teal bags, 3 muted lavender bags.
muted brown bags contain 4 plaid aqua bags.
shiny red bags contain 1 clear orange bag.
drab magenta bags contain 2 light lavender bags, 4 shiny chartreuse bags, 3 wavy violet bags.
vibrant tomato bags contain 5 pale violet bags, 3 clear purple bags, 2 light red bags.
mirrored plum bags contain 1 dim white bag, 1 striped lavender bag.
dark beige bags contain 3 wavy gray bags.
dim plum bags contain 5 mirrored yellow bags, 4 posh cyan bags.
faded purple bags contain 3 posh purple bags, 5 dull bronze bags, 1 striped plum bag.
vibrant olive bags contain 1 muted chartreuse bag, 2 striped blue bags, 1 shiny maroon bag.
dull cyan bags contain 2 faded violet bags, 2 pale olive bags, 3 dull yellow bags, 3 drab olive bags.
wavy magenta bags contain 5 dull bronze bags, 1 vibrant cyan bag, 1 clear plum bag.
plaid lime bags contain 1 dull maroon bag, 1 pale teal bag, 3 pale orange bags, 1 shiny teal bag.
vibrant maroon bags contain 5 shiny gray bags, 3 pale salmon bags.
faded tomato bags contain 2 pale orange bags, 5 dim tomato bags, 5 plaid lavender bags, 5 pale salmon bags.
wavy lime bags contain 2 faded salmon bags, 3 muted olive bags, 2 dotted coral bags.
shiny gold bags contain 5 drab olive bags, 4 pale green bags.
mirrored black bags contain 1 faded fuchsia bag, 5 dull yellow bags.
posh olive bags contain 5 dull plum bags, 2 wavy aqua bags, 5 posh brown bags.
posh red bags contain 1 dim salmon bag, 4 bright gray bags.
pale purple bags contain 1 drab indigo bag, 3 mirrored silver bags.
dotted coral bags contain 4 dim indigo bags, 5 shiny yellow bags, 3 dim plum bags, 1 light silver bag.
dim beige bags contain 1 dim indigo bag.
dark coral bags contain 4 bright lime bags, 1 muted white bag, 2 shiny olive bags.
striped silver bags contain 3 light white bags.
drab bronze bags contain 4 striped tomato bags, 1 dim coral bag, 2 clear brown bags.
vibrant cyan bags contain 1 muted green bag, 4 vibrant silver bags, 4 dotted white bags, 4 muted magenta bags.
drab gray bags contain 3 dim indigo bags, 2 pale tomato bags, 1 dark fuchsia bag.
dull chartreuse bags contain 5 dark brown bags, 5 dull blue bags, 3 bright cyan bags, 2 plaid indigo bags.
dim crimson bags contain 3 wavy turquoise bags.
posh crimson bags contain 1 bright silver bag, 4 dark purple bags, 1 bright black bag, 5 dotted salmon bags.
plaid turquoise bags contain 1 striped gold bag.
shiny tomato bags contain 4 vibrant gold bags, 5 vibrant teal bags, 2 vibrant yellow bags, 1 faded silver bag.
mirrored white bags contain 3 plaid crimson bags.
vibrant tan bags contain 1 posh red bag, 1 muted purple bag, 4 clear teal bags.
dark lavender bags contain 3 dull yellow bags, 5 clear yellow bags, 5 clear plum bags, 2 posh orange bags.
dark purple bags contain 2 light magenta bags, 1 clear violet bag.
muted indigo bags contain 5 mirrored coral bags, 1 posh purple bag.
faded aqua bags contain 5 faded fuchsia bags, 2 dark bronze bags, 4 vibrant white bags.
dark silver bags contain no other bags.
bright black bags contain 4 dotted fuchsia bags, 1 light silver bag, 4 wavy white bags.
pale teal bags contain 4 plaid lavender bags, 3 dim red bags, 2 vibrant yellow bags, 2 clear bronze bags.
drab lime bags contain 3 dull teal bags.
shiny yellow bags contain 1 striped plum bag, 5 dull plum bags, 5 dark magenta bags, 1 vibrant silver bag.
dull salmon bags contain 2 dotted magenta bags, 2 vibrant fuchsia bags, 1 wavy beige bag.
muted gray bags contain 1 drab indigo bag, 2 vibrant brown bags.
clear turquoise bags contain 1 drab silver bag.
drab white bags contain 1 posh brown bag, 2 muted green bags, 2 vibrant yellow bags, 2 dotted white bags.
light silver bags contain 4 dark silver bags, 5 faded olive bags.
plaid coral bags contain 5 vibrant tomato bags, 5 striped tan bags.
light gray bags contain 4 dark blue bags, 5 dull violet bags, 3 striped lime bags, 3 wavy magenta bags.
striped maroon bags contain 2 light coral bags.
striped green bags contain 1 shiny teal bag.
mirrored cyan bags contain 2 vibrant white bags.
mirrored fuchsia bags contain 2 drab blue bags, 2 wavy beige bags.
clear yellow bags contain no other bags.
faded silver bags contain 1 muted magenta bag, 1 mirrored black bag, 5 light purple bags.
drab chartreuse bags contain 4 muted red bags, 2 plaid lavender bags.
muted beige bags contain 4 dark tomato bags, 2 wavy brown bags, 1 wavy beige bag.
pale maroon bags contain 3 muted tomato bags, 1 shiny maroon bag.
pale turquoise bags contain 4 muted beige bags, 2 pale beige bags.
clear olive bags contain 1 clear yellow bag, 2 clear chartreuse bags.
muted tan bags contain 5 dotted bronze bags, 1 light green bag, 2 pale orange bags, 1 plaid tan bag.
vibrant fuchsia bags contain 2 dull bronze bags, 4 plaid lavender bags, 3 clear bronze bags, 3 dull blue bags.
striped magenta bags contain 1 mirrored violet bag, 5 plaid aqua bags, 5 light plum bags.
dim gray bags contain 3 mirrored blue bags, 2 vibrant crimson bags, 1 dull gold bag.
muted purple bags contain 3 plaid salmon bags.
drab plum bags contain 4 dark tomato bags, 1 muted white bag, 2 plaid magenta bags, 5 mirrored blue bags.
muted white bags contain 3 light silver bags, 2 striped magenta bags, 4 shiny indigo bags.
shiny cyan bags contain 4 vibrant fuchsia bags, 1 striped cyan bag, 5 pale orange bags.
pale tomato bags contain 4 bright maroon bags, 4 faded chartreuse bags, 3 shiny black bags, 2 muted green bags.
wavy gray bags contain 4 vibrant silver bags.
wavy lavender bags contain 1 dull bronze bag, 5 dark silver bags, 5 light blue bags.
plaid chartreuse bags contain 4 dotted tan bags, 1 clear silver bag, 2 dotted purple bags, 3 drab green bags.
faded olive bags contain no other bags.
drab aqua bags contain 1 vibrant indigo bag, 1 mirrored maroon bag, 4 pale silver bags.
dotted chartreuse bags contain 1 dotted aqua bag, 3 dark red bags.
muted crimson bags contain 3 dull gold bags, 1 light cyan bag.
light orange bags contain 2 drab fuchsia bags, 2 faded black bags, 5 faded green bags.
drab teal bags contain 1 clear tan bag, 5 mirrored green bags.
light chartreuse bags contain 5 bright tomato bags, 4 clear coral bags.
mirrored blue bags contain 3 dark violet bags, 2 dim silver bags, 2 vibrant white bags, 2 bright gray bags.
wavy salmon bags contain 2 muted fuchsia bags, 5 vibrant lavender bags, 1 clear teal bag.
pale black bags contain 3 dull fuchsia bags, 5 light indigo bags, 2 muted aqua bags.
wavy white bags contain 1 wavy gray bag.
dotted indigo bags contain 3 clear bronze bags, 1 pale green bag, 1 faded fuchsia bag.
wavy tomato bags contain 2 light black bags, 2 pale tan bags.
clear gold bags contain 4 drab fuchsia bags.
vibrant green bags contain 4 clear gray bags, 4 vibrant lavender bags, 4 pale beige bags.
light crimson bags contain 4 light turquoise bags.
posh silver bags contain 4 pale silver bags.
dotted green bags contain 3 pale turquoise bags, 1 wavy coral bag.
clear coral bags contain 1 posh purple bag.
clear fuchsia bags contain 1 dull teal bag.
vibrant blue bags contain 5 faded olive bags, 1 dotted olive bag.
dull bronze bags contain no other bags.
plaid plum bags contain 5 light teal bags, 5 bright fuchsia bags, 4 dark cyan bags, 2 striped olive bags.
muted blue bags contain 2 bright green bags, 4 clear teal bags, 5 dotted blue bags, 3 shiny brown bags.
dotted salmon bags contain 3 dull olive bags, 3 vibrant gray bags, 5 mirrored purple bags.
pale red bags contain 1 posh bronze bag, 1 posh plum bag.
dotted beige bags contain 5 pale violet bags, 5 striped bronze bags.
dark magenta bags contain no other bags.
clear red bags contain 2 faded black bags, 3 striped lavender bags.
dim purple bags contain 2 shiny orange bags.
shiny teal bags contain 5 plaid magenta bags, 2 faded violet bags.
dull tan bags contain 5 dark lime bags, 1 vibrant silver bag, 4 faded brown bags, 3 bright brown bags.
bright yellow bags contain 2 light green bags, 4 muted magenta bags, 5 faded indigo bags, 3 bright beige bags.
clear plum bags contain 5 light purple bags.
pale beige bags contain 5 plaid tan bags, 5 striped plum bags, 3 muted brown bags.
vibrant beige bags contain 5 wavy violet bags, 5 faded maroon bags.
shiny plum bags contain 4 pale blue bags, 5 dull brown bags, 5 mirrored black bags.
plaid gold bags contain 3 muted magenta bags, 2 clear yellow bags, 3 light blue bags, 1 posh orange bag.
dull lavender bags contain 2 bright aqua bags, 2 plaid plum bags, 3 dark tomato bags.
dark salmon bags contain 3 plaid magenta bags, 4 bright turquoise bags.
posh indigo bags contain 4 vibrant lavender bags, 2 clear bronze bags.
dotted tan bags contain 5 dull gray bags, 4 wavy green bags, 4 vibrant bronze bags.
clear black bags contain 1 shiny crimson bag, 3 shiny beige bags, 4 dotted red bags.
vibrant indigo bags contain 2 clear beige bags, 2 dim salmon bags.
dark brown bags contain 3 dim silver bags.
dim white bags contain 1 clear gold bag.
drab fuchsia bags contain 5 dull bronze bags, 1 clear chartreuse bag.
shiny orange bags contain 5 plaid aqua bags.
striped gray bags contain 2 plaid orange bags.
bright blue bags contain 5 shiny crimson bags, 4 dim coral bags, 2 posh aqua bags, 2 bright orange bags.
striped brown bags contain 5 bright teal bags, 5 striped blue bags, 3 plaid brown bags, 3 drab violet bags.
dim magenta bags contain 1 dark white bag.
posh gray bags contain 2 mirrored silver bags, 4 light cyan bags, 1 posh lime bag.
mirrored bronze bags contain 3 plaid brown bags, 4 wavy lavender bags, 4 vibrant coral bags, 5 striped bronze bags.
light olive bags contain 3 pale indigo bags.
clear beige bags contain 4 striped plum bags, 1 striped fuchsia bag.
mirrored coral bags contain 5 posh maroon bags, 3 dim yellow bags.
wavy tan bags contain 4 dim yellow bags, 3 plaid beige bags.
vibrant brown bags contain 2 dull olive bags, 5 light blue bags, 3 light purple bags.
striped blue bags contain 3 clear gold bags, 2 dull gray bags.
wavy turquoise bags contain 2 vibrant aqua bags, 5 clear purple bags, 5 muted blue bags, 1 muted black bag.
plaid teal bags contain 2 striped magenta bags.
dim blue bags contain 3 wavy aqua bags, 3 plaid salmon bags.
pale chartreuse bags contain 1 faded coral bag.
dark tan bags contain 1 dim olive bag, 5 pale violet bags.
wavy olive bags contain 3 light tomato bags, 4 clear salmon bags.
drab beige bags contain 5 faded tomato bags, 3 dotted indigo bags, 1 posh purple bag.
vibrant salmon bags contain 5 bright green bags, 2 shiny violet bags, 1 dark fuchsia bag.
dull gold bags contain 4 pale maroon bags.
pale gold bags contain 4 dull salmon bags.
clear gray bags contain 1 clear chartreuse bag.
clear purple bags contain 3 clear coral bags, 3 drab lavender bags, 3 clear silver bags.
bright magenta bags contain 4 posh coral bags, 2 striped salmon bags, 5 bright fuchsia bags.
faded lavender bags contain 3 drab olive bags, 5 clear plum bags, 5 light purple bags, 1 plaid gold bag.
dark white bags contain 1 clear gold bag.
bright gold bags contain 2 dotted black bags, 1 dotted indigo bag.
dark gold bags contain 1 dotted fuchsia bag.
plaid black bags contain 2 dotted lavender bags, 1 dim indigo bag, 1 wavy aqua bag, 1 pale white bag.
muted silver bags contain 3 posh crimson bags.
bright red bags contain 3 shiny indigo bags, 1 clear yellow bag.
mirrored orange bags contain 1 dull black bag, 1 faded gold bag, 5 muted yellow bags.
bright chartreuse bags contain 2 drab gray bags, 4 dull green bags, 1 muted plum bag, 3 mirrored turquoise bags.
plaid tan bags contain 4 drab fuchsia bags.
mirrored tan bags contain 5 striped salmon bags, 5 drab crimson bags.
shiny turquoise bags contain 2 dark indigo bags, 1 dotted crimson bag, 1 pale tomato bag.
pale magenta bags contain 3 light yellow bags, 5 bright turquoise bags, 5 striped turquoise bags, 3 dull red bags.
muted maroon bags contain 2 plaid orange bags.
mirrored turquoise bags contain 3 drab gray bags.
clear white bags contain 5 muted turquoise bags.
striped red bags contain 2 muted turquoise bags, 1 wavy cyan bag, 1 faded chartreuse bag, 2 mirrored beige bags.
muted yellow bags contain 4 dim beige bags, 2 muted magenta bags, 3 mirrored aqua bags, 5 striped brown bags.
dotted brown bags contain 3 bright brown bags, 1 posh purple bag.
posh gold bags contain 5 dotted indigo bags, 4 dull chartreuse bags.
shiny magenta bags contain 5 light red bags, 1 dark lime bag.
plaid red bags contain 3 dim maroon bags, 5 posh magenta bags, 1 drab coral bag.
mirrored yellow bags contain 2 pale silver bags, 3 dim salmon bags, 4 bright teal bags, 2 pale green bags.
clear lavender bags contain 1 pale blue bag.
bright lime bags contain 2 clear orange bags.
wavy red bags contain 2 dull teal bags, 3 striped fuchsia bags, 1 dull turquoise bag.
striped tan bags contain 5 light black bags, 4 posh cyan bags.
light blue bags contain no other bags.
muted lavender bags contain 1 muted tomato bag, 3 dim tomato bags, 5 plaid brown bags, 3 muted olive bags.
vibrant purple bags contain 2 muted tomato bags, 4 vibrant silver bags, 3 drab olive bags, 5 dull gray bags.
plaid indigo bags contain 3 drab silver bags, 4 wavy brown bags, 5 dotted gray bags, 3 dim silver bags.
mirrored olive bags contain 3 dark tan bags.
wavy cyan bags contain 4 drab gold bags, 5 clear teal bags.
pale indigo bags contain 2 dull gray bags.
pale violet bags contain 4 dull orange bags.
dim orange bags contain 3 wavy orange bags.
pale plum bags contain 1 dim salmon bag, 1 posh cyan bag, 2 vibrant fuchsia bags.
bright turquoise bags contain 4 wavy fuchsia bags, 3 faded crimson bags, 2 striped lime bags.
dim cyan bags contain 4 striped plum bags, 1 shiny cyan bag, 3 dotted gold bags.
dim fuchsia bags contain 5 shiny cyan bags.
light tan bags contain 2 shiny blue bags, 3 clear beige bags.
faded green bags contain 1 bright teal bag, 3 faded fuchsia bags, 5 light white bags.
plaid violet bags contain 4 muted purple bags, 4 wavy white bags.
wavy chartreuse bags contain 2 vibrant aqua bags, 2 mirrored black bags, 2 striped coral bags, 3 mirrored lime bags.
dull white bags contain 3 light plum bags, 3 vibrant purple bags, 2 vibrant aqua bags, 1 bright teal bag.
dark teal bags contain 2 plaid aqua bags, 2 bright blue bags, 5 dotted coral bags.
plaid maroon bags contain 5 shiny green bags, 4 clear red bags.
dull purple bags contain 2 dotted brown bags, 3 muted tomato bags, 5 vibrant green bags, 4 pale plum bags.
striped aqua bags contain 1 dark tan bag, 2 dull turquoise bags, 4 muted magenta bags.
shiny chartreuse bags contain 2 muted bronze bags, 1 light white bag, 5 light turquoise bags.
faded white bags contain 3 muted red bags.
striped turquoise bags contain 2 posh lavender bags.
bright lavender bags contain 2 wavy fuchsia bags, 4 faded purple bags.
light teal bags contain 4 mirrored yellow bags, 3 dull orange bags, 3 drab green bags.
muted fuchsia bags contain 3 clear gold bags.
faded bronze bags contain 4 muted green bags.
striped teal bags contain 4 muted fuchsia bags, 3 bright aqua bags.
plaid gray bags contain 5 dark lavender bags, 2 bright crimson bags, 3 shiny salmon bags, 5 wavy cyan bags.
dark red bags contain 2 striped salmon bags.
striped purple bags contain 3 shiny crimson bags.
pale blue bags contain 1 drab tan bag, 3 dark chartreuse bags, 2 mirrored gold bags, 3 muted turquoise bags.
plaid bronze bags contain 5 dark olive bags, 5 pale blue bags, 1 posh red bag.
muted chartreuse bags contain 1 posh orange bag, 5 light purple bags, 3 drab olive bags.
muted teal bags contain 1 faded olive bag.
plaid cyan bags contain 4 clear brown bags, 4 shiny yellow bags.
dull brown bags contain 3 plaid tan bags, 5 muted brown bags, 4 dull white bags, 4 pale orange bags.
wavy orange bags contain 2 striped gold bags, 5 drab crimson bags, 5 muted chartreuse bags, 1 striped coral bag.
pale gray bags contain 3 vibrant gold bags.
clear crimson bags contain 1 wavy turquoise bag, 5 drab white bags, 3 wavy purple bags.
wavy brown bags contain 5 drab fuchsia bags, 1 dull plum bag.
dull gray bags contain 4 clear yellow bags, 3 vibrant silver bags.
mirrored aqua bags contain 2 plaid crimson bags, 5 plaid white bags.
vibrant bronze bags contain 1 shiny gray bag, 4 pale beige bags, 1 muted chartreuse bag, 3 striped cyan bags.
light magenta bags contain 3 striped crimson bags, 5 vibrant bronze bags, 3 faded gold bags.
posh green bags contain 2 faded chartreuse bags.
light beige bags contain 4 wavy lavender bags, 3 light white bags.
dotted tomato bags contain 1 plaid violet bag, 2 striped purple bags.
faded maroon bags contain 1 light plum bag.
posh orange bags contain 4 pale silver bags, 5 wavy lavender bags, 2 faded olive bags, 5 striped plum bags.
dull plum bags contain 3 muted tomato bags, 5 clear yellow bags.
plaid blue bags contain 2 dark indigo bags.
dotted gold bags contain 2 shiny plum bags, 4 clear indigo bags, 5 dull turquoise bags, 5 clear teal bags.
posh salmon bags contain 2 pale purple bags, 3 dim chartreuse bags.
clear bronze bags contain 3 muted tomato bags, 1 wavy brown bag, 4 dim indigo bags.
dim red bags contain 2 dim white bags, 3 vibrant crimson bags, 1 mirrored violet bag, 5 striped brown bags.
posh lavender bags contain 5 vibrant indigo bags, 3 plaid olive bags, 1 striped silver bag.
dark chartreuse bags contain 1 clear yellow bag.
light lavender bags contain 4 dotted gold bags, 2 wavy silver bags, 2 shiny gray bags, 3 dull yellow bags.
bright cyan bags contain 5 dark blue bags, 3 dim magenta bags, 5 dark white bags.
posh white bags contain 3 pale salmon bags, 1 wavy tomato bag.
dim yellow bags contain 1 dim tomato bag.
wavy green bags contain 2 dotted white bags, 5 posh orange bags, 5 clear gold bags, 4 shiny gold bags.
drab gold bags contain 1 shiny violet bag, 5 wavy lavender bags.
light brown bags contain 5 wavy purple bags.
plaid brown bags contain 2 pale plum bags.
drab purple bags contain 5 clear red bags, 5 drab plum bags, 2 dark violet bags, 1 dark lime bag.
light black bags contain 2 light plum bags.
light violet bags contain 4 posh orange bags, 5 mirrored cyan bags.
muted green bags contain 5 pale orange bags, 5 dark magenta bags, 1 drab fuchsia bag.
drab olive bags contain 2 dull gray bags.
shiny brown bags contain 1 striped teal bag, 4 light blue bags, 2 dim coral bags, 1 plaid brown bag.
drab tan bags contain 4 clear gold bags, 3 mirrored black bags.
bright fuchsia bags contain 3 faded blue bags, 5 striped plum bags, 4 dark lavender bags, 4 muted aqua bags.
shiny tan bags contain 3 plaid aqua bags.
striped bronze bags contain 1 mirrored aqua bag.
plaid orange bags contain 5 muted magenta bags, 4 clear teal bags.
dull maroon bags contain 5 vibrant purple bags, 5 mirrored aqua bags, 5 wavy red bags.
vibrant teal bags contain 1 muted tomato bag, 1 faded violet bag.
wavy yellow bags contain 5 wavy violet bags, 3 mirrored fuchsia bags, 1 bright violet bag.
dull coral bags contain 5 dark magenta bags, 3 clear indigo bags, 1 clear olive bag, 3 mirrored tomato bags.
dull aqua bags contain 1 pale coral bag, 5 muted green bags, 2 mirrored fuchsia bags, 1 vibrant tomato bag.
vibrant gray bags contain 4 drab chartreuse bags, 3 bright brown bags, 2 vibrant silver bags.
mirrored magenta bags contain 1 wavy lavender bag, 4 vibrant olive bags, 4 faded indigo bags, 2 posh green bags.
dotted crimson bags contain 4 dull brown bags, 1 drab fuchsia bag, 3 pale tomato bags, 2 dotted orange bags.
dull lime bags contain 5 plaid beige bags.
light green bags contain 5 posh red bags, 5 pale blue bags, 4 striped lavender bags.
dotted lavender bags contain 2 muted turquoise bags, 4 clear purple bags, 5 dim salmon bags, 4 clear yellow bags.
posh purple bags contain 2 dull white bags, 2 clear gray bags, 4 plaid gold bags.
clear silver bags contain 5 plaid olive bags.
striped olive bags contain 4 striped lavender bags, 3 muted purple bags.
plaid lavender bags contain 2 mirrored yellow bags, 2 drab crimson bags, 3 light blue bags, 3 drab tan bags.
bright crimson bags contain 4 drab olive bags.
shiny crimson bags contain 2 dim coral bags, 1 plaid white bag, 4 wavy brown bags.
dotted plum bags contain 4 drab fuchsia bags.
bright tan bags contain 5 dark lavender bags.
muted violet bags contain 2 drab white bags.
shiny black bags contain 4 wavy green bags, 1 dull white bag, 2 vibrant cyan bags.
dull teal bags contain 5 pale yellow bags.
dark aqua bags contain 3 striped tan bags.
shiny gray bags contain 2 pale orange bags, 2 faded olive bags, 3 dim salmon bags.
dotted gray bags contain 5 bright violet bags, 2 shiny cyan bags, 3 mirrored yellow bags.
muted orange bags contain 3 plaid salmon bags.
bright orange bags contain 2 shiny green bags, 4 dark violet bags, 5 clear gold bags, 2 clear yellow bags.
striped orange bags contain 1 drab silver bag, 2 vibrant silver bags.
posh tomato bags contain 2 light plum bags, 2 faded fuchsia bags, 4 mirrored cyan bags.
wavy coral bags contain 4 bright blue bags, 5 mirrored yellow bags, 4 drab aqua bags, 5 pale coral bags.
wavy maroon bags contain 4 plaid tan bags.
faded gold bags contain 1 shiny gray bag, 1 light turquoise bag.
faded salmon bags contain 5 clear violet bags, 5 faded black bags, 1 striped lime bag, 3 shiny olive bags.
dark plum bags contain 5 bright fuchsia bags, 5 faded green bags, 3 mirrored blue bags, 5 pale red bags.
plaid crimson bags contain 5 mirrored purple bags.
dotted lime bags contain 3 light silver bags.
bright indigo bags contain 1 dull coral bag, 4 faded brown bags, 3 muted fuchsia bags, 3 wavy maroon bags.
posh yellow bags contain 5 dark indigo bags, 3 light tan bags.
pale silver bags contain no other bags.
dim salmon bags contain 3 dull bronze bags, 3 clear yellow bags.
bright green bags contain 3 drab fuchsia bags.
bright bronze bags contain 1 clear blue bag, 1 mirrored white bag, 4 drab plum bags.
faded chartreuse bags contain 2 clear brown bags, 2 drab coral bags.
pale fuchsia bags contain 2 plaid silver bags.
dim aqua bags contain 1 striped silver bag, 3 mirrored tan bags, 4 dotted violet bags, 3 dotted black bags.
drab tomato bags contain 3 dim chartreuse bags, 2 mirrored violet bags.
wavy indigo bags contain 3 faded plum bags, 3 bright turquoise bags.
drab coral bags contain 2 plaid aqua bags, 2 dim plum bags, 3 light plum bags.
plaid fuchsia bags contain 1 dark silver bag, 4 clear gold bags, 3 dim white bags, 3 dotted white bags.
muted lime bags contain 3 pale silver bags.
vibrant red bags contain 4 drab plum bags, 3 pale yellow bags, 5 posh cyan bags, 5 light teal bags.
plaid purple bags contain 4 dim green bags.
dim bronze bags contain 5 dim salmon bags, 4 pale white bags, 5 vibrant cyan bags.
dotted maroon bags contain 1 mirrored teal bag, 3 dark lime bags, 2 pale yellow bags.
pale cyan bags contain 4 dull salmon bags, 4 bright bronze bags.
drab crimson bags contain 4 plaid aqua bags, 2 dotted coral bags, 3 muted green bags, 1 dull white bag.
faded lime bags contain 3 posh blue bags.
pale yellow bags contain 1 mirrored gold bag, 3 faded brown bags, 3 pale silver bags, 4 light black bags.
light bronze bags contain 2 dark purple bags, 1 faded maroon bag, 5 faded purple bags.
dotted white bags contain 3 vibrant silver bags, 2 faded olive bags.
dark maroon bags contain 2 dull silver bags, 3 dark blue bags, 3 drab turquoise bags, 4 vibrant olive bags.
dim brown bags contain 3 faded green bags, 3 muted tomato bags.
dull indigo bags contain 3 faded fuchsia bags.
plaid olive bags contain 4 mirrored gold bags, 4 dim indigo bags, 2 wavy gray bags.
wavy blue bags contain 4 bright purple bags, 4 vibrant gold bags, 4 light blue bags, 4 dotted indigo bags.
light gold bags contain 2 dim lime bags, 2 dim yellow bags.
shiny maroon bags contain 5 dull white bags, 4 shiny red bags, 2 clear bronze bags.
light white bags contain 2 dark magenta bags.
dark olive bags contain 5 plaid brown bags, 3 muted fuchsia bags, 1 bright orange bag, 3 dotted red bags.
drab violet bags contain 3 posh orange bags.
striped crimson bags contain 4 mirrored yellow bags, 5 light plum bags.
vibrant orange bags contain 4 pale silver bags, 2 plaid orange bags, 2 posh silver bags, 4 muted plum bags.
wavy crimson bags contain 1 faded olive bag, 4 vibrant yellow bags, 5 vibrant white bags, 3 dotted red bags.
pale bronze bags contain 1 mirrored maroon bag, 5 drab salmon bags.
dark gray bags contain 4 dim fuchsia bags.
light lime bags contain 5 dull plum bags, 2 dotted violet bags, 2 dull turquoise bags.
shiny blue bags contain 2 clear coral bags, 4 vibrant bronze bags.
dull magenta bags contain 2 mirrored aqua bags.
vibrant lavender bags contain 3 vibrant fuchsia bags, 3 light red bags, 2 vibrant cyan bags.
posh tan bags contain 5 drab blue bags, 2 bright magenta bags, 4 shiny chartreuse bags.
plaid salmon bags contain 2 dull violet bags.
posh bronze bags contain 2 muted plum bags, 5 faded fuchsia bags, 4 bright beige bags.
posh teal bags contain 4 drab indigo bags, 1 posh black bag, 1 dark crimson bag, 1 shiny lavender bag.
striped coral bags contain 4 dim lime bags, 3 light plum bags, 5 dull bronze bags.
faded plum bags contain 2 muted magenta bags, 3 shiny cyan bags.
striped lime bags contain 1 vibrant silver bag, 4 drab fuchsia bags.
vibrant chartreuse bags contain 5 dotted olive bags.
drab green bags contain 1 drab olive bag.
pale salmon bags contain 5 posh purple bags, 3 plaid tan bags, 2 vibrant white bags.
dotted teal bags contain 1 dull yellow bag, 2 mirrored tomato bags, 1 dotted blue bag, 1 dim olive bag.
dim black bags contain 3 posh purple bags, 5 dotted indigo bags, 1 faded fuchsia bag.
faded coral bags contain 1 dotted white bag, 4 shiny gray bags.
striped gold bags contain 1 posh tomato bag, 5 vibrant fuchsia bags, 3 dim chartreuse bags.
drab yellow bags contain 5 dark beige bags, 2 dull gray bags.
striped lavender bags contain 1 dark fuchsia bag, 5 striped magenta bags, 5 muted green bags.
faded brown bags contain 3 posh red bags, 2 plaid tan bags.
faded orange bags contain 3 muted white bags, 1 plaid beige bag, 2 clear gray bags, 2 wavy salmon bags.
shiny violet bags contain 3 posh cyan bags.
bright coral bags contain 3 plaid silver bags, 3 light gold bags.
dim silver bags contain 5 dim indigo bags, 3 wavy maroon bags, 3 mirrored yellow bags.
muted gold bags contain 1 vibrant cyan bag, 3 muted chartreuse bags, 2 faded olive bags.
bright silver bags contain 3 posh aqua bags.
dotted blue bags contain 1 striped tan bag, 5 mirrored teal bags, 2 dull coral bags.
dim lime bags contain 4 striped plum bags.
light red bags contain 2 shiny black bags, 2 posh brown bags, 2 dotted purple bags, 3 dim indigo bags.
plaid yellow bags contain 4 shiny blue bags, 1 wavy white bag, 3 muted beige bags, 5 posh purple bags.
dark fuchsia bags contain 2 bright lime bags, 4 pale silver bags.
vibrant black bags contain 5 dark violet bags.
bright purple bags contain 5 striped brown bags, 3 wavy maroon bags, 5 drab indigo bags, 2 bright brown bags.
pale lavender bags contain 1 shiny lime bag, 5 shiny green bags, 2 bright purple bags.
dark yellow bags contain 4 drab lime bags.
pale white bags contain 3 shiny beige bags, 3 mirrored purple bags, 2 dark silver bags.
dull green bags contain 5 clear olive bags, 2 pale green bags.
dark crimson bags contain 4 clear turquoise bags, 1 wavy aqua bag, 2 bright tan bags, 4 mirrored tan bags.
dim olive bags contain 2 dark lavender bags, 4 muted chartreuse bags, 2 posh purple bags.
dotted olive bags contain 2 dull bronze bags, 1 faded lavender bag.
light yellow bags contain 3 posh brown bags, 4 muted orange bags.
posh blue bags contain 4 clear olive bags, 1 clear teal bag, 1 pale silver bag, 4 shiny blue bags.
drab brown bags contain 5 light teal bags, 4 clear magenta bags, 5 clear olive bags, 2 striped silver bags.
clear lime bags contain 5 drab blue bags, 5 shiny cyan bags, 5 shiny red bags.
shiny coral bags contain 3 bright orange bags, 4 bright gray bags, 4 muted chartreuse bags, 4 dull salmon bags.
plaid beige bags contain 3 mirrored gold bags, 2 pale tomato bags.
clear maroon bags contain 1 striped magenta bag, 3 shiny violet bags, 1 wavy magenta bag.
faded beige bags contain 4 dotted crimson bags, 3 dull indigo bags, 2 mirrored orange bags, 2 dim white bags.
pale orange bags contain 2 clear orange bags, 3 vibrant aqua bags.
dull fuchsia bags contain 1 light plum bag, 5 dotted white bags, 4 dotted purple bags, 2 dim teal bags.
mirrored gray bags contain 3 faded white bags, 4 light olive bags, 5 muted turquoise bags.
faded cyan bags contain 4 shiny plum bags, 2 pale orange bags, 5 clear orange bags, 4 striped fuchsia bags.
vibrant yellow bags contain 3 shiny yellow bags, 2 striped magenta bags.
dotted fuchsia bags contain 5 shiny olive bags, 4 faded olive bags.
dim teal bags contain 1 shiny yellow bag, 3 light purple bags, 4 mirrored teal bags.
dull red bags contain 4 faded purple bags, 1 drab black bag.
striped beige bags contain 1 mirrored salmon bag, 5 faded purple bags, 5 clear beige bags.
mirrored maroon bags contain 1 dim plum bag, 3 pale chartreuse bags, 1 striped tan bag.
dim green bags contain 3 plaid olive bags.
plaid tomato bags contain 3 drab yellow bags, 4 vibrant magenta bags, 2 muted cyan bags, 1 muted black bag.
shiny green bags contain 1 dull tomato bag, 1 dotted orange bag.
clear violet bags contain 1 dark violet bag.
dull beige bags contain 1 light beige bag, 2 muted green bags, 5 plaid gold bags.
shiny bronze bags contain 1 bright violet bag.
wavy bronze bags contain 1 plaid lavender bag.
clear blue bags contain 2 dark lavender bags.
pale green bags contain 3 bright teal bags, 3 dim lime bags, 2 vibrant purple bags, 5 light plum bags.
posh violet bags contain 3 plaid beige bags.
dim maroon bags contain 5 plaid cyan bags.
faded tan bags contain 3 vibrant bronze bags, 1 drab tomato bag, 2 dim indigo bags, 1 bright bronze bag.
dark turquoise bags contain 2 pale salmon bags, 4 muted purple bags, 2 light olive bags.
vibrant white bags contain 1 posh silver bag, 2 clear olive bags, 2 bright brown bags, 3 muted turquoise bags.
striped salmon bags contain 2 dotted orange bags, 4 dotted maroon bags.
plaid silver bags contain 2 vibrant purple bags, 5 dark brown bags.
muted magenta bags contain no other bags.
light cyan bags contain 5 faded lavender bags.
bright aqua bags contain 5 plaid white bags, 1 striped coral bag, 4 muted chartreuse bags, 3 light turquoise bags.
muted red bags contain 5 dotted gold bags.
mirrored lavender bags contain 1 muted cyan bag.
drab salmon bags contain 3 light black bags, 5 posh lavender bags, 5 dull aqua bags, 2 mirrored white bags.
muted tomato bags contain no other bags.
bright salmon bags contain 5 dim coral bags, 1 wavy beige bag, 2 striped gray bags.
mirrored salmon bags contain 4 clear purple bags, 4 dim black bags.
bright tomato bags contain 3 dark chartreuse bags.
vibrant silver bags contain no other bags.
wavy purple bags contain 4 posh silver bags, 4 pale tan bags, 3 drab fuchsia bags, 5 shiny violet bags.
light purple bags contain 5 clear yellow bags, 3 dotted white bags, 3 pale silver bags, 1 striped blue bag.
mirrored tomato bags contain 3 shiny crimson bags, 1 muted chartreuse bag, 1 drab tan bag, 3 pale tomato bags.
faded fuchsia bags contain 1 posh orange bag, 4 clear chartreuse bags, 1 dull gray bag.
mirrored purple bags contain 4 faded fuchsia bags, 4 dark magenta bags, 5 wavy brown bags.
vibrant violet bags contain 5 dotted aqua bags.
vibrant crimson bags contain 1 dotted maroon bag, 2 posh silver bags, 3 clear teal bags, 3 dim silver bags.
dull yellow bags contain 1 clear chartreuse bag.
clear salmon bags contain 2 clear red bags.
shiny olive bags contain 3 dim indigo bags, 3 wavy maroon bags.
bright violet bags contain 4 dotted red bags, 1 mirrored green bag.
dull violet bags contain 3 bright lime bags, 4 striped plum bags, 5 drab crimson bags, 2 vibrant white bags.
shiny white bags contain 3 vibrant aqua bags, 3 dim teal bags, 3 dark orange bags.
dark lime bags contain 3 muted magenta bags.
clear teal bags contain 4 posh cyan bags, 2 pale silver bags, 5 plaid aqua bags, 1 dull yellow bag.
faded indigo bags contain 3 plaid crimson bags.
wavy aqua bags contain 5 striped black bags.
muted aqua bags contain 5 shiny green bags, 4 mirrored cyan bags, 2 light silver bags, 5 striped black bags.
dotted magenta bags contain 5 striped plum bags, 3 shiny red bags.
faded teal bags contain 4 dim silver bags, 3 faded green bags, 5 drab coral bags.
posh coral bags contain 3 plaid olive bags, 5 striped brown bags, 2 bright fuchsia bags, 1 bright tan bag.
faded magenta bags contain 3 striped cyan bags, 1 dim beige bag.
posh plum bags contain 4 clear plum bags, 2 clear teal bags.
faded violet bags contain 2 clear beige bags, 4 shiny red bags.
dotted bronze bags contain 4 dim brown bags.
mirrored gold bags contain 3 posh cyan bags, 2 dim silver bags.
muted salmon bags contain 1 bright maroon bag, 3 dark tomato bags.
dim gold bags contain 3 posh red bags, 5 vibrant silver bags, 4 drab tan bags.
bright olive bags contain 3 dim salmon bags, 5 pale coral bags, 4 bright tomato bags.
posh chartreuse bags contain 4 vibrant white bags, 2 dull indigo bags.
wavy fuchsia bags contain 3 shiny magenta bags, 4 wavy red bags, 4 faded gold bags, 4 posh red bags.
posh magenta bags contain 5 light turquoise bags, 1 dull blue bag.
dim violet bags contain 5 shiny gold bags, 4 plaid aqua bags, 5 dull violet bags, 1 clear violet bag.
plaid white bags contain 3 muted magenta bags, 3 plaid aqua bags, 2 faded black bags, 2 shiny gold bags.
dotted silver bags contain 5 mirrored brown bags, 5 bright yellow bags.
mirrored brown bags contain 4 dim plum bags.
dull blue bags contain 1 pale indigo bag, 2 shiny black bags, 3 faded lavender bags.
dim coral bags contain 2 plaid aqua bags, 1 dotted orange bag, 4 striped coral bags.
mirrored violet bags contain 2 pale green bags, 5 dark lavender bags, 1 faded olive bag.
dotted purple bags contain 3 pale indigo bags.
dull turquoise bags contain 4 striped magenta bags, 2 dull gray bags, 3 shiny indigo bags.
striped black bags contain 2 mirrored teal bags.
shiny indigo bags contain 3 shiny gray bags, 2 clear plum bags.
plaid magenta bags contain 3 light white bags.
wavy black bags contain 2 dull cyan bags, 3 pale orange bags, 4 clear magenta bags.
pale coral bags contain 1 bright tomato bag.
drab cyan bags contain 3 dark chartreuse bags, 2 dim black bags, 1 dotted gray bag.
dark cyan bags contain 4 bright turquoise bags, 5 faded cyan bags, 5 dim salmon bags.
bright brown bags contain 4 drab tan bags, 4 mirrored gold bags.
striped chartreuse bags contain 1 wavy silver bag.
striped yellow bags contain 3 dim beige bags, 5 dim coral bags.
dotted aqua bags contain 1 mirrored green bag, 5 shiny maroon bags.")
