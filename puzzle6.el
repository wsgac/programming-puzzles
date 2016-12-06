;; --- Day 6: Signals and Noise ---

;; Something is jamming your communications with Santa. Fortunately, your signal is only partially jammed, and protocol in situations like this is to switch to a simple repetition code to get the message through.

;; In this model, the same message is sent repeatedly. You've recorded the repeating message signal (your puzzle input), but the data seems quite corrupted - almost too badly to recover. Almost.

;; All you need to do is figure out which character is most frequent for each position. For example, suppose you had recorded the following messages:

;; eedadn
;; drvtee
;; eandsr
;; raavrd
;; atevrs
;; tsrnev
;; sdttsa
;; rasrtv
;; nssdts
;; ntnada
;; svetve
;; tesnvt
;; vntsnd
;; vrdear
;; dvrsen
;; enarar
;; The most common character in the first column is e; in the second, a; in the third, s, and so on. Combining these characters returns the error-corrected message, easter.

;; Given the recording in your puzzle input, what is the error-corrected version of the message being sent?

(defvar *signal* '("uflevqwk" "rrddgrgk"  "jownyelh"  "sskcoaaq"  "cfngremt"  "jntgxujt"  "wiatbvvv"  "ilspdbkl"  "cwgqwrrq"  "acusllxy"  "kidxuluo"  "gxunfaxm"  "rfuifzfd"  "qidkyaow"  "tjqlatwp"  "nhxmfxyx"  "tanronrx"  "qzjpwgvl"  "junnfzts"  "eumwzjzv"  "eonigpng"  "dufrqxjd"  "isqxdaej"  "hpqakcge"  "sywhphbi"  "fjskaqwd"  "hmyvgeqj"  "tpegglex"  "vpbzqqfp"  "jznqymjw"  "ngnyjegm"  "qgpocbnr"  "oaqwlpkp"  "jiowhrte"  "qamdygfg"  "ylvnybbb"  "makvhqsf"  "gjjnnbsg"  "xghnzlkw"  "sfkcmopw"  "wiyvydqu"  "bxczqgba"  "pcrlchbn"  "vvhewabm"  "nnftdojn"  "icywzuwc"  "wiokylcn"  "qmxppodk"  "yzedohag"  "fvjrfyrj"  "tfkaufmo"  "xkemipkv"  "iyatsikk"  "eafrhudq"  "nfyghtau"  "wttrrdvo"  "wauvzvhd"  "bzzblbam"  "sswyuaow"  "mvufeors"  "iumazrzo"  "clhrmzdc"  "czvoaqyr"  "plrvvzjb"  "yjxrypwt"  "rinqjksp"  "mmigjtpp"  "abrmadmr"  "fvmijrpg"  "jxwuzsdq"  "gqahdshs"  "lumftsyb"  "fkecsohh"  "bpsxszgv"  "qqepgxkb"  "iacloyjn"  "xtrurnvi"  "owrjjche"  "ittraljs"  "ekdxrdsu"  "kxmzdpol"  "uekohjhp"  "rlxydeli"  "gzqnbjzd"  "zwywoena"  "hgrhtngu"  "cyderbzp"  "mqwbnimx"  "ygxhrzpc"  "viqvwmir"  "ajwdsqnv"  "vycvuquh"  "rlqburdz"  "nrjikkqz"  "byiuygpb"  "qcwlrnvs"  "iqucoawp"  "utoaavak"  "ggjbwqvk"  "issedmzt"  "mmapzgrn"  "iwdadrtc"  "dpeszvwm"  "zgggkijl"  "gdneqfqi"  "wyaqgjis"  "jvowmavy"  "vxvesfbr"  "jsqmzcxq"  "fdtbsrpl"  "avmjobsz"  "nelqcrir"  "bohozvfw"  "pgnbpxve"  "zhzhlbox"  "hgmkxgqq"  "lcgcoyic"  "dxgjxzvr"  "sssgrtay"  "dhtzcyfj"  "qcovunah"  "lekkbwfw"  "uwdrgkrm"  "brlyetlg"  "dputvtfm"  "xkbgowvo"  "wdqnyjyj"  "lpkuxlqb"  "kejpkgbf"  "urfwmenq"  "tzugmuca"  "zmtfxlpn"  "bmuzgzco"  "swainwsv"  "cfmiaylt"  "vrzplmiu"  "ekoixwzr"  "sqbthhmj"  "kssmukqk"  "kgscswjx"  "owpvilla"  "wtzxjouf"  "cauhexcn"  "dprkkeah"  "pxptpwlv"  "btfmykon"  "lyvhwdcf"  "uojsofzg"  "ubqslzwj"  "pfvuloqf"  "hrucrkyt"  "urtjueoq"  "vqiibvdk"  "hvdtsupq"  "mryedbuz"  "yplctgby"  "ftqrhyal"  "oqeuxwsh"  "rclpgvjw"  "mhcrzrcg"  "covwlwho"  "gjhxzapi"  "egzhpzza"  "inbmkkbh"  "ezjvqrkb"  "szrmrsxh"  "ajyakvvs"  "hlmfstfp"  "wghyzvuf"  "ydidinar"  "oirwjiqo"  "udtjksso"  "jwnaosru"  "lhdyuucj"  "ekeifpwp"  "djefaqtt"  "rbwqvkax"  "lcaupsdz"  "ovjjidmx"  "hvpdenve"  "qdlqusku"  "enlupxlr"  "adtrmoyy"  "nnlluigo"  "gbyotoqo"  "ajavmaun"  "xtanzcix"  "fcqgsiuw"  "jbyyhitl"  "ctqkoeic"  "vusvexhj"  "dywmkciv"  "uegfjdip"  "sgksturi"  "fnppgtvo"  "epzgllth"  "zpkurdxk"  "kkesnnqe"  "tkepiakd"  "gukbfvsw"  "evgdijdz"  "rbildand"  "fxmpwuvs"  "fmnzyqea"  "vfktdoao"  "whgkcomu"  "umfodiid"  "afvjmwrf"  "lonuybho"  "lhopnkwv"  "bxekeeki"  "lzdgfhyo"  "hobjehhm"  "bjsyzipy"  "bgizykuf"  "uktmpbnb"  "qxxdsxro"  "dqxmncoz"  "abqqwmei"  "kyvcmjfb"  "fngzcrzk"  "dythedbo"  "oejkgzke"  "hocnhppm"  "ogugyxhi"  "ncikbndl"  "amwuhiuf"  "psrllnmr"  "xdhbvbcc"  "hobbvqhw"  "phiwblgs"  "wrixgmhb"  "vvmrbduw"  "nhakqryr"  "vsbbxaur"  "yfdsecin"  "kacwteta"  "efvhzgyg"  "lrjzpefq"  "zrjvuqrv"  "jcwnbmvh"  "wjuvxmou"  "nqcvtdcg"  "ucpnrgxc"  "uswrjjdi"  "mmyjmojk"  "wvcagtyn"  "recxcumh"  "qvrxogte"  "ljfyiyme"  "olmihwjb"  "plzycdty"  "qnyoxrfx"  "onsuhsdi"  "asbocmtb"  "jvndrjvs"  "mgzoinlt"  "rjfpztzt"  "fmdjpiqu"  "goofefiz"  "iqhvllgl"  "culamtyk"  "myxvmukc"  "cwvftlqc"  "lluqawec"  "mlmvmwfm"  "utrfawmz"  "olzrgvmv"  "zisabtap"  "fcuxuvnj"  "qyimlura"  "tlaltbsi"  "uyiyareq"  "hpztunnb"  "drfdwtfz"  "rosbuudb"  "tioknlid"  "gddxjxwj"  "aziakvby"  "hhttoluv"  "kkarvjkn"  "puprxohy"  "xjcxgzcv"  "dxuyhpkq"  "vclyijgd"  "redbicst"  "gqbttgxu"  "bffdncnd"  "xskpmkio"  "skclmsum"  "tznntmoj"  "gzhhsfwc"  "ywbppcgx"  "byrbeaxe"  "hwrlakpf"  "crlwqcgj"  "vhkaxgcz"  "wftehney"  "tbtmrkxb"  "fusuyqka"  "rwpnxmhx"  "rzqonvaz"  "ybpwtppo"  "cnverhwt"  "vdtfgbux"  "palvhikl"  "kzhqkdll"  "qqklvdyg"  "jmladlee"  "nepjydti"  "aedwmblx"  "ramkmzgr"  "wsolkgti"  "wwfrciju"  "roocdhyw"  "osstwykz"  "jkpkhqew"  "aqmgjjjd"  "tajwlxnv"  "plhodkvv"  "xcahqxwi"  "zylepnec"  "pmlywqoo"  "ospgrrdu"  "wblhxxir"  "iqfzvlpt"  "bsofcsig"  "iepxcvwy"  "guugpghl"  "ikwdlzfs"  "yomdcwxz"  "urbpcbrh"  "wtrzcfiv"  "kaapqrqk"  "ienueukt"  "digyopeb"  "kfgclsod"  "henjotok"  "nkzjuoxm"  "xohkdunh"  "gubuvylj"  "lugeqspf"  "xdkcfccg"  "sjnkiffo"  "xbviiody"  "juwtshlv"  "hdjfddmc"  "ipftsslh"  "zggnepcm"  "rvccowqn"  "swmcofau"  "oksngpvy"  "bjcthagw"  "tmzxsyqs"  "rebascnq"  "yxfixusy"  "iwewlkxk"  "nndfkckq"  "uhvaxjal"  "fcgqdlru"  "tkhlguoa"  "wsoefgmr"  "eebcbzeh"  "asmepwma"  "dqbujtpa"  "xjvmupwe"  "rrufqppv"  "yiaqkmsf"  "cvivqgtm"  "yrxfrfdi"  "bjkmyhdp"  "kwdvoyvn"  "nykyqaxg"  "zqioepkg"  "dwxantqi"  "bklxeoqh"  "rlxawoax"  "bdpfhqkn"  "fyzpjymf"  "meoqawzz"  "zowwxenu"  "zhqpnbtv"  "cubwtngh"  "srkxkkbi"  "kxcdvznw"  "ipsllqbn"  "imecbjsm"  "sehlcine"  "iogdiznk"  "gfovshxc"  "zkofnscb"  "zqiffwrz"  "zizfepxw"  "abknfcxu"  "hmqtwdqf"  "kehagtsw"  "xsemphlf"  "qwkuvbea"  "febqsrpl"  "cwpiafgk"  "adqsvihd"  "meclknwc"  "rpchvmja"  "mpqsuevs"  "yoebikcy"  "siyqnsga"  "dhoonzsx"  "zwydsejp"  "naytcuwv"  "ftbcjdte"  "kqckamex"  "nirapuiw"  "fayqugxd"  "whdbfmwl"  "pocevsjf"  "pxoxjycs"  "lpfwtpox"  "yaqigvsj"  "okiszxlp"  "mlxwdfoq"  "vjyckcjt"  "mvsefvcb"  "yrimofcf"  "qbxahhop"  "uarjvpep"  "gdtednol"  "damojpod"  "vtrmgubl"  "ndyuwayp"  "jhwjhbwx"  "cgjenmep"  "hawhcszs"  "ddaxxpec"  "qpruxjsi"  "nczelirl"  "ukesixzr"  "yfoqzfbk"  "kdgbriyq"  "dbjinxzc"  "tqjuhice"  "ntbgchkh"  "tlfxuvfj"  "ynwfakyu"  "xivxijyl"  "hsemlrom"  "oaaebmru"  "fskqiiiu"  "dbxrjzqd"  "fuelfktt"  "rknawwlh"  "pdyuhufu"  "pmvsoiwp"  "qcygmygy"  "lhovvslf"  "vrxdvmfq"  "crgndvzy"  "gyzzesbl"  "ankdtxyy"  "wfrurbrd"  "wrmpakxf"  "ubczqfpb"  "sohcpnnk"  "lrjfiylr"  "vdwmlkne"  "tlekcnym"  "jadicszq"  "kztcxzwn"  "qptbjfuf"  "gjhhzhuh"  "myjjzyzf"  "akxabrde"  "lgnystrm"  "zxtuokjq"  "bffdkzun"  "modssgbc"  "xvedbysj"  "czpdqkdn"  "qlzprpuj"  "abuoeoex"  "ymxzkccu"  "jbslwqku"  "wgehntzj"  "zivljhpa"  "jsustyir"  "xglfyyun"  "emitjtfp"  "jyltuhtp"  "vbqgtwni"  "iojlycew"  "bhrrxbcg"  "vhswcbaz"  "tzsobxfz"  "hpcbvvnt"  "cuilflxq"  "ohklemxz"  "aklsjxas"  "qczwvhdv"  "kecxvvua"  "nnlcshhz"  "gtxeqhed"  "ebxirqft"  "xawqimpq"  "ehizwpau"  "gjgobwcc"  "wuojmfgr"  "bubtyubk"  "mfnjqxsd"  "bzuobqeg"  "cnwsyfqt"  "difjbhys"  "awvnvqxb"  "eblmxhlf"  "enoeneis"  "ispzlatd"  "pvtyulpk"  "lrjnrogd"  "dfxsbdhb"  "hivbahmg"  "azpiqnci"  "ktcmajzb"  "mfngiemn"  "xtktrzne"  "xydgmtrj"  "ittdjyqt"  "pvrapfhj"  "pfpqmubd"  "eemyoxye"  "eyorfilw"  "jmztxeql"  "oybsnyus"  "nihbjowm"  "gknzlcem"  "chcslofr"  "zbpsfdsg"  "dxbzaqos"  "cqzhujhq"  "qlvbwfsn"  "ydgipmtr"  "fhbcfors"  "enxopgsy"  "sbtdtgjn"  "klapbmbk"  "qxuqemnb"  "pspxwtlz"  "vnorcada"  "qcmhrdcg"  "lxwmeejb"  "mxgueeih"  "rggqkdvj"  "kwyrmqms"  "nzfhpzmc"  "ohrlyols"  "vrzgflzw"  "zujmieye"  "smzjujaa"  "mwfontrh"  "suhnkihy"  "tpavgxze"  "tjzbnyvy"  "leakgvti"  "cptoaqgx"  "pegikbtg"  "ocggetmv"  "xxiytrxf"  "yqpipjfa"  "tihrfpez"  "ukxiajhk"  "xighnfgf"  "sgcitckm"  "tbvhtnmr"  "jwbooowx"  "ldnjtipa"  "xpehmloo"  "stxkplmo"  "lgqddrhb"  "hdhsqxdt"  "daxztiim"  "pcafwglc"  "ynhyvsjt"  "lujyvuug"  "pnicccbq"  "stmwwjec"  "zzsysevl"  "ypvbqpfo"  "tljegcgq"  "bqanmeji"  "ejqeknda"  "jndzdwde"  "pneveaiv"  "gjwpfrjz"  "zzwsmfss"  "ylakaful"  "gmyzwvot"  "toiqhrrh"  "htszfvzt"  "hagybnpz"  "ocliwiav"  "evzlyabp"  "txppqmkk"  "xovsnwyn"  "smsbqsax"  "anysxgxd"  "ztqoskny"  "uuvozsty"  "nynkbdot"  "nxpgfyyw"  "vxftiuty"  "svijqamw"  "rqdjnwdm"  "vqhcjbqp"  "mibabxxi"  "bffhbltc"  "zotfzcbx"  "ozsxkzzh"  "mluqlfrm"  "wvrnhdvg"  "fmyniyor"  "kcdxacgg"  "oxfzqjba"  "fdhedghj"  "otytinze"  "uufwzhll"  "jnlztauj"  "ktujpjae"))

(defun transpose-string-list (strings)
  (apply #'map 'list #'list (mapcar #'string-to-list strings)))

(defun hash-table-alist (table)
  "Returns an association list containing the keys and values of hash table TABLE."
  (let ((alist nil))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             table)
    alist))

(defun hash-table-top-value (table)
  "Returns the top N entries from hash table TABLE. Values are expected to be numeric."
  ;; (subseq (cl-sort (hash-table-alist table) #'> :key #'cdr) 0 n))
  (car (cl-sort (hash-table-alist table) #'> :key #'cdr)))

(defun hash-table-bottom-value (table)
  "Returns the top N entries from hash table TABLE. Values are expected to be numeric."
  ;; (subseq (cl-sort (hash-table-alist table) #'> :key #'cdr) 0 n))
  (car (cl-sort (hash-table-alist table) #'< :key #'cdr)))

(defun find-most-frequent (list)
  (cl-loop
   with hash = (make-hash-table)
   for item in list
   do (incf (gethash item hash 0))
   finally (return (hash-table-top-value hash))))

(defun find-least-frequent (list)
  (cl-loop
   with hash = (make-hash-table)
   for item in list
   do (incf (gethash item hash 0))
   finally (return (hash-table-bottom-value hash))))

(defun generate-histograms (signal)
  (mapcar #'car
	  (mapcar #'find-most-frequent
		  (transpose-string-list signal))))

(defun solve-puzzle (&optional signal)
  (apply #'string (generate-histograms (or signal *signal*))))

;; --- Part Two ---

;; Of course, that would be the message - if you hadn't agreed to use a modified repetition code instead.

;; In this modified code, the sender instead transmits what looks like random data, but for each character, the character they actually want to send is slightly less likely than the others. Even after signal-jamming noise, you can look at the letter distributions in each column and choose the least common letter to reconstruct the original message.

;; In the above example, the least common character in the first column is a; in the second, d, and so on. Repeating this process for the remaining characters produces the original message, advent.

;; Given the recording in your puzzle input and this new decoding methodology, what is the original message that Santa is trying to send?

(defun solve-puzzle-2 (&optional signal)
  (apply #'string
	 (mapcar #'car
		 (mapcar #'find-least-frequent
			 (transpose-string-list (or signal *signal*))))))
