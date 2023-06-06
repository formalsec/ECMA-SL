; 
(set-info :status true)
(declare-fun z () Real)
(assert
 (let (($x36 (= z 0.0)))
 (= $x36 false)))
(assert
 (let (($x36 (= z 0.0)))
 (= $x36 false)))
(assert
 (let (($x9 (= 0.0 z)))
 (not $x9)))
(assert
 (let (($x32 (< 0.0 z)))
 (not $x32)))
(check-sat)
