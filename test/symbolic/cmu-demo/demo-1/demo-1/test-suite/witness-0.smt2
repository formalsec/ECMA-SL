; 
(set-info :status true)
(declare-fun __len () Int)
(declare-fun __pos () Int)
(declare-fun iface___instr_symb_str_0 () String)
(assert
 (let ((?x9 (str.++ "cat /sys/class/net/" iface___instr_symb_str_0)))
 (let ((?x11 (str.++ ?x9 "/address")))
 (let ((?x14 (str.substr ?x11 __pos __len)))
 (= ?x14 "; $(touch success) #")))))
(check-sat)
