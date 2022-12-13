module M

(SL : SecurityLevel.M) = struct

exception Except of string

type sl = SL.t

type state_t =  (sl SecCallStack.t) * (sl SecHeap.t) * (sl SecStore.t) * sl list


type monitor_return = | MReturn of state_t
                      | MFail of (state_t * string)


let eval_small_step (m_state: state_t) (tl:sl SecLabel.t) : monitor_return =
	MFail (m_state, "Everything is declined!")

let initial_monitor_state (): state_t =
  let sheap = SecHeap.create () in
  let ssto = SecStore.create [] in
  let scs = SecCallStack.create () in
  (scs, sheap, ssto, [(SL.get_low ())])


let parse_lvl = SL.parse_lvl

end 
