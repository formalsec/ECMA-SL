module Val = struct

  let val_hashtable_html = Hashtbl.create 0

  let () =
    Hashtbl.add val_hashtable_html "" "the empty String";
    Hashtbl.add val_hashtable_html "Value" "[[Value]]";
    Hashtbl.add val_hashtable_html "Writable" "[[Writable]]";
    Hashtbl.add val_hashtable_html "Set" "[[Set]]";
    Hashtbl.add val_hashtable_html "Get" "[[Get]]";
    Hashtbl.add val_hashtable_html "Enumerable" "[[Enumerable]]";
    Hashtbl.add val_hashtable_html "Configurable" "[[Configurable]]";
    Hashtbl.add val_hashtable_html "Object" "Object";
    Hashtbl.add val_hashtable_html "Undefined" "Undefined";
    Hashtbl.add val_hashtable_html "Null" "Null";
    Hashtbl.add val_hashtable_html "Boolean" "Boolean";
    Hashtbl.add val_hashtable_html "Number" "Number";
    Hashtbl.add val_hashtable_html "String" "String";
    Hashtbl.add val_hashtable_html "EnvironmentRecord" "<a href=\"#sec-10.2.1\">Environment Record</a>";
    Hashtbl.add val_hashtable_html "Class" "[[Class]]";
    Hashtbl.add val_hashtable_html "Put" "[[Put]]";
    Hashtbl.add val_hashtable_html "Prototype" "[[Prototype]]";
    Hashtbl.add val_hashtable_html "Extensible" "[[Extensible]]";
    Hashtbl.add val_hashtable_html "ParameterMap" "[[ParameterMap]]";
    Hashtbl.add val_hashtable_html "Call" "[[Call]]";
    Hashtbl.add val_hashtable_html "Construct" "[[Construct]]";
    Hashtbl.add val_hashtable_html "HasInstance" "[[HasInstance]]";
    Hashtbl.add val_hashtable_html "Scope" "[[Scope]]";
    Hashtbl.add val_hashtable_html "Code" "[[Code]]";
    Hashtbl.add val_hashtable_html "FormalParameters" "[[FormalParameters]]";
    Hashtbl.add val_hashtable_html "TargetFunction" "[[TargetFunction]]";
    Hashtbl.add val_hashtable_html "BoundThis" "[[BoundThis]]";
    Hashtbl.add val_hashtable_html "BoundArgs" "[[BoundArgs]]";
    Hashtbl.add val_hashtable_html "GetOwnProperty" "[[GetOwnProperty]]";
    Hashtbl.add val_hashtable_html "DefineOwnProperty" "[[DefineOwnProperty]]";
    Hashtbl.add val_hashtable_html "Delete" "[[Delete]]";
    Hashtbl.add val_hashtable_html "Reference" "<a href=\"#sec-8.7\">Reference</a>";
    Hashtbl.add val_hashtable_html "Get_internal" "the special [[Get]] internal method defined below";
    Hashtbl.add val_hashtable_html "Put_internal" "the special [[Put]] internal method defined below";
    Hashtbl.add val_hashtable_html "ArgumentsObjectGet" "the definition provided below";
    Hashtbl.add val_hashtable_html "ArgumentsObjectGetOwnProperty" "the definition provided below";
    Hashtbl.add val_hashtable_html "ArgumentsObjectDefineOwnProperty" "the definition provided below";
    Hashtbl.add val_hashtable_html "ArgumentsObjectDelete" "the definition provided below";
    Hashtbl.add val_hashtable_html "internalTypeErrorThrower" "be a <i><a href=\"#sec-13\">FunctionBody</a></i> that unconditionally throws a <b>TypeError</b> exception and performs no other action"

end

module E_Expr = struct

  let special_vals_hashtable_html = Hashtbl.create 0
  let vars_hashtable_html = Hashtbl.create 0
  let internal_methods_list = [
    "Call";
    "HasInstance";
    "Construct"
  ]

  let init_var_bindings (bindings : (string * string) list) : unit =
    Hashtbl.reset vars_hashtable_html;
    Hashtbl.replace_seq vars_hashtable_html (List.to_seq bindings)

  let is_internal_method (name : string) : bool =
    List.mem name internal_methods_list

  let is_special_val (str : string) : bool =
    Hashtbl.mem special_vals_hashtable_html str

  let get_special_val_html (str : string) : string =
    Hashtbl.find special_vals_hashtable_html str

  let () =
    Hashtbl.add special_vals_hashtable_html "GetF" "as described in <a href=\"#sec-15.3.5.4\">15.3.5.4</a>";
    Hashtbl.add special_vals_hashtable_html "Call" "as described in <a href=\"#sec-13.2.1\">13.2.1</a>";
    Hashtbl.add special_vals_hashtable_html "CallBind" "as described in <a href=\"#sec-15.3.4.5.1\">15.3.4.5.1</a>";
    Hashtbl.add special_vals_hashtable_html "Construct" "as described in <a href=\"#sec-13.2.2\">13.2.2</a>";
    Hashtbl.add special_vals_hashtable_html "ConstructBind" "as described in <a href=\"#sec-15.3.4.5.2\">15.3.4.5.2</a>";
    Hashtbl.add special_vals_hashtable_html "HasInstance" "as described in <a href=\"#sec-15.3.5.3\">15.3.5.3</a>";
    Hashtbl.add special_vals_hashtable_html "HasInstanceBind" "as described in <a href=\"#sec-15.3.4.5.3\">15.3.4.5.3</a>";

end

module E_Stmt = struct

  let vars_hashtable_html = Hashtbl.create 0
  let macro_hashtable_html = Hashtbl.create 0
  let swappable_hashtable_html = Hashtbl.create 0

  let init_var_bindings (bindings : (string * string) list) : unit =
    Hashtbl.reset vars_hashtable_html;
    Hashtbl.replace_seq vars_hashtable_html (List.to_seq bindings)

  let () =
    Hashtbl.add macro_hashtable_html "Reject" (Some "<a href=\"#reject-DefineOwnProperty\">Reject</a>");
    Hashtbl.add swappable_hashtable_html "Reject" true;
end
