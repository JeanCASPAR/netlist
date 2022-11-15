open Netlist_ast

exception Type_error of string

let print_only = ref false
let number_steps = ref (-1)
let debug_mode = ref false

(* c/c de netlist_parser.mly *)
let bool_of_string s = match s with
| "t" | "1" -> true
| "f" | "0" -> false
| _ -> raise (Netlist.Parse_error "Wrong input")

(* c/c de netlist_parser.mly *)
let bool_array_of_string s =
 let a = Array.make (String.length s) false in
 for i = 0 to String.length s - 1 do
   a.(i) <- bool_of_string (String.sub s i 1)
 done;
 a

let simulator program number_steps =
  if !print_only then
    List.iter (fun (id, _) -> Format.printf "%s@." id) program.p_eqs
  else begin

  (* Some auxiliary functions *)
  let read_arg id =
    let s = read_line () in
    match Env.find id program.p_vars with
    | TBit -> VBit (bool_of_string s)
    | TBitArray n ->
      let t = bool_array_of_string s in
      if Array.length t = n
      then VBitArray t
    else raise (Netlist.Parse_error "Wrong input")
  in
  let format_arg =
    let b_to_s = function true -> "1" | false -> "0"
    in
    function
    | VBit b -> b_to_s b
    | VBitArray t -> String.concat "" (List.map b_to_s (Array.to_list t))
  in
  let get_val var vars = match Env.find_opt var vars with
  | Some v -> v
  (* default : zero *)
  | None -> match Env.find var program.p_vars with
    | TBit -> VBit false
    | TBitArray n -> VBitArray (Array.make n false)
  in
  let get_arg arg vars = match arg with
  | Avar id -> get_val id vars
  | Aconst value -> value
  in
  let get_addr addr vars = get_arg addr vars |>
    (function VBitArray t -> t | VBit b -> [| b |]) |>
    (* We assume big-endian representation *)
    fun t ->
    Array.fold_right (fun b (i, tmp) -> (i + 1, if b then 1 lsl (i + 1) + tmp else tmp)) t (-1, 0) |>
    snd
  in
  let init_rom id addr_size word_size =
    Format.printf "initializing ROM %s [%d, %d]:@." id addr_size word_size;
    Format.printf "m (default) if you want it to be manually filled@.";
    Format.printf "f if you want it to be read from a file, see README.md for format@.";
    Format.printf "z if you want it to be zeroed@.";
    let choice = read_line () in
    let t = Array.make (1 lsl addr_size) (VBitArray (Array.make word_size false)) in
    begin match choice with
    | "z" -> ()
    | "m" | "" -> begin
        for i = 0 to (1 lsl addr_size - 1) do
          Format.printf "%s[%d] ? @?" id i;
          let a = read_line () in
          let a = bool_array_of_string a in
          if Array.length a <> word_size
          then raise (Type_error ("Wrong input, expected bit array of size " ^ string_of_int word_size))
          else t.(i) <- VBitArray a
        done
      end
    | "f" -> begin
      Format.printf "path ? @?";
      let path = read_line () in
      let ic = Netlist.find_file path in
      let line = ref 0 in
      try
        while true do
          let s = input_line ic in
          let tmp = bool_array_of_string s in
          if Array.length tmp <> word_size
          then raise (Type_error
            ("Wrong input at line " ^ string_of_int !line ^ ", expected bit array of size " ^ string_of_int word_size)
          )
          else t.(!line) <- VBitArray tmp;
          incr line
        done
      with End_of_file -> ();
    end
    | _ -> raise (Netlist.Parse_error ("Wrong input, expected z, m or f, got " ^ choice))
    end;
    t
  in
  (* simulator code *)

  (* contains the previous value of each variables
     used with the reg instruction
     get the value with get_val *)
  let old = ref Env.empty in

  (* initialize ram and rom *)
  let (ram, rom) = begin
    let ram = ref Env.empty in
    let rom = ref Env.empty in
    List.iter (fun (id, e) -> match e with
      | Eram (addr_size, word_size, _, _, _, _) ->
        ram := Env.add id (Array.make (1 lsl addr_size) (VBitArray (Array.make word_size false))) !ram
      | Erom (addr_size, word_size, _) ->
        let t = init_rom id addr_size word_size in
        rom := Env.add id t !rom
      | _ -> ()
    ) program.p_eqs;
    (!ram, !rom)
  end in

  for i = 0 to number_steps - 1 do
    Format.printf "step %d :@." i;
    let vars = ref Env.empty in
    (* assign inputs *)
    List.iter
      (fun id -> Format.printf "%s ? @?" id; vars := Env.add id (read_arg id) !vars)
      program.p_inputs;
    (* compute the value of each equation *)
    List.iter (fun (id, e) ->
      let res = match e with
      | Earg a -> get_arg a !vars
      | Ereg id -> get_val id !old
      | Enot a -> (match get_arg a !vars with VBit b -> VBit (not b) | VBitArray t -> VBitArray (Array.map not t))
      | Ebinop (op, e1, e2) -> begin match (get_arg e1 !vars, get_arg e2 !vars) with 
        | VBit b1, VBit b2 -> begin match op with
          | Or -> VBit (b1 || b2)
          | Xor -> VBit (b1 <> b2)
          | And -> VBit (b1 && b2)
          | Nand -> VBit (not (b1 && b2))
          end
        | VBitArray t, VBit b | VBit b, VBitArray t -> begin match op with
          | Or -> VBitArray (Array.map (fun b2 -> b || b2) t)
          | Xor -> VBitArray (Array.map (fun b2 -> b <> b2) t)
          | And -> VBitArray (Array.map (fun b2 -> b && b2) t)
          | Nand -> VBitArray (Array.map (fun b2 -> not (b && b2)) t)
        end
        | VBitArray t1, VBitArray t2 -> begin match op with
          | Or -> VBitArray (Array.map2 (fun b1 b2 -> (b1 || b2)) t1 t2)
          | Xor -> VBitArray (Array.map2 (fun b1 b2 -> (b1 <> b2)) t1 t2)
          | And -> VBitArray (Array.map2 (fun b1 b2 -> (b1 && b2)) t1 t2)
          | Nand -> VBitArray (Array.map2 (fun b1 b2 -> (not (b1 && b2))) t1 t2)
        end
      end
      | Emux (choice, e1, e2) -> begin match get_arg choice !vars with 
        | VBit false | VBitArray [| false |]-> get_arg e1 !vars
        | VBit true | VBitArray [| true |] -> get_arg e2 !vars
        | _ -> raise (Type_error "expected bit, got array")
        end
      | Erom (_, _, read_addr) ->
        let addr = get_addr read_addr !vars in
        (Env.find id rom).(addr)
      | Eram (_, _, read_addr, _, _, _) ->
        let addr = get_addr read_addr !vars in
        (Env.find id ram).(addr) (* Write is deferred at the end *)
      | Econcat (e1, e2) -> begin match (get_arg e1 !vars, get_arg e2 !vars) with
        | VBitArray t1, VBitArray t2 -> VBitArray (Array.concat [t1; t2])
        | VBit b, VBitArray t -> VBitArray (Array.concat [[|b|]; t])
        | VBitArray t, VBit b -> VBitArray (Array.concat [t; [|b|]])
        | VBit b1, VBit b2 -> VBitArray [|b1; b2|]
        end
      | Eslice (i, j, a) -> begin match (get_arg a !vars) with
        | VBitArray t -> VBitArray (Array.sub t i (j - i + 1))
        | VBit t -> VBit t
        end
      | Eselect (i, a) -> begin match (get_arg a !vars) with
        | VBit t -> VBit t
        | VBitArray t -> VBit t.(i)
        end
      in vars := Env.add id res !vars
    ) program.p_eqs;

    (* write in RAM *)
    List.iter (fun (id, e) -> match e with
      | Eram (_, _, _, write_enable, write_addr, write_data) ->
        begin match get_arg write_enable !vars with
        | VBit true | VBitArray [| true |] ->
          let write_addr = get_addr write_addr !vars in
          let write_data = get_arg write_data !vars in
          (Env.find id ram).(write_addr) <- write_data
        | _ -> ()
        end
      | _ -> ()
    ) program.p_eqs;

    (* if debug mode is enabled *)
    if !debug_mode
    then Env.iter (fun id _ -> Format.printf "DEBUG: %s = %s@." id (format_arg (get_val id !vars)))
      program.p_vars;
    
    (* print outputs *)
    List.iter
      (fun id -> Format.printf "=> %s = %s@." id (format_arg (get_val id !vars)))
      program.p_outputs;
    old := !vars
  done
  end

let compile filename =
  try
    let p = Netlist.read_file filename in
    begin try
        let p = Scheduler.schedule p in
        simulator p !number_steps
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
    end;
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    [
      "-n", Arg.Set_int number_steps, "Number of steps to simulate";
      "-print", Arg.Bool (fun b -> print_only := b), "Print the sorted netlist";
      "-debug", Arg.Bool (fun b -> debug_mode := b), "Print each variable at the end of each step"
    ]
    compile
    ""
;;

main ()
