(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Intel_proc

let bprint_arg_mem b arch mem = match mem with

  |  (_, reg1, 1, BaseSymbol s, 0) ->
    Printf.bprintf b "%s(%%%s)" s (string_of_register arch reg1)

  |  (_, reg1, 1, BaseSymbol s, offset) ->
    if offset < 0 then
      Printf.bprintf b "%s%d(%%%s)" s offset (string_of_register arch reg1)
    else
      Printf.bprintf b "%s+%d(%%%s)" s offset (string_of_register arch reg1)

  |  (_, reg1, scale, BaseSymbol s, offset) ->
    if offset = 0 then
      Printf.bprintf b "%s(,%%%s,%d)" s
        (string_of_register arch reg1) scale
    else    if offset < 0 then
      Printf.bprintf b "%s%d(,%%%s,%d)" s
        offset (string_of_register arch reg1) scale
    else
      Printf.bprintf b "%s+%d(,%%%s,%d)" s
        offset (string_of_register arch reg1) scale

  |  (_, reg1, 1, NoBase, 0) ->
    Buffer.add_char b '(';
    Printf.bprintf b "%%%s" (string_of_register arch reg1);
    Buffer.add_char b ')'

  |  (_, reg1, 1, NoBase, offset) ->
    if offset <> 0 then begin
      if offset < 0 then
        Printf.bprintf b "-%d" (-offset)
      else
        Printf.bprintf b "%d" offset
    end;
    Buffer.add_char b '(';
    Printf.bprintf b "%%%s" (string_of_register arch reg1);
    Buffer.add_char b ')'

  |  (_, reg1, scale, reg2, offset) ->
    if offset <> 0 then begin
      if offset < 0 then
        Printf.bprintf b "-%d" (-offset)
      else
	Printf.bprintf b "%d" offset
    end;
    Buffer.add_char b '(';
    begin
      match reg2 with
	NoBase -> ()
      | BaseReg reg2 ->
        Printf.bprintf b "%%%s" (string_of_register arch reg2)
      | BaseSymbol s ->
        Printf.bprintf b "%s" s
    end;
    Buffer.add_char b ',';
    Printf.bprintf b "%%%s" (string_of_register arch reg1);
    if scale <> 1 then
      Printf.bprintf b ",%d" scale;
    Buffer.add_char b ')'


let bprint_arg arch b ins arg =
  match arg with
  | ConstantInt int ->
    Printf.bprintf b "$%d" int
  | ConstantNat int ->
    Printf.bprintf b "$%nd" int
  | LabelPLT label ->
    Printf.bprintf b "%s@PLT" label
  | LabelGOTPCREL label ->
    Printf.bprintf b "%s@GOTPCREL(%%rip)" label
  | LabelRel (_, label, 0) ->
    Printf.bprintf b "%s" label
  | LabelRel (_, label, i) ->
    Printf.bprintf b "%s+%d" label i
  | LabelDiff (l1, l2) ->
    Printf.bprintf b "%s-%s" l1 l2
  | LabelAbs (label, 0L) ->
    Printf.bprintf b "%s" label
  | LabelAbs (label, iL) ->
    Printf.bprintf b "%s+%Ld" label iL
  | LabelOffset l ->
    (* only in win?? or 32 bits *)
    Printf.bprintf b "$%s" l
  | Direct s ->
    Buffer.add_string b s

  | Reg8 register8 ->
    Printf.bprintf b "%%%s" (string_of_register8 register8)
  | Reg16 register16 ->
    Printf.bprintf b "%%%s" (string_of_register16 register16)
  | Reg32 register32 ->
    Printf.bprintf b "%%%s" (string_of_register32 register32)
  | Reg register ->
    Printf.bprintf b "%%%s" (string_of_register arch register)
  | Regf registerf ->
    Printf.bprintf b "%%%s" (string_of_registerf registerf)

  | Mem ( ptr, r, scale, base, offset) ->
    bprint_arg_mem b arch (ptr, r, scale, base, offset)

let bprint_args arch b instr args =
  match args, instr with
    [], _ -> ()
  | [ Reg _  | Mem _ as arg ],  (CALL _ | JMP _) ->
    tab b; Buffer.add_char b '*'; bprint_arg arch b instr arg
  | [ arg ], _ -> tab b; bprint_arg arch b instr arg
  | [ arg1; arg2 ], _ ->
    tab b; bprint_arg arch b instr arg1;
    Buffer.add_char b ',';
    Buffer.add_char b ' ';
    bprint_arg arch b instr arg2
  | _ -> assert false

let rec string_of_constant = function
  | ConstLabel _
  | ConstInt _
  | ConstNat _
  | Const32 _
  | Const64 _
  | ConstFloat _
    as c -> string_of_simple_constant c
  | ConstAdd (c1, c2) ->
    (string_of_simple_constant c1) ^ " + " ^ (string_of_simple_constant c2)
  | ConstSub (c1, c2) ->
    (string_of_simple_constant c1) ^ " - " ^ (string_of_simple_constant c2)

and string_of_simple_constant = function
  | ConstLabel l -> l
  | ConstInt n -> string_of_int n
  | ConstNat n -> Nativeint.to_string n
  | Const32 n -> Printf.sprintf "0x%lx" n
  | Const64 n -> Printf.sprintf "0x%Lx" n
  | ConstFloat f ->
    let x = Int64.bits_of_float (float_of_string f) in
    Printf.sprintf "0x%Lx" x
  | ConstAdd (c1, c2) ->
    Printf.sprintf "(%s + %s)"
      (string_of_simple_constant c1) (string_of_simple_constant c2)
  | ConstSub (c1, c2) ->
    Printf.sprintf "(%s - %s)"
      (string_of_simple_constant c1) (string_of_simple_constant c2)

let get_suffix s =
  match s with
   | B -> "b"
   | W -> "w"
   | L -> "l"
   | Q -> "q"

let suffix ins s =
  ins ^ (get_suffix s)

let suffix2 ins s1 s2 =
  ins ^ (get_suffix s1) ^ (get_suffix s2)


let list_o arg = match arg with None -> [] | Some arg -> [arg]

let bprint_instr b arch instr =
  begin
    match instr with
      Global s ->
      Printf.bprintf b "\t.globl\t%s" s;
    | Align (_data,n) ->
      Printf.bprintf b "\t.align\t%d" n
    | NewLabel (s, _) ->
      Printf.bprintf b "%s:" s
    | Comment s ->
      Printf.bprintf b "\t\t\t\t(* %s *)" s
    | Specific s ->
      Printf.bprintf b "\t%s" s
    | End -> ()
    | _ ->
      let ins, args =
        match instr with
	  Global _ | Align _ | NewLabel _
        | Comment _ | Specific _
	| End
        | External _ -> assert false

        | Segment Text -> ".text", []
        | Segment Data -> ".data", []
        | Set (arg1, arg2) -> ".set", [ arg1; arg2 ]
        | Space n ->
          if system = S_solaris then
            Printf.sprintf ".zero\t%d" n, []
          else
            Printf.sprintf ".space\t%d" n, []
        | Constant (n, BYTE) ->
          Printf.sprintf ".byte\t%s" (string_of_constant n), []
        | Constant (n, WORD) ->
          if system = S_solaris then
            Printf.sprintf ".value\t%s" (string_of_constant n), []
          else
            Printf.sprintf ".word\t%s" (string_of_constant n), []
        | Constant (n, DWORD) ->
          Printf.sprintf ".long\t%s" (string_of_constant n), []
        | Constant (n, QWORD) ->
	  Printf.sprintf ".quad\t%s" (string_of_constant n), []
        | Constant _ -> assert false
	| Bytes s ->
          if system = S_solaris then
            assert false (* TODO *)
          else
	    Printf.sprintf ".ascii\t\"%s\""
              (string_of_string_literal s), []

        | NOP -> "nop", []
	| NEG arg ->  "neg", [ arg ]
	| ADD (s, arg1, arg2) ->  suffix "add" s, [arg1; arg2]
	| SUB (s, arg1, arg2) -> suffix "sub" s, [arg1; arg2]
	| XOR (s, arg1, arg2) ->  suffix "xor" s, [arg1; arg2]
	| OR (s, arg1, arg2) -> suffix "or" s, [arg1; arg2]
	| AND (s, arg1, arg2) -> suffix "and" s, [arg1; arg2]
	| CMP (s, arg1, arg2) -> suffix "cmp" s, [arg1; arg2]

	| LEAVE -> "leave", []
	| SAR (s, arg1, arg2) -> suffix "sar" s, [arg1; arg2]
	| SHR (s, arg1, arg2) -> suffix "shr" s, [arg1; arg2]
	| SAL (s, arg1, arg2) -> suffix "sal" s, [arg1; arg2]

        | MOVABSQ (arg1, arg2) -> "movabsq", [arg1; arg2]
        | FISTP (s, arg) -> suffix "fistp" s, [ arg ]

	| FSTP (None, arg) -> "fstp", [arg]
	| FSTP (Some s, arg) -> suffix "fstp" s, [arg]
	| FSTPS arg -> "fstps", [ arg ]
        | FILD (s, arg) -> suffix "fild" s, [ arg ]
        | HLT -> "hlt", []

        | FCOMPP -> "fcompp", []
        | FCOMPL arg -> "fcompl", [ arg ]
        | FLDL arg -> "fldl", [ arg ]
        | FLDS arg -> "flds", [ arg ]
        | FNSTSW arg -> "fnstsw", [ arg ]
        | FNSTCW arg -> "fnstcw", [ arg ]
        | FLDCW arg -> "fldcw", [ arg ]

        | FCHS arg -> "fchs", list_o arg
        | FABS  arg -> "fabs", list_o arg

        | FADDL arg -> "faddl", list_o arg
        | FSUBL arg -> "fsubl", list_o arg
        | FMULL arg -> "fmull", list_o arg
        | FDIVL  arg-> "fdivl", list_o arg
        | FSUBRL arg -> "fsubrl", list_o arg
        | FDIVRL arg -> "fdivrl", list_o arg

        | FLD1 -> "fld1", []
        | FPATAN -> "fpatan", []
        | FPTAN -> "fptan", []
        | FCOS -> "fcos", []
        | FLDLN2 -> "fldln2", []
        | FLDLG2 -> "fldlg2", []
        | FXCH arg -> "fxch", list_o arg
        | FYL2X -> "fyl2x", []
        | FSIN -> "fsin", []
        | FSQRT -> "fsqrt", []
        | FLDZ -> "fldz", []

        | FADDP (arg1, arg2)  -> "faddp", [ arg1; arg2 ]
        | FSUBP (arg1, arg2)  -> "fsubp", [ arg1; arg2 ]
        | FMULP (arg1, arg2)  -> "fmulp", [ arg1; arg2 ]
        | FDIVP (arg1, arg2)  -> "fdivp", [ arg1; arg2 ]
        | FSUBRP (arg1, arg2)  -> "fsubrp", [ arg1; arg2 ]
        | FDIVRP (arg1, arg2)  -> "fdivrp", [ arg1; arg2 ]

        | FADDS arg -> "fadds", list_o arg
        | FSUBS arg -> "fsubs", list_o arg
        | FMULS arg -> "fmuls", list_o arg
        | FDIVS arg -> "fdivs", list_o arg
        | FSUBRS arg -> "fsubrs", list_o arg
        | FDIVRS arg -> "fdivrs", list_o arg

	| INC (s, arg) ->  suffix "inc" s, [ arg ]
	| DEC (s, arg) ->  suffix "dec" s, [ arg ]

	| IMUL (s, arg1, arg2) ->  suffix "imul" s, arg1 :: list_o arg2
	| IDIV (s, arg) ->  suffix "idiv" s, [ arg ]

	| MOV (s, arg1, arg2) ->  suffix "mov" s, [arg1; arg2]
	| MOVZX (s1, s2, arg1, arg2) -> suffix2 "movz" s1 s2, [arg1; arg2]
	| MOVSX (s1, s2, arg1, arg2) -> suffix2 "movs" s1 s2, [arg1; arg2]
	| MOVSS (arg1, arg2) ->  "movss", [arg1; arg2]
	| MOVSXD (arg1, arg2) ->  "movslq", [arg1; arg2]

	| MOVSD (arg1, arg2) ->  "movsd", [ arg1 ; arg2 ]
	| ADDSD (arg1, arg2) ->  "addsd", [ arg1 ; arg2 ]
	| SUBSD (arg1, arg2) ->  "subsd", [ arg1 ; arg2 ]
	| MULSD (arg1, arg2) ->  "mulsd", [ arg1 ; arg2 ]
	| DIVSD (arg1, arg2) ->  "divsd", [ arg1 ; arg2 ]
	| SQRTSD (arg1, arg2) -> "sqrtsd", [ arg1; arg2 ]
	| ROUNDSD rounding ->
	  Printf.sprintf "roundsd.%s" (match rounding with
	      RoundDown -> "down"
	    | RoundUp -> "up"
	    | RoundTruncate -> "trunc"
	    | RoundNearest -> "near"), []
	| CVTSS2SD (arg1, arg2) ->  "cvtss2sd", [ arg1; arg2 ]
	| CVTSD2SS (arg1, arg2) ->  "cvtsd2ss", [ arg1; arg2 ]
	| CVTSI2SD (arg1, arg2) ->  "cvtsi2sd", [ arg1; arg2 ]
	| CVTSI2SDQ (arg1, arg2) ->  "cvtsi2sdq", [ arg1; arg2 ]
	| CVTSD2SI (arg1, arg2) ->  "cvtsd2si", [ arg1; arg2 ]
	| CVTTSD2SI (arg1, arg2) ->  "cvttsd2si", [ arg1; arg2 ]
	| UCOMISD (arg1, arg2) ->  "ucomisd", [ arg1; arg2 ]
	| COMISD (arg1, arg2) ->  "comisd", [ arg1; arg2 ]

	| CALL arg  ->  "call", [ arg ]
	| JMP (_, arg) ->  "jmp", [ arg ]
	| RET ->  "ret", []
	| PUSH (s, arg) -> suffix "push" s, [ arg ]
	| POP (s, arg) -> suffix "pop" s, [ arg ]

	| TEST (s, arg1, arg2) ->  suffix "test" s, [ arg1; arg2]
	| SET (condition, arg) ->
	  Printf.sprintf  "set%s" (string_of_condition condition), [ arg ]
	| J (_,condition, arg) ->
	  Printf.sprintf  "j%s" (string_of_condition condition), [arg]


	| CMOV condition ->
	  Printf.sprintf "cmov%s" (string_of_condition condition), []
	| XORPD (arg1, arg2) ->  "xorpd", [ arg1; arg2 ]
	| ANDPD (arg1, arg2) ->  "andpd", [ arg1; arg2 ]
	| MOVLPD (arg1, arg2) ->  "movlpd", [arg1; arg2]
	| MOVAPD (arg1, arg2) ->  "movapd", [arg1; arg2]
	| LEA (s, arg1, arg2) ->  suffix "lea" s, [ arg1; arg2 ]
	| CQTO ->  "cqto", []
        | CLTD -> "cltd", []

        | XCHG (arg1, arg2) -> "xchg", [ arg1; arg2 ]
        | BSWAP arg -> "bswap", [ arg ]
      in
      bprint b ins;
      bprint_args arch b instr args;
  end;
  Buffer.add_string b "\n"

