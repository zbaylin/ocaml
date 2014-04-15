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

let string_of_datatype = function
  | QWORD -> "QWORD"
  | OWORD -> "OWORD"
  | NO -> assert false
  | REAL4 -> "REAL4"
  | REAL8 -> "REAL8"
  | BYTE -> "BYTE"
  | WORD -> "WORD"
  | DWORD -> "DWORD"
  | NEAR -> "NEAR"
  | PROC -> "PROC"

let string_of_datatype_ptr = function
  | QWORD -> "QWORD PTR "
  | OWORD -> "OWORD PTR "
  | NO -> ""
  | REAL4 -> "REAL4 PTR "
  | REAL8 -> "REAL8 PTR "
  | BYTE -> "BYTE PTR "
  | WORD -> "WORD PTR "
  | DWORD -> "DWORD PTR "
  | NEAR -> "NEAR PTR "
  | PROC -> "PROC PTR"

let bprint_arg_mem b arch mem = match mem with

  |  (ptr, reg1, 1, NoBase, 0) ->
    Printf.bprintf b "%s[%s]"
      (string_of_datatype_ptr ptr)
      (string_of_register arch reg1);

  |  (ptr, reg1, 1, NoBase, offset) ->
    Printf.bprintf b "%s[%s%s%d]"
      (string_of_datatype_ptr ptr)
      (string_of_register arch reg1)
      (if offset > 0 then "+" else "")
      offset

  |  (ptr, reg1, scale, NoBase, 0) ->
    Printf.bprintf b "%s[%s*%d]"
      (string_of_datatype_ptr ptr)
      (string_of_register arch reg1)
      scale
  |  (ptr, reg1, scale, NoBase, offset) ->
    Printf.bprintf b "%s[%s*%d%s%d]"
      (string_of_datatype_ptr ptr)
      (string_of_register arch reg1)
      scale
      (if offset > 0 then "+" else "")
      offset
  |  (ptr, reg1, 1, reg2, 0) ->
    Printf.bprintf b "%s[%s+%s]"
      (string_of_datatype_ptr ptr)
      (match reg2 with
	 NoBase -> assert false
       | BaseReg reg2 ->
         string_of_register arch reg2
       | BaseSymbol s -> s)
      (string_of_register arch reg1)
  |  (ptr, reg1, 1, reg2, offset) ->
    Printf.bprintf b "%s[%s+%s%s%d]"
      (string_of_datatype_ptr ptr)
      (match reg2 with
	 NoBase -> assert false
       | BaseReg reg2 ->
         string_of_register arch reg2
       | BaseSymbol s -> s)
      (string_of_register arch reg1)
      (if offset > 0 then "+" else "")
      offset
  |  (ptr, reg1, scale, reg2, 0) ->
    Printf.bprintf b "%s[%s+%s*%d]"
      (string_of_datatype_ptr ptr)
      (match reg2 with
	 NoBase -> assert false
       | BaseReg reg2 ->
         string_of_register arch reg2
       | BaseSymbol s -> s)
      (string_of_register arch reg1)
      scale
  |  (ptr, reg1, scale, reg2, offset) ->
    Printf.bprintf b "%s[%s+%s*%d%s%d]"
      (string_of_datatype_ptr ptr)
      (match reg2 with
	 NoBase -> assert false
       | BaseReg reg2 ->
         string_of_register arch reg2
       | BaseSymbol s -> s)
      (string_of_register arch reg1)
      scale
      (if offset > 0 then "+" else "")
      offset


  let bprint_arg arch b ins arg =
    match arg with
    | ConstantInt int ->
      Printf.bprintf b "%d" int
    | ConstantNat int ->
      begin match ins with
        | MOVABSQ _ ->
          (* force ml64 to use mov reg, imm64 instruction *)
          Printf.bprintf b "0%nxH" int
        | _ ->
          Printf.bprintf b "%nd" int
      end
    | LabelRel (ptr, string, 0) ->
      Printf.bprintf b "%s%s"  (string_of_datatype_ptr ptr) string
    | LabelRel ( ptr, string, i) ->
      Printf.bprintf b "%s%s+%d" (string_of_datatype_ptr ptr) string i
    | LabelDiff (l1, l2) ->
      Printf.bprintf b "%s-%s" l1 l2
    | LabelAbs (string, 0L) ->
      Printf.bprintf b "%s" string
    | LabelAbs (string, iL) ->
      Printf.bprintf b "%s+%Ld" string iL
    | LabelOffset s ->
      Printf.bprintf b "OFFSET %s" s
    | Direct s ->
      Buffer.add_string b s
    | Reg8 register8 ->
      Printf.bprintf b "%s" (string_of_register8 register8)
    | Reg16 register16 ->
      Printf.bprintf b "%s" (string_of_register16 register16)
    | Reg32 register32 ->
      Printf.bprintf b "%s" (string_of_register32 register32)
    | Reg register ->
      Printf.bprintf b "%s" (string_of_register arch register)
    | Regf registerf ->
      Printf.bprintf b "%s" (string_of_registerf registerf)

    | LabelPLT _ -> assert false
    | LabelGOTPCREL _ -> assert false

    | Mem ( ptr, r, scale, base, offset) ->
      bprint_arg_mem b arch (ptr, r, scale, base, offset)

let bprint_args arch b instr args =
  match args with
  | [] -> ()
  | [ arg ] -> tab b; bprint_arg arch b instr arg
  | [ arg2; arg1 ] ->
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
  | ConstLabel l -> if l = "." then  "THIS BYTE"      else l
  | ConstInt n -> string_of_int n
  | ConstNat n -> Nativeint.to_string n
  | Const32 n -> Printf.sprintf "0%lxh" n
  | Const64 n -> Printf.sprintf "0%Lxh" n
  | ConstFloat s ->
    (* MASM doesn't like floating-point constants such as 2e9.
        Turn them into 2.0e9. *)
    let pos_e = ref (-1) and pos_dot = ref (-1) in
    for i = 0 to String.length s - 1 do
      match s.[i] with
        'e'|'E' -> pos_e := i
      | '.'     -> pos_dot := i
      | _       -> ()
    done;
    if !pos_dot < 0 && !pos_e >= 0 then begin
      Printf.sprintf "%s.0%s"
        (String.sub s 0 !pos_e)
        (String.sub s !pos_e (String.length s - !pos_e))
    end else
      s
  | ConstAdd (c1, c2) ->
    Printf.sprintf "(%s + %s)"
      (string_of_simple_constant c1) (string_of_simple_constant c2)
  | ConstSub (c1, c2) ->
    Printf.sprintf "(%s - %s)"
      (string_of_simple_constant c1) (string_of_simple_constant c2)

let buf_bytes_directive b directive s =
   let pos = ref 0 in
   for i = 0 to String.length s - 1 do
     if !pos = 0
     then begin
       if i > 0 then Buffer.add_char b '\n';
       Buffer.add_char b '\t';
       Buffer.add_string b directive;
       Buffer.add_char b '\t';
     end
     else Buffer.add_char b ',';
     Printf.bprintf b "%d" (Char.code s.[i]);
     incr pos;
     if !pos >= 16 then begin pos := 0 end
   done



let list_o arg = match arg with None -> [] | Some arg -> [arg]

let bprint_instr_name b arch instr =
  match instr with
    Global s ->
    Printf.bprintf b "\tPUBLIC\t%s" s
  | Align (_data,n) ->
    Printf.bprintf b "\tALIGN\t%d" n
  | NewLabel (s, NO) ->
    Printf.bprintf b "%s:" s
  | NewLabel (s, ptr) ->
    Printf.bprintf b "%s LABEL %s" s (string_of_datatype ptr)
  | Comment s ->
    Printf.bprintf b " ; %s " s
  | Specific s ->
    Printf.bprintf b "\t%s" s

  | Segment Text -> Buffer.add_string b "\t.CODE"
  | Segment Data -> Buffer.add_string b "\t.DATA"

  | Constant (n, ptr) ->
    Printf.bprintf b "\t%s\t%s" (string_of_datatype ptr) (string_of_constant n)

  | External (s, ptr) ->
    Printf.bprintf b "\tEXTRN\t%s: %s" s (string_of_datatype ptr)

  | End ->
    Printf.bprintf b "END"

  | Space n ->
    Printf.bprintf b "\tBYTE\t%d DUP (?)" n

  | Bytes s -> buf_bytes_directive b "BYTE" s

  | _ ->
    let name, args =
      match instr with
	Global _ | Align _
      | NewLabel _ | Comment _
      | Specific _ | End
      | Segment _ | Constant _
      | Bytes _ | Space _ | External _
        -> assert false
      | Set (arg1, arg2) -> ".set", [ arg1; arg2 ]


      | NEG arg ->  "neg", [arg]
      | NOP -> "nop", []
      | ADD (_s, arg1, arg2) ->  "add", [arg1; arg2]
      | SUB (_s, arg1, arg2) ->   "sub", [arg1; arg2]
      | XOR (_s, arg1, arg2) ->   "xor", [arg1; arg2]
      | OR (_s, arg1, arg2) ->    "or", [arg1; arg2]
      | AND (_s, arg1, arg2) ->   "and", [arg1; arg2]
      | CMP (_s, arg1, arg2) ->  "cmp", [arg1; arg2]

      | MOVABSQ (arg1, arg2) -> "mov", [ arg1; arg2]

      | LEAVE -> "leave", []
      | SAR (_s, arg1, arg2) ->  "sar", [arg1; arg2]
      | SHR (_s, arg1, arg2) ->  "shr", [arg1; arg2]
      | SAL (_s, arg1, arg2) ->  "sal", [arg1; arg2]

      | FSTP (_, arg) -> "fstp", [ arg ]
      | FSTPS arg -> "fstps", [ arg]
      | FILD (_, arg) -> "fild", [arg]
      | FCOMPP -> "fcompp", []
      | FCOMPL arg -> "fcomp", [ arg ]
      | FLDL arg -> "fld", [ arg ]
      | FLDS arg -> "flds", [ arg]
      | FLDCW arg -> "fldcw", [ arg ]
      | FISTP (_, arg) -> "fistp", [ arg]

      | FNSTSW arg -> "fnstsw", [ arg ]
      | FNSTCW arg -> "fnstcw", [ arg ]

      | FCHS arg -> "fchs", list_o arg
      | FABS arg -> "fabs", list_o arg
      | FADDL arg | FADDS arg -> "fadd", list_o arg
      | FSUBL arg | FSUBS arg -> "fsub", list_o arg
      | FMULL arg | FMULS arg -> "fmul", list_o arg
      | FDIVL arg | FDIVS  arg -> "fdiv", list_o arg
      | FSUBRL arg | FSUBRS arg -> "fsubr", list_o arg
      | FDIVRL arg | FDIVRS arg -> "fdivr", list_o arg

      | FADDP (arg1, arg2)  -> "faddp", [ arg1; arg2 ]
      | FSUBP (arg1, arg2)  -> "fsubp", [ arg1; arg2 ]
      | FMULP (arg1, arg2)  -> "fmulp", [ arg1; arg2 ]
      | FDIVP (arg1, arg2)  -> "fdivp", [ arg1; arg2 ]
      | FSUBRP (arg1, arg2)  -> "fsubrp", [ arg1; arg2 ]
      | FDIVRP (arg1, arg2)  -> "fdivrp", [ arg1; arg2 ]

      | INC (_s, arg) ->  "inc", [ arg ]
      | DEC (_s, arg) ->  "dec", [ arg ]

      | IMUL (_s, arg1, arg2) ->  "imul", arg1 :: list_o arg2
      | IDIV (_s, arg) ->  "idiv", [ arg ]
      | HLT -> assert false
      | MOV (_s, arg1, arg2) ->  "mov", [ arg1; arg2]

      | MOVZX (_, _, arg1, arg2) ->  "movzx", [ arg1; arg2]
      | MOVSX (_, _, arg1, arg2) ->  "movsx",  [ arg1; arg2]
      | MOVSS (arg1, arg2) ->  "movss", [ arg1; arg2 ]
      | MOVSXD (arg1, arg2) ->  "movsxd", [ arg1; arg2 ]

      | MOVSD (arg1, arg2) ->  "movsd", [ arg1; arg2 ]
      | ADDSD (arg1, arg2) ->  "addsd", [ arg1 ; arg2 ]
      | SUBSD (arg1, arg2) ->  "subsd", [ arg1 ; arg2 ]
      | MULSD (arg1, arg2) ->  "mulsd", [ arg1 ; arg2 ]
      | DIVSD (arg1, arg2) ->  "divsd", [ arg1 ; arg2 ]
      | SQRTSD (arg1, arg2) -> "sqrtsd", [ arg1; arg2]
      | ROUNDSD rounding ->
	Printf.sprintf "roundsd.%s" (match rounding with
	    RoundDown -> "down"
	  | RoundUp -> "up"
	  | RoundTruncate -> "trunc"
	  | RoundNearest -> "near"), []
      | CVTSS2SD (arg1, arg2) ->  "cvtss2sd", [ arg1; arg2 ]
      | CVTSD2SS (arg1, arg2) ->  "cvtsd2ss", [ arg1; arg2 ]
      | CVTSI2SD (arg1, arg2) ->  "cvtsi2sd", [ arg1; arg2 ]
      | CVTSD2SI (arg1, arg2) ->  "cvtsd2si", [ arg1; arg2 ]
      | CVTSI2SDQ (arg1, arg2) ->  "cvtsi2sdq", [ arg1; arg2 ]
      | CVTTSD2SI (arg1, arg2) ->  "cvttsd2si", [ arg1; arg2 ]
      | UCOMISD (arg1, arg2) ->  "ucomisd", [ arg1; arg2]
      | COMISD (arg1, arg2) ->  "comisd", [arg1; arg2]

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

      | CALL arg  ->  "call", [ arg ]
      | JMP (_, arg) ->  "jmp", [ arg]
      | RET ->  "ret", []
      | PUSH (_, arg) ->  "push", [arg]
      | POP (_, arg) ->  "pop", [arg]

      | TEST (_s, arg1, arg2) ->  "test", [arg1; arg2]
      | SET (condition, arg) ->
	Printf.sprintf  "set%s" (string_of_condition condition), [ arg ]
      | J (_,condition, arg) ->
	Printf.sprintf  "j%s" (string_of_condition condition), [ arg ]

      | CMOV condition ->
	Printf.sprintf "cmov%s" (string_of_condition condition), []
      | XORPD (arg1, arg2) ->  "xorpd", [ arg1; arg2 ]
      | ANDPD (arg1, arg2) ->  "andpd", [ arg1; arg2 ]
      | MOVLPD (arg1, arg2) ->  "movlpd", [ arg1; arg2 ]
      | MOVAPD (arg1, arg2) ->  "movapd", [ arg1; arg2 ]
      | CLTD -> "cdq", []

      | LEA (_s, arg1, arg2) ->  "lea", [arg1; arg2]
      | CQTO ->  "cqo", []
      | XCHG (arg1, arg2) -> "xchg", [ arg1; arg2 ]
      | BSWAP arg -> "bswap", [ arg ]

    in
    bprint b name;
    bprint_args arch b instr args;
()

let bprint_instr b arch instr =
  bprint_instr_name b arch instr;
  Buffer.add_string b "\n"

