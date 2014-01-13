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

let bprint_arg arch b ins arg =
  match arg with
  | Constant int ->
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
    Printf.bprintf b "%%%s" (arch.string_of_register register)
  | Regf registerf ->
    Printf.bprintf b "%%%s" (string_of_registerf registerf)

  | Mem (_, reg1, 1, BaseSymbol s, 0) ->
    Printf.bprintf b "%s(%%%s)" s (arch.string_of_register reg1)

  | Mem (_, reg1, 1, BaseSymbol s, offset) ->
    if offset < 0 then
      Printf.bprintf b "%s%d(%%%s)" s offset (arch.string_of_register reg1)
    else
      Printf.bprintf b "%s+%d(%%%s)" s offset (arch.string_of_register reg1)

  | Mem (_, reg1, scale, BaseSymbol s, offset) ->
    if offset = 0 then
      Printf.bprintf b "%s(,%%%s,%d)" s
        (arch.string_of_register reg1) scale
    else    if offset < 0 then
      Printf.bprintf b "%s%d(,%%%s,%d)" s
        offset (arch.string_of_register reg1) scale
    else
      Printf.bprintf b "%s+%d(,%%%s,%d)" s
        offset (arch.string_of_register reg1) scale

  | Mem (_, reg1, 1, NoBase, 0) ->
    Buffer.add_char b '(';
    Printf.bprintf b "%%%s" (arch.string_of_register reg1);
    Buffer.add_char b ')'

  | Mem (_, reg1, 1, NoBase, offset) ->
    if offset <> 0 then begin
      if offset < 0 then
        Printf.bprintf b "-%d" (-offset)
      else
        Printf.bprintf b "%d" offset
    end;
    Buffer.add_char b '(';
    Printf.bprintf b "%%%s" (arch.string_of_register reg1);
    Buffer.add_char b ')'

  | Mem (_, reg1, scale, reg2, offset) ->
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
        Printf.bprintf b "%%%s" (arch.string_of_register reg2)
      | BaseSymbol s ->
        Printf.bprintf b "%s" s
    end;
    Buffer.add_char b ',';
    Printf.bprintf b "%%%s" (arch.string_of_register reg1);
    if scale <> 1 then
      Printf.bprintf b ",%d" scale;
    Buffer.add_char b ')'

let bprint_args arch b instr =
  match instr with
    { args = [||] } -> ()
  | { args = [|Reg _ | Mem _ as arg|]; instr = (CALL | JMP _) } ->
    tab b; Buffer.add_char b '*'; bprint_arg arch b instr arg
  | { args = [|arg|] } -> tab b; bprint_arg arch b instr arg
  | { args = [|arg1; arg2|] } ->
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

let suff arch ins =
  if arch.arch64 then ins ^ "q" else ins ^ "l"

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


let bprint_instr b arch instr =
  begin
    match instr.instr with
      Global s ->
      Printf.bprintf b "\t.globl\t%s" s;
    | Align (data,n) ->
      Printf.bprintf b "\t.align\t%d" n
    | NewLabel (s, _) ->
      Printf.bprintf b "%s:" s
    | Comment s ->
      Printf.bprintf b "\t\t\t\t(* %s *)" s
    | Specific s ->
      Printf.bprintf b "\t%s" s
    | End -> ()
    | _ ->
      let ins =
        match instr.instr with
	  Global _ | Align _ | NewLabel _
        | Comment _ | Specific _
	| End
        | External _ -> assert false

        | Segment Text -> ".text"
        | Segment Data -> ".data"
        | Set -> ".set"
        | Space n ->
          if system = S_solaris then
            Printf.sprintf ".zero\t%d" n
          else
            Printf.sprintf ".space\t%d" n
        | Constant (n, BYTE) ->
          Printf.sprintf ".byte\t%s" (string_of_constant n)
        | Constant (n, WORD) ->
          if system = S_solaris then
            Printf.sprintf ".value\t%s" (string_of_constant n)
          else
            Printf.sprintf ".word\t%s" (string_of_constant n)
        | Constant (n, DWORD) ->
          Printf.sprintf ".long\t%s" (string_of_constant n)
        | Constant (n, QWORD) ->
	  Printf.sprintf ".quad\t%s" (string_of_constant n)
        | Constant _ -> assert false
	| Bytes s ->
          if system = S_solaris then
            assert false (* TODO *)
          else
	    Printf.sprintf ".ascii\t\"%s\""
              (string_of_string_literal s)

        | NOP -> "nop"
	| NEG ->  "neg"
	| ADD s ->  suffix "add" s
	| SUB s -> suffix "sub" s
	| XOR s ->  suffix "xor" s
	| OR s -> suffix "or" s
	| AND s -> suffix "and" s
	| CMP s -> suffix "cmp" s

	| LEAVE -> "leave"
	| SAR s -> suffix "sar" s
	| SHR s -> suffix "shr" s
	| SAL s -> suffix "sal" s

        | MOVABSQ -> "movabsq"
        | FISTP s -> suffix "fistp" s

	| FSTP None -> "fstp"
	| FSTP (Some s) -> suffix "fstp" s
	| FSTPS -> "fstps"
        | FILD s -> suffix "fild" s
        | HLT -> "hlt"

        | FCOMPP -> "fcompp"
        | FCOMPL -> "fcompl"
        | FLDL -> "fldl"
        | FLDS -> "flds"
        | FNSTSW -> "fnstsw"
        | FNSTCW -> "fnstcw"
        | FLDCW -> "fldcw"

        | FCHS -> "fchs"
        | FABS -> "fabs"

        | FADDL -> "faddl"
        | FSUBL -> "fsubl"
        | FMULL -> "fmull"
        | FDIVL -> "fdivl"
        | FSUBRL -> "fsubrl"
        | FDIVRL -> "fdivrl"

        | FLD1 -> "fld1"
        | FPATAN -> "fpatan"
        | FPTAN -> "fptan"
        | FCOS -> "fcos"
        | FLDLN2 -> "fldln2"
        | FLDLG2 -> "fldlg2"
        | FXCH -> "fxch"
        | FYL2X -> "fyl2x"
        | FSIN -> "fsin"
        | FSQRT -> "fsqrt"
        | FLDZ -> "fldz"

        | FADDP -> "faddp"
        | FSUBP -> "fsubp"
        | FMULP -> "fmulp"
        | FDIVP -> "fdivp"
        | FSUBRP -> "fsubrp"
        | FDIVRP -> "fdivrp"

        | FADDS -> "fadds"
        | FSUBS -> "fsubs"
        | FMULS -> "fmuls"
        | FDIVS -> "fdivs"
        | FSUBRS -> "fsubrs"
        | FDIVRS -> "fdivrs"

	| INC s ->  suffix "inc" s
	| DEC s ->  suffix "dec" s

	| IMUL s ->  suffix "imul" s
	| IDIV s ->  suffix "idiv" s

	| MOV s ->  suffix "mov" s
	| MOVZX (s1, s2) -> suffix2 "movz" s1 s2
	| MOVSX (s1, s2) -> suffix2 "movs" s1 s2
	| MOVSS ->  "movss"
	| MOVSXD ->  "movslq"

	| MOVSD ->  "movsd"
	| ADDSD ->  "addsd"
	| SUBSD ->  "subsd"
	| MULSD ->  "mulsd"
	| DIVSD ->  "divsd"
	| SQRTSD -> "sqrtsd"
	| ROUNDSD rounding ->
	  Printf.sprintf "roundsd.%s" (match rounding with
	      RoundDown -> "down"
	    | RoundUp -> "up"
	    | RoundTruncate -> "trunc"
	    | RoundNearest -> "near")
	| CVTSS2SD ->  "cvtss2sd"
	| CVTSD2SS ->  "cvtsd2ss"
	| CVTSI2SD ->  "cvtsi2sd"
	| CVTSI2SDQ ->  "cvtsi2sdq"
	| CVTSD2SI ->  "cvtsd2si"
	| CVTTSD2SI ->  "cvttsd2si"
	| UCOMISD ->  "ucomisd"
	| COMISD ->  "comisd"

	| CALL  ->  "call"
	| JMP _ ->  "jmp"
	| RET ->  "ret"
	| PUSH s -> suffix "push" s
	| POP s -> suffix "pop" s

	| TEST s ->  suffix "test" s
	| SET condition ->
	  Printf.sprintf  "set%s" (string_of_condition condition)
	| J (_,condition) ->
	  Printf.sprintf  "j%s" (string_of_condition condition)

	| CMOV condition ->
	  Printf.sprintf "cmov%s" (string_of_condition condition)
	| XORPD ->  "xorpd"
	| ANDPD ->  "andpd"
	| MOVLPD ->  "movlpd"
	| MOVAPD ->  "movapd"

	| LEA s ->  suffix "lea" s
	| CQTO ->  "cqto"
        | CLTD -> "cltd"

        | XCHG -> "xchg"
        | BSWAP -> "bswap"
      in
      bprint b ins
  end;
  bprint_args arch b instr;
  Buffer.add_string b "\n"

