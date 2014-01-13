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

  let bprint_arg arch b ins arg =
    match arg with
    | Constant int ->
      Printf.bprintf b "%d" int
    | ConstantNat int when ins.instr = MOVABSQ ->
  (* force ml64 to use mov reg, imm64 instruction *)
      Printf.bprintf b "0%nxH" int
    | ConstantNat int ->
      Printf.bprintf b "%nd" int
    | LabelPLT _ -> assert false
    | LabelGOTPCREL _ -> assert false
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
      Printf.bprintf b "%s" (arch.string_of_register register)
    | Regf registerf ->
      Printf.bprintf b "%s" (string_of_registerf registerf)

    | Mem (ptr, reg1, 1, NoBase, 0) ->
      Printf.bprintf b "%s[%s]"
        (string_of_datatype_ptr ptr)
        (arch.string_of_register reg1);

    | Mem (ptr, reg1, 1, NoBase, offset) ->
      Printf.bprintf b "%s[%s%s%d]"
                  (string_of_datatype_ptr ptr)
                  (arch.string_of_register reg1)
                  (if offset > 0 then "+" else "")
                  offset

    | Mem (ptr, reg1, scale, NoBase, 0) ->
      Printf.bprintf b "%s[%s*%d]"
        (string_of_datatype_ptr ptr)
        (arch.string_of_register reg1)
        scale
    | Mem (ptr, reg1, scale, NoBase, offset) ->
      Printf.bprintf b "%s[%s*%d%s%d]"
        (string_of_datatype_ptr ptr)
        (arch.string_of_register reg1)
        scale
        (if offset > 0 then "+" else "")
        offset
    | Mem (ptr, reg1, 1, reg2, 0) ->
      Printf.bprintf b "%s[%s+%s]"
        (string_of_datatype_ptr ptr)
        (match reg2 with
	   NoBase -> assert false
         | BaseReg reg2 ->
           arch.string_of_register reg2
        | BaseSymbol s -> s)
        (arch.string_of_register reg1)
    | Mem (ptr, reg1, 1, reg2, offset) ->
      Printf.bprintf b "%s[%s+%s%s%d]"
        (string_of_datatype_ptr ptr)
        (match reg2 with
	   NoBase -> assert false
         | BaseReg reg2 ->
           arch.string_of_register reg2
        | BaseSymbol s -> s)
        (arch.string_of_register reg1)
        (if offset > 0 then "+" else "")
        offset
    | Mem (ptr, reg1, scale, reg2, 0) ->
      Printf.bprintf b "%s[%s+%s*%d]"
        (string_of_datatype_ptr ptr)
        (match reg2 with
	   NoBase -> assert false
         | BaseReg reg2 ->
           arch.string_of_register reg2
        | BaseSymbol s -> s)
        (arch.string_of_register reg1)
        scale
    | Mem (ptr, reg1, scale, reg2, offset) ->
      Printf.bprintf b "%s[%s+%s*%d%s%d]"
        (string_of_datatype_ptr ptr)
        (match reg2 with
	   NoBase -> assert false
         | BaseReg reg2 ->
           arch.string_of_register reg2
        | BaseSymbol s -> s)
        (arch.string_of_register reg1)
        scale
        (if offset > 0 then "+" else "")
        offset

let bprint_args arch b instr =
  match instr with
    { args = [||] } -> ()
  | { args = [|arg|] } -> tab b; bprint_arg arch b instr arg
  | { args = [|arg2; arg1|] } ->
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

let bprint_instr_name b arch instr =
  match instr.instr with
    Global s ->
    Printf.bprintf b "\tPUBLIC\t%s" s
  | Align (data,n) ->
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
    let name =
      match instr.instr with
	Global _ | Align _
      | NewLabel _ | Comment _
      | Specific _ | End
      | Segment _ | Constant _
      | Bytes _ | Space _ | External _
        -> assert false
      | Set -> ".set"
      | NOP -> "nop"

      | NEG ->  "neg"
      | ADD _ ->  "add"
      | SUB _ ->   "sub"
      | XOR _ ->   "xor"
      | OR _ ->    "or"
      | AND _ ->   "and"
      | CMP _ ->  "cmp"

      | MOVABSQ -> "mov"

      | LEAVE -> "leave"
      | SAR _ ->  "sar"
      | SHR _ ->  "shr"
      | SAL _ ->  "sal"

      | FSTP _ -> "fstp"
      | FSTPS -> "fstps"
      | FILD _ -> "fild"
      | FCOMPP -> "fcompp"
      | FCOMPL -> "fcomp"
      | FLDL -> "fld"
      | FLDS -> "flds"
      | FLDCW -> "fldcw"
      | FISTP _ -> "fistp"

      | FNSTSW -> "fnstsw"
      | FNSTCW -> "fnstcw"

      | FCHS -> "fchs"
      | FABS -> "fabs"
      | FADDL | FADDP | FADDS -> "fadd"
      | FSUBL | FSUBP | FSUBS -> "fsub"
      | FMULL | FMULP | FMULS -> "fmul"
      | FDIVL | FDIVP | FDIVS  -> "fdiv"
      | FSUBRL | FSUBRP | FSUBRS -> "fsubr"
      | FDIVRL | FDIVRP | FDIVRS -> "fdivr"

      | INC _ ->  "inc"
      | DEC _ ->  "dec"

      | IMUL _ ->  "imul"
      | IDIV _ ->  "idiv"
      | HLT -> assert false
      | MOV _ ->  "mov"

      | MOVZX _ ->  "movzx"
      | MOVSX _ ->  "movsx"
      | MOVSS ->  "movss"
      | MOVSXD ->  "movsxd"

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
      | CVTSD2SI ->  "cvtsd2si"
      | CVTSI2SDQ ->  "cvtsi2sdq"
      | CVTTSD2SI ->  "cvttsd2si"
      | UCOMISD ->  "ucomisd"
      | COMISD ->  "comisd"

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

      | CALL  ->  "call"
      | JMP _ ->  "jmp"
      | RET ->  "ret"
      | PUSH _ ->  "push"
      | POP _ ->  "pop"

      | TEST _ ->  "test"
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
      | CLTD -> "cdq"

      | LEA _ ->  "lea"
      | CQTO ->  "cqo"
      | XCHG -> "xchg"
      | BSWAP -> "bswap"

    in
    bprint b name

let bprint_instr b arch instr =
  bprint_instr_name b arch instr;
  bprint_args arch b instr;
  Buffer.add_string b "\n"

