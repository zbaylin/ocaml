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

module StringSet = Set.Make(struct type t = string let compare = compare end)
module StringMap = Map.Make(struct type t = string let compare = compare end)
module IntSet = Set.Make(struct type t = int let compare x y = x - y end)
module IntMap = Map.Make(struct type t = int let compare x y = x - y end)

type condition =
  | O
  | NO
  | B | C | NAE
  | NB | NC | AE
  | Z | E
  | NZ | NE
  | BE | NA
  | NBE | A
  | S
  | NS
  | P | PE
  | NP | PO
  | L | NGE
  | NL | GE
  | LE | NG
  | NLE | G

type locality =
    Loc_unknown of int (* position of instruction *)
  | Loc_near (* 8 bits offset *)
  | Loc_far  (* 32 bits offset *)

type rounding =
  | RoundUp
  | RoundDown
  | RoundNearest
  | RoundTruncate

type constant =
  | ConstInt of int
  | ConstNat of nativeint
  | Const32 of int32
  | Const64 of int64
  | ConstFloat of string
  | ConstLabel of string
  | ConstAdd of constant * constant
  | ConstSub of constant * constant

type segment_type = Text | Data

type data_type = (* only used for MASM *)
  NO | REAL4 | REAL8 | BYTE | WORD | DWORD | QWORD | NEAR | OWORD | PROC
(* PROC could be a display for NEAR on 32 bits ? *)

type suffix = B | W | L | Q

type register64 =
  | RAX | RBX | RDI | RSI | RDX | RCX | RBP | RSP
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | RIP

type register8 =
    AL | BL | CL | DL
  | AH | BH | CH | DH
  | DIL | SIL | R8B | R9B
  | R10B | R11B | BPL | R12B | R13B | SPL | R14B | R15B

type register16 =
    AX | BX | DI | SI | DX | CX | SP | BP
  | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W

type register32 =
    EAX | EBX | EDI | ESI | EDX | ECX | R8D | R9D
  | R10D | R11D | EBP | R12D | R13D | R14D | R15D | ESP

type registerf = XMM of int | TOS | ST of int

type 'reg base =
    NoBase
  | BaseReg of 'reg
  | BaseSymbol of string

type arg =
  | ConstantInt of int
  | ConstantNat of nativeint
  | LabelRel of data_type * string * int
  | LabelDiff of string * string (* label - label *)
  | LabelAbs of string * int64
  | LabelPLT of string
  | LabelGOTPCREL of string
  | LabelOffset of string (* for non pic-code ? *)
  | Direct of string

  | Reg8 of register8
  | Reg16 of register16
  | Reg32 of register32
  | Reg of register64 (* register with architecture size *)
  | Regf of registerf
  | Mem of
      data_type *
       register64 * (* scale *) int *
        register64 base * (* offset *) int

type instruction =
    Segment of segment_type
  | Global of string
  | Constant of constant * data_type
  | Align of bool * int
  | NewLabel of string * data_type
  | Bytes of string
  | Space of int
  | Comment of string
  | Specific of string
  | External of string * data_type
  | Set of arg * arg
  | End

  | NOP

  | ADD of suffix * arg * arg
  | SUB of suffix * arg * arg
  | XOR of suffix * arg * arg
  | OR of suffix * arg * arg
  | AND of suffix * arg * arg
  | CMP of suffix * arg * arg

  | FSTP of suffix option * arg
  | FSTPS of arg

  | FCOMPP
  | FCOMPL of arg
  | FLDL of arg
  | FLDS of arg
  | FNSTSW of arg
  | FNSTCW of arg
  | FLDCW of arg

  | FCHS of arg option
  | FABS of arg option
  | FADDL of arg option
  | FSUBL of arg option
  | FMULL of arg option
  | FDIVL of arg option
  | FSUBRL of arg option
  | FDIVRL of arg option
  | FILD of suffix * arg
  | FISTP of suffix * arg
  | HLT

  | FADDP of arg * arg
  | FSUBP of arg * arg
  | FMULP of arg * arg
  | FDIVP of arg * arg
  | FSUBRP of arg * arg
  | FDIVRP of arg * arg

  | FADDS of arg option
  | FSUBS of arg option
  | FMULS of arg option
  | FDIVS of arg option
  | FSUBRS of arg option
  | FDIVRS of arg option

  | FLD1
  | FPATAN
  | FPTAN
  | FCOS
  | FLDLN2
  | FLDLG2
  | FXCH of arg option
  | FYL2X
  | FSIN
  | FSQRT
  | FLDZ

  | SAR of suffix * arg * arg
  | SHR of suffix * arg * arg
  | SAL of suffix * arg * arg
  | INC of suffix * arg
  | DEC of suffix * arg
  | IMUL of suffix * arg * arg option
  | IDIV of suffix * arg
  | PUSH of suffix * arg
  | POP of suffix * arg

  | MOV of suffix * arg * arg

  | MOVZX of suffix * suffix * arg * arg
  | MOVSX of suffix * suffix * arg * arg
  | MOVSS of arg * arg
  | MOVSXD (* MOVSLQ *) of arg * arg

  | MOVSD of arg * arg
  | ADDSD of arg * arg
  | SUBSD of arg * arg
  | MULSD of arg * arg
  | DIVSD of arg * arg
  | SQRTSD of arg * arg
  | ROUNDSD of rounding
  | NEG of arg

  | CVTSS2SD of arg * arg
  | CVTSD2SS of arg * arg
  | CVTSI2SD of arg * arg
  | CVTSI2SDQ of arg * arg
  | CVTSD2SI of arg * arg
  | CVTTSD2SI of arg * arg
  | UCOMISD of arg * arg
  | COMISD of arg * arg

  | CALL of arg
  | JMP of locality ref * arg
  | RET

  | TEST of suffix * arg * arg
  | SET of condition * arg
  | J of locality ref * condition * arg

  | CMOV of condition
  | XORPD of arg * arg
  | ANDPD of arg * arg
  | MOVAPD of arg * arg
  | MOVLPD of arg * arg
  | MOVABSQ of arg * arg

  | CLTD
  | LEA of suffix * arg * arg
  | CQTO
  | LEAVE

  | XCHG of arg * arg
  | BSWAP of arg

type segment = {
  mutable seg_instrs : instruction list;
}


type system =
(* 32 bits and 64 bits *)
  | S_macosx
  | S_gnu
  | S_cygwin

(* 32 bits only *)
  | S_solaris
  | S_win32
  | S_linux_elf
  | S_bsd_elf
  | S_beos
  | S_mingw

(* 64 bits only *)
  | S_win64
  | S_linux
  | S_mingw64

  | S_unknown

let system = match Config.system with
  | "macosx" -> S_macosx
  | "solaris" -> S_solaris
  | "win32" -> S_win32
  | "linux_elf" -> S_linux_elf
  | "bsd_elf" -> S_bsd_elf
  | "beos" -> S_beos
  | "gnu" -> S_gnu
  | "cygwin" -> S_cygwin
  | "mingw" -> S_mingw
  | "mingw64" -> S_mingw64
  | "win64" -> S_win64
  | "linux" -> S_linux

  | _ -> S_unknown


let new_segment () = {
  seg_instrs = [];
}

let clear_segment s =
  s.seg_instrs <- []

let emit seg ins =
  seg.seg_instrs <- ins :: seg.seg_instrs

let string_of_string_literal s =
  let b = Buffer.create (String.length s + 2) in
  let last_was_escape = ref false in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c >= '0' && c <= '9' then
      if !last_was_escape
      then Printf.bprintf b "\\%o" (Char.code c)
      else Buffer.add_char b c
    else if c >= ' ' && c <= '~' && c <> '"' (* '"' *) && c <> '\\' then begin
      Buffer.add_char b c;
      last_was_escape := false
    end else begin
      Printf.bprintf b "\\%o" (Char.code c);
      last_was_escape := true
    end
  done;
  Buffer.contents b

let string_of_symbol prefix s =
  let b = Buffer.create (1 + String.length s) in
  Buffer.add_string b prefix;
  String.iter
    (function
     | ('A'..'Z' | 'a'..'z' | '0'..'9' | '_') as c -> Buffer.add_char b c
     | c -> Printf.bprintf b "$%02x" (Char.code c)
    )
    s;
  Buffer.contents b


let string_of_register64 reg64 =
    match reg64 with
    | RAX -> "rax"
    | RBX -> "rbx"
    | RDI -> "rdi"
    | RSI -> "rsi"
    | RDX -> "rdx"
    | RCX -> "rcx"
    | RBP -> "rbp"
    | RSP -> "rsp"
    | R8 -> "r8"
    | R9 -> "r9"
    | R10 -> "r10"
    | R11 -> "r11"
    | R12 -> "r12"
    | R13 -> "r13"
    | R14 -> "r14"
    | R15 -> "r15"
    | RIP -> "rip"

let string_of_register arch64 reg =
  if arch64 then string_of_register64 reg else
  match reg with
    RAX -> "eax"
  | RBX -> "ebx"
  | RDI -> "edi"
  | RSI -> "esi"
  | RDX -> "edx"
  | RCX -> "ecx"
  | RSP -> "esp"
  | RBP -> "ebp"
  | R8 -> "r8d"
  | R9 -> "r9d"
  | R10 -> "r10d"
  | R11 -> "r11d"
  | R12 -> "r12d"
  | R13 -> "r13d"
  | R14 -> "r14d"
  | R15 -> "r15d"
  | RIP -> assert false

let register reg32 =
  match reg32 with
    EAX -> RAX
  | EBX -> RBX
  | EDI -> RDI
  | ESI -> RSI
  | EDX -> RDX
  | ECX -> RCX
  | ESP -> RSP
  | EBP -> RBP
  | R8D -> R8
  | R9D -> R9
  | R10D -> R10
  | R11D -> R11
  | R12D -> R12
  | R13D -> R13
  | R14D -> R14
  | R15D -> R15

let string_of_register8 reg8 = match reg8 with
  | AL -> "al"
  | BL -> "bl"
  | DL -> "dl"
  | CL -> "cl"
  | AH -> "ah"
  | BH -> "bh"
  | CH -> "ch"
  | DH -> "dh"
  | DIL -> "dil"
  | SIL -> "sil"
  | R8B -> "r8b"
  | R9B -> "r9b"
  | R10B -> "r10b"
  | R11B -> "r11b"
  | BPL -> "bpl"
  | R12B -> "r12b"
  | R13B -> "r13b"
  | SPL -> "spl"
  | R14B -> "r14b"
  | R15B -> "r15b"

let string_of_register16 reg16 =
  match reg16 with
    AX -> "ax"
  | BX -> "bx"
  | DI -> "di"
  | SI -> "si"
  | DX -> "dx"
  | CX -> "cx"
  | SP -> "sp"
  | BP -> "bp"
  | R8W -> "r8w"
  | R9W -> "r9w"
  | R10W -> "r10w"
  | R11W -> "r11w"
  | R12W -> "r12w"
  | R13W -> "r13w"
  | R14W -> "r14w"
  | R15W -> "r15w"

let string_of_register32 reg32 =
  match reg32 with
    EAX -> "eax"
  | EBX -> "ebx"
  | EDI -> "edi"
  | ESI -> "esi"
  | EDX -> "edx"
  | ECX -> "ecx"
  | ESP -> "esp"
  | EBP -> "ebp"
  | R8D -> "r8d"
  | R9D -> "r9d"
  | R10D -> "r10d"
  | R11D -> "r11d"
  | R12D -> "r12d"
  | R13D -> "r13d"
  | R14D -> "r14d"
  | R15D -> "r15d"

let string_of_registerf regf =
  match regf with
  | XMM n -> Printf.sprintf "xmm%d" n
  | TOS -> Printf.sprintf "tos"
  | ST n -> Printf.sprintf "st(%d)" n

let string_of_condition condition = match condition with
    E -> "e"
  | AE -> "ae"
  | A -> "a"
  | GE -> "ge"
  | G -> "g"
  | NE -> "ne"
  | B -> "b"
  | BE -> "be"
  | L -> "l"
  | LE -> "le"
  | NLE -> "nle"
  | NG -> "ng"
  | NL -> "nl"
  | NGE -> "nge"
  | PO -> "po"
  | NP -> "np"
  | PE -> "pe"
  | P -> "p"
  | NS -> "ns"
  | S -> "s"
  | NBE -> "nbe"
  | NA -> "na"
  | NZ -> "nz"
  | Z -> "z"
  | NC -> "nc"
  | NB -> "nb"
  | NAE -> "nae"
  | C -> "c"
  | NO -> "no"
  | O -> "o"


let tab b = Buffer.add_char b '\t'
let bprint b s = tab b; Buffer.add_string b s

let arch64 = ref true

(* [print_assembler] is used to decide whether assembly code
  should be printed in the .s file or not. *)
let print_assembler = ref true

(* These hooks can be used to insert optimization passes on
  the assembly code. *)
let assembler_passes = ref ([] :
      (instruction list -> instruction list) list)

(* Which asm conventions to use *)
let masm =
  match Config.ccomp_type with
  | "msvc" | "masm" -> true
  | _      -> false

(* Shall we use an external assembler command ?
   If [binary_content] contains some data, we can directly
   save it. Otherwise, we have to ask an external command.
*)
let binary_content = ref None
let assemble_file infile outfile =
  match !binary_content with
  | None ->
     if masm then
      Ccomp.command (Config.asm ^
                     Filename.quote outfile ^ " " ^ Filename.quote infile ^
                     (if !Clflags.verbose then "" else ">NUL"))
    else
      Ccomp.command (Config.asm ^ " -o " ^
                     Filename.quote outfile ^ " " ^ Filename.quote infile)
  | Some content ->
    let oc = open_out_bin outfile in
    output_string oc content;
    close_out oc;
    binary_content := None;
    0

(*
module MakeEmitter(M : sig

      type reg
      val word_size : data_type

    end) = struct

  let arch = {
    arch64 = M.arch64;
    string_of_register = M.string_of_register;
    bprint_instr = M.bprint_instr;  }
*)

(* Override emitaux.ml *)

  let emit_int n = ConstantInt n
  let emit_nativeint n = ConstantNat n
  let emit_float64_directive f = ConstFloat f

  let (seg : segment) = new_segment ()

  let init_segments () =
    clear_segment seg;
    ()
  let emit = emit seg

(* eta-expand to create ref everytime *)
  let _jmp arg = emit (JMP (ref (Loc_unknown 0), arg))
  let _j cond arg = emit (J (ref (Loc_unknown 0), cond, arg))

  let _global s = emit (Global s)
  let _specific s = emit (Specific s)
  let _text () = emit (Segment Text)
  let _data () = emit (Segment Data)
  let _align n = emit (Align (false, n))
  let _llabel s = emit (NewLabel (s, NO)) (* local label *)
  let _comment s = emit (Comment s)
  let _extrn s ptr = emit (External (s, ptr))

  let _qword cst = emit (Constant (cst, QWORD))
  let _long cst = emit (Constant (cst, DWORD))
  let _word cst = emit (Constant (cst, WORD))
  let _byte n = emit (Constant (n, BYTE))
  let _ascii s = emit (Bytes s)
  let _space n = emit (Space n)


  (* mnemonics *)
  let _call arg = emit (CALL arg)
  let _set cond arg = emit (SET (cond, arg))

  let _je = _j E
  let _jae = _j AE
  let _jb = _j B
  let _jg = _j G
  let _jbe = _j BE
  let _ja = _j A
  let _jne = _j NE
  let _jp = _j P

  (* Qword mnemonics *)
  let _addq (arg1, arg2) = emit (ADD (Q, arg1, arg2))
  let _subq (arg1, arg2) = emit (SUB (Q, arg1, arg2))
  let _andq (arg1, arg2) = emit (AND (Q, arg1, arg2))
  let _orq (arg1, arg2) = emit (OR (Q, arg1, arg2))
  let _salq (arg1, arg2) = emit (SAL (Q, arg1, arg2))
  let _sarq (arg1, arg2) = emit (SAR (Q, arg1, arg2))
  let _shrq (arg1, arg2) = emit (SHR (Q, arg1, arg2))
  let _imulq (arg1, arg2) = emit (IMUL (Q, arg1, arg2))
  let _xorq (arg1, arg2) = emit (XOR (Q, arg1, arg2))
  let _cmpq (arg1, arg2) = emit (CMP (Q, arg1, arg2))
  let _popq arg = emit (POP (Q, arg))
  let _pushq arg = emit (PUSH (Q, arg))
  let _testq (arg1, arg2) = emit (TEST (Q, arg1, arg2))
  let _movq (arg1, arg2) = emit (MOV (Q, arg1, arg2))
  let _leaq (arg1, arg2) = emit (LEA (Q, arg1, arg2))
  let _movzbq (arg1, arg2)  = emit (MOVZX (B,Q, arg1, arg2))
  let _movsbq (arg1, arg2)  = emit (MOVSX (B,Q, arg1, arg2))
  let _movzwq (arg1, arg2)  = emit (MOVZX (W,Q, arg1, arg2))
  let _movswq (arg1, arg2)  = emit (MOVSX (W,Q, arg1, arg2))
  let _idivq arg = emit (IDIV (Q, arg))

  (* Long-word mnemonics *)
  let _addl (arg1, arg2) = emit (ADD (L, arg1, arg2))
  let _subl (arg1, arg2) = emit (SUB  (L, arg1, arg2))
  let _andl (arg1, arg2) = emit (AND  (L, arg1, arg2))
  let _orl (arg1, arg2) = emit (OR  (L, arg1, arg2))
  let _sall (arg1, arg2) = emit (SAL  (L, arg1, arg2))
  let _sarl (arg1, arg2) = emit (SAR  (L, arg1, arg2))
  let _shrl (arg1, arg2) = emit (SHR  (L, arg1, arg2))
  let _xorl (arg1, arg2) = emit (XOR (L, arg1, arg2))
  let _cmpl (arg1, arg2) = emit (CMP  (L, arg1, arg2))
  let _testl (arg1, arg2) = emit (TEST (L, arg1, arg2))
  let _movl (arg1, arg2) = emit (MOV (L, arg1, arg2))
  let _imull (arg1, arg2) = emit (IMUL (L, arg1, arg2))
  let _idivl arg = emit (IDIV (L, arg))
  let _popl arg = emit (POP (L, arg))
  let _pushl arg = emit (PUSH (L, arg))
  let _decl arg = emit (DEC (L, arg))
  let _incl arg = emit (INC (L, arg))
  let _leal (arg1, arg2) = emit (LEA (L, arg1, arg2))
  let _fistpl arg = emit (FISTP (L, arg))
  let _movzbl (arg1, arg2) = emit (MOVZX (B,L, arg1, arg2))
  let _movsbl (arg1, arg2) = emit (MOVSX (B,L, arg1, arg2))
  let _movzwl (arg1, arg2) = emit (MOVZX (W,L, arg1, arg2))
  let _movswl (arg1, arg2) = emit (MOVSX (W,L, arg1, arg2))
  let _fildl arg = emit (FILD (L, arg))
  let _fstpl arg = emit  (FSTP (Some L, arg))

(* Word mnemonics *)
  let _movw (arg1, arg2) = emit (MOV (W, arg1, arg2))

  (* Byte mnemonics *)
  let _decb arg = emit (DEC (B, arg))
  let _cmpb (arg1, arg2) = emit (CMP (B, arg1, arg2))
  let _movb (arg1, arg2) = emit (MOV (B, arg1, arg2))
  let _andb (arg1, arg2) = emit (AND (B, arg1, arg2))
  let _xorb (arg1, arg2) = emit (XOR (B, arg1, arg2))
  let _movb (arg1, arg2) = emit (MOV (B, arg1, arg2))
  let _testb (arg1, arg2) = emit (TEST (B, arg1, arg2))


  let _movsd (arg1, arg2) = emit (MOVSD (arg1, arg2))
  let _ucomisd (arg1, arg2) = emit (UCOMISD (arg1, arg2))
  let _comisd (arg1, arg2) = emit (COMISD (arg1, arg2))
  let _movapd (arg1, arg2) = emit (MOVAPD  (arg1, arg2))
  let _movabsq (arg1, arg2) = emit (MOVABSQ  (arg1, arg2))
  let _xorpd (arg1, arg2) = emit (XORPD  (arg1, arg2))
  let _andpd (arg1, arg2) = emit (ANDPD  (arg1, arg2))

  let _movslq (arg1, arg2) = emit (MOVSXD  (arg1, arg2))
  let _movss (arg1, arg2) = emit (MOVSS (arg1, arg2))
  let _cvtss2sd (arg1, arg2) = emit (CVTSS2SD (arg1, arg2))
  let _cvtsd2ss (arg1, arg2) = emit (CVTSD2SS (arg1, arg2))
  let _cvtsi2sd (arg1, arg2) = emit (CVTSI2SD (arg1, arg2))
  let _cvttsd2si (arg1, arg2) = emit (CVTTSD2SI (arg1, arg2))
  let _addsd (arg1, arg2) = emit (ADDSD (arg1, arg2))
  let _subsd  (arg1, arg2) = emit (SUBSD (arg1, arg2))
  let _mulsd (arg1, arg2) = emit (MULSD (arg1, arg2))
  let _divsd (arg1, arg2) = emit (DIVSD (arg1, arg2))
  let _sqrtsd (arg1, arg2) = emit (SQRTSD (arg1, arg2))

let _cqto () = emit CQTO

  let _incq arg = emit (INC (Q, arg))
  let _decq arg = emit (DEC (Q, arg))
  let _xchg (arg1, arg2) = emit (XCHG (arg1, arg2))
  let _bswap arg = emit (BSWAP arg)
  let _ret () = emit RET
  let _cltd () = emit CLTD
  let _hlt () = emit HLT

  let _nop () = emit NOP
  let _fchs arg_o = emit (FCHS arg_o)
  let _fabs arg_o = emit (FABS arg_o)

  let _faddl arg = emit  (FADDL arg)
  let _fsubl arg = emit  (FSUBL arg)
  let _fmull arg = emit  (FMULL arg)
  let _fdivl arg = emit  (FDIVL arg)
  let _fsubrl arg = emit  (FSUBRL arg)
  let _fdivrl arg = emit  (FDIVRL arg)

  let _faddp (arg1, arg2) = emit  (FADDP (arg1, arg2))
  let _fsubp (arg1, arg2) = emit  (FSUBP (arg1, arg2))
  let _fmulp (arg1, arg2) = emit  (FMULP (arg1, arg2))
  let _fdivp (arg1, arg2) = emit  (FDIVP (arg1, arg2))
  let _fsubrp (arg1, arg2) = emit  (FSUBRP (arg1, arg2))
  let _fdivrp (arg1, arg2) = emit  (FDIVRP (arg1, arg2))

  let _fadds arg = emit  (FADDS arg)
  let _fsubs arg = emit  (FSUBS arg)
  let _fmuls arg  = emit  (FMULS arg)
  let _fdivs arg = emit  (FDIVS arg)
  let _fsubrs arg = emit  (FSUBRS arg)
  let _fdivrs arg = emit  (FDIVRS arg)

  let _fcompp () = emit FCOMPP
  let _fcompl arg = emit (FCOMPL arg)
  let _fldl arg = emit (FLDL arg)
  let _flds arg = emit (FLDS arg)
  let _fnstsw arg = emit (FNSTSW arg)

  let _fld1 () = emit FLD1
  let _fpatan () = emit  FPATAN
  let _fptan () = emit  FPTAN
  let _fcos () = emit  FCOS
  let _fldln2 () = emit  FLDLN2
  let _fldlg2 () = emit  FLDLG2
  let _fxch arg = emit  (FXCH arg)
  let _fyl2x () = emit  FYL2X
  let _fsin () = emit  FSIN
  let _fsqrt () = emit  FSQRT
  let _fstp arg = emit  (FSTP (None, arg))
  let _fstps arg = emit  (FSTPS arg)
  let _fldz () = emit FLDZ
  let _fnstcw arg = emit (FNSTCW arg)
  let _fldcw arg = emit (FLDCW arg)


 (* arguments *)
  let _l l = LabelRel(NO, l, 0)

  let _offset l =
    if system = S_win64 then LabelOffset l
    else _l l
  let at_rip pref s =
    if system = S_win64 then LabelRel(pref, s, 0)
    else Mem (pref, RIP, 1, BaseSymbol s, 0)


  let _int n = emit_int n
  let _st n = Regf (ST n)
  let _offset l = LabelOffset l

let generate_code oc bprint_instr =
  let instrs = List.rev seg.seg_instrs in
  let instrs = List.fold_left (fun instrs pass ->
      pass instrs
    ) instrs !assembler_passes in

  if ! print_assembler then
    let b = Buffer.create 10000 in
    List.iter (bprint_instr b !arch64) instrs;
    let s = Buffer.contents b in
    output_string oc s



