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
    Loc_unknown
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

type instr =
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
  | Set
  | End

  | NOP

  | ADD of suffix
  | SUB of suffix
  | XOR of suffix
  | OR of suffix
  | AND of suffix
  | CMP of suffix

  | FSTP of suffix option
  | FSTPS

  | FCOMPP
  | FCOMPL
  | FLDL
  | FLDS
  | FNSTSW
  | FNSTCW
  | FLDCW

  | FCHS
  | FABS
  | FADDL
  | FSUBL
  | FMULL
  | FDIVL
  | FSUBRL
  | FDIVRL
  | FILD of suffix
  | FISTP of suffix
  | HLT

  | FADDP
  | FSUBP
  | FMULP
  | FDIVP
  | FSUBRP
  | FDIVRP

  | FADDS
  | FSUBS
  | FMULS
  | FDIVS
  | FSUBRS
  | FDIVRS

  | FLD1
  | FPATAN
  | FPTAN
  | FCOS
  | FLDLN2
  | FLDLG2
  | FXCH
  | FYL2X
  | FSIN
  | FSQRT
  | FLDZ

  | SAR of suffix
  | SHR of suffix
  | SAL of suffix
  | INC of suffix
  | DEC of suffix
  | IMUL of suffix
  | IDIV of suffix
  | PUSH of suffix
  | POP of suffix

  | MOV of suffix

  | MOVZX of suffix * suffix
  | MOVSX of suffix * suffix
  | MOVSS
  | MOVSXD (* MOVSLQ *)

  | MOVSD
  | ADDSD
  | SUBSD
  | MULSD
  | DIVSD
  | SQRTSD
  | ROUNDSD of rounding
  | NEG

  | CVTSS2SD
  | CVTSD2SS
  | CVTSI2SD
  | CVTSI2SDQ
  | CVTSD2SI
  | CVTTSD2SI
  | UCOMISD
  | COMISD

  | CALL
  | JMP of locality
  | RET

  | TEST of suffix
  | SET of condition
  | J of locality * condition

  | CMOV of condition
  | XORPD
  | ANDPD
  | MOVAPD
  | MOVLPD
  | MOVABSQ

  | CLTD
  | LEA of suffix
  | CQTO
  | LEAVE

  | XCHG
  | BSWAP

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

type 'reg arg =
  | Constant of int
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
  | Reg of 'reg
  | Regf of registerf
  | Mem of
      data_type *
        'reg * (* scale *) int *
        'reg base * (* offset *) int

type 'reg instruction = {
  mutable instr : instr;
  mutable args : 'reg arg array;
}

type 'reg segment = {
  mutable seg_instrs : 'reg instruction list;
}

type 'reg arch = {
  arch64 : bool;
  string_of_register : ('reg -> string);
  bprint_instr : (Buffer.t -> 'reg arch -> 'reg instruction -> unit);
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

let emit seg ins args =
  seg.seg_instrs <- {
    instr = ins;
    args = args
  } :: seg.seg_instrs

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


module MakeEmitter(M : sig

      type reg
      val arch64 : bool
      val string_of_register : reg -> string
      val bprint_instr : Buffer.t -> reg arch -> reg instruction -> unit
      val word_size : data_type

    end) = struct

  let arch = {
    arch64 = M.arch64;
    string_of_register = M.string_of_register;
    bprint_instr = M.bprint_instr;  }

(* Override emitaux.ml *)

  let emit_int n = Constant n
  let emit_nativeint n = ConstantNat n
  let emit_float64_directive f = ConstFloat f

  let (seg : M.reg segment) = new_segment ()

  let init_segments () =
    clear_segment seg;
    ()
  let emit = emit seg



  let _global s = emit (Global s) [||]
  let _specific s = emit (Specific s) [||]
  let _text () = emit (Segment Text) [||]
  let _data () = emit (Segment Data) [||]
  let _align n = emit (Align (false, n)) [||]
  let _llabel s = emit (NewLabel (s, NO)) [||] (* local label *)
  let _label s = emit (NewLabel (s, M.word_size)) [||]
  let _comment s = emit (Comment s) [||]
  let _extrn s ptr = emit (External (s, ptr)) [||]

  let _qword cst = emit (Constant (cst, QWORD)) [||]
  let _long cst = emit (Constant (cst, DWORD)) [||]
  let _word cst = emit (Constant (cst, WORD)) [||]
  let _byte n = emit (Constant (n, BYTE)) [||]
  let _ascii s = emit (Bytes s) [||]
  let _space n = emit (Space n) [||]


  (* mnemonics *)
  let _call = emit CALL
  let _jmp = emit (JMP Loc_unknown)
  let _j cond = emit (J (Loc_unknown, cond))
  let _set cond = emit (SET cond)

  let _je = _j E
  let _jae = _j AE
  let _jb = _j B
  let _jg = _j G
  let _jbe = _j BE
  let _ja = _j A
  let _jne = _j NE
  let _jp = _j P

  (* Qword mnemonics *)
  let _addq = emit (ADD Q)
  let _subq = emit (SUB Q)
  let _andq = emit (AND Q)
  let _orq = emit (OR Q)
  let _salq = emit (SAL Q)
  let _sarq = emit (SAR Q)
  let _shrq = emit (SHR Q)
  let _imulq = emit (IMUL Q)
  let _xorq = emit (XOR Q)
  let _cmpq = emit (CMP Q)
  let _popq = emit (POP Q)
  let _pushq = emit (PUSH Q)
  let _testq = emit (TEST Q)
  let _movq = emit (MOV Q)
  let _leaq = emit (LEA Q)
  let _movzbq = emit (MOVZX (B,Q))
  let _movsbq = emit (MOVSX (B,Q))
  let _movzwq = emit (MOVZX (W,Q))
  let _movswq = emit (MOVSX (W,Q))
  let _idivq = emit (IDIV Q)

  (* Long-word mnemonics *)
  let _addl = emit (ADD L)
  let _subl = emit (SUB L)
  let _andl = emit (AND L)
  let _orl = emit (OR L)
  let _sall = emit (SAL L)
  let _sarl = emit (SAR L)
  let _shrl = emit (SHR L)
  let _imull = emit (IMUL L)
  let _idivl = emit (IDIV L)
  let _xorl = emit (XOR L)
  let _cmpl = emit (CMP L)
  let _popl = emit (POP L)
  let _pushl = emit (PUSH L)
  let _testl = emit (TEST L)
  let _decl = emit (DEC L)
  let _movw = emit (MOV W)
  let _movl = emit (MOV L)
  let _incl = emit (INC L)
  let _leal = emit (LEA L)
  let _fistpl = emit (FISTP L)
  let _movzbl = emit (MOVZX (B,L))
  let _movsbl = emit (MOVSX (B,L))
  let _movzwl = emit (MOVZX (W,L))
  let _movswl = emit (MOVSX (W,L))
  let _fildl = emit (FILD L)
  let _fstpl = emit  (FSTP (Some L))

(* Word mnemonics *)
  let _movw = emit (MOV W)

  (* Byte mnemonics *)
  let _decb = emit (DEC B)
  let _cmpb = emit (CMP B)
  let _movb = emit (MOV B)
  let _andb = emit (AND B)
  let _xorb = emit (XOR B)
  let _movb = emit (MOV B)
  let _testb = emit (TEST B)


  let _movsd = emit MOVSD
  let _ucomisd = emit UCOMISD
  let _comisd = emit COMISD
  let _movapd = emit MOVAPD
  let _xorpd = emit XORPD
  let _movabsq = emit MOVABSQ

  let _movslq = emit MOVSXD
  let _cvtss2sd = emit CVTSS2SD
  let _movss = emit MOVSS
  let _cvtsd2ss = emit CVTSD2SS
  let _cqto = emit CQTO
  let _addsd = emit ADDSD
  let _subsd = emit SUBSD
  let _mulsd = emit MULSD
  let _divsd = emit DIVSD
  let _incq = emit (INC Q)
  let _decq = emit (DEC Q)
  let _andpd = emit ANDPD
  let _cvtsi2sd = emit CVTSI2SD
  let _cvttsd2si = emit CVTTSD2SI
  let _xchg = emit XCHG
  let _bswap = emit BSWAP
  let _sqrtsd = emit SQRTSD
  let _ret = emit RET
  let _cltd = emit CLTD
  let _hlt = emit HLT

  let _nop = emit NOP
  let _fchs = emit FCHS
  let _fabs = emit FABS

  let _faddl = emit  FADDL
  let _fsubl = emit  FSUBL
  let _fmull = emit  FMULL
  let _fdivl = emit  FDIVL
  let _fsubrl = emit  FSUBRL
  let _fdivrl = emit  FDIVRL

  let _faddp = emit  FADDP
  let _fsubp = emit  FSUBP
  let _fmulp = emit  FMULP
  let _fdivp = emit  FDIVP
  let _fsubrp = emit  FSUBRP
  let _fdivrp = emit  FDIVRP

  let _fadds = emit  FADDS
  let _fsubs = emit  FSUBS
  let _fmuls = emit  FMULS
  let _fdivs = emit  FDIVS
  let _fsubrs = emit  FSUBRS
  let _fdivrs = emit  FDIVRS

  let _fcompp = emit FCOMPP
  let _fcompl = emit FCOMPL
  let _fldl = emit FLDL
  let _flds = emit FLDS
  let _fnstsw = emit FNSTSW

  let _fld1 = emit FLD1
  let _fpatan = emit  FPATAN
  let _fptan = emit  FPTAN
  let _fcos = emit  FCOS
  let _fldln2 = emit  FLDLN2
  let _fldlg2 = emit  FLDLG2
  let _fxch = emit  FXCH
  let _fyl2x = emit  FYL2X
  let _fsin = emit  FSIN
  let _fsqrt = emit  FSQRT
  let _fstp = emit  (FSTP None)
  let _fstps = emit  FSTPS
  let _fldz = emit FLDZ
  let _fnstcw = emit FNSTCW
  let _fldcw = emit FLDCW


 (* arguments *)
  let _l l = LabelRel(NO, l, 0)

  let _offset l =
    if system = S_win64 then LabelOffset l
    else _l l
  let at_rip pref s =
    if system = S_win64 then LabelRel(pref, s, 0)
    else Mem (pref, RIP, 1, BaseSymbol s, 0)

  let _mem offset reg = Mem(NO, reg, 1, NoBase, offset)

(* On win32/win64, some memory references need to specify the size of
   the operand in 'pref' *)
  let _mem_ptr pref offset reg = Mem(pref, reg, 1, NoBase, offset)

  let _int n = emit_int n
  let _st n = Regf (ST n)
  let _offset l = LabelOffset l

end
