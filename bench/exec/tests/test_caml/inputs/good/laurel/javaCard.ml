(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/javaCard.ml,v 1.4 2000/02/29 08:39:42 fpottier Exp $ *)

(* This module defines the concrete JavaCard instruction set. *)

(* The type of local variable indices. *)

type index = int

(* The type of method tokens. *)

type token = int

(* The type of constant pool indices. *)

type cpx = int

(* Various instruction parameters. *)

type prim =
  | PrimBoolean
  | PrimByte
  | PrimShort
  | PrimInt

type jarray =
  | ArrayPrim of prim
  | ArrayRef of cpx

type check =
  | CheckArray of jarray
  | CheckClass of cpx

type this =
  | UseThis
  | UseStack

type absi =
  | AbsiA
  | AbsiB
  | AbsiS
  | AbsiI

type asi =
  | AsiA
  | AsiS
  | AsiI

type si =
  | SiS
  | SiI

type condition =
  | Acmpeq
  | Acmpne
  | Scmpeq
  | Scmpne
  | Scmplt
  | Scmpge
  | Scmpgt
  | Scmple
  | Eq
  | Ne
  | Lt
  | Ge
  | Gt
  | Le
  | Nonnull
  | Null

(* The type of instructions. ['a] stands for the type of nodes; it is used in branch targets. *)

type 'a opcode =
  | Aconst_null
  | Arraylength
  | Arrayload of absi
      (* includes \verb+aaload+, \verb+baload+, \verb+iaload+, \verb+saload+ *)
  | Arraystore of absi
      (* includes \verb+aastore+, \verb+bastore+, \verb+iastore+, \verb+sastore+ *)
  | Athrow
  | Checkcast of check
  | Dup
  | Dup_x of int * int
  | Dup2
  | Getfield of absi * this * cpx
      (* includes \verb+getfield_<t>+, \verb+getfield_<t>_this+ and \verb+getfield_<t>_w+ *)
  | Getstatic of absi * cpx
  | Goto of 'a
      (* includes \verb+goto+ and \verb+goto_w+ *)
  | I2b
  | I2s
  | Iadd
  | Iand
  | Icmp
  | Idiv
  | If of condition * 'a
      (* includes \verb+if_acmp<cond>+, \verb+if_scmp<cond>+, \verb+if<cond>+, \verb+ifnonull+, \verb+ifnull+
	 and their \verb+_w+ variants *)
  | Iinc of index * int
      (* includes \verb+iinc+ and \verb+iinc_w+ *)
  | Imul
  | Ineg
  | Instanceof of check
  | Invokeinterface of int * cpx * token
  | Invokespecial of cpx
  | Invokestatic of cpx
  | Invokevirtual of cpx
  | Ior
  | Ipush of int
      (* includes \verb+bipush+, \verb+sipush+, \verb+iipush+, \verb+iconst_<i>+ *)
  | Irem
  | Ishl
  | Ishr
  | Isub
  | Iushr
  | Ixor
  | Jsr of 'a
  | Load of asi * index
      (* includes \verb+aload+, \verb+iload+, \verb+sload+, and their \verb+_<n>+ variants *)
  | Lookupswitch of si * 'a * (int * 'a) list
      (* includes \verb+ilookupswitch+ and \verb+slookupswitch+ *)
  | New of cpx
  | Newarray of jarray
      (* includes \verb+anewarray+ and \verb+newarray+ *)
  | Nop
  | Pop
  | Pop2
  | Putfield of absi * this * cpx
      (* includes \verb+putfield_<t>+, \verb+putfield_<t>_this+ and \verb+putfield_<t>_w+ *)
  | Putstatic of absi * cpx
  | Ret of index
  | Return of asi option
      (* includes \verb+areturn+, \verb+ireturn+, \verb+return+, \verb+sreturn+ *)
  | S2b
  | S2i
  | Sadd
  | Sand
  | Sdiv
  | Sinc of index * int
      (* includes \verb+sinc+ and \verb+sinc_w+ *)
  | Smul
  | Sneg
  | Sor
  | Spush of int
      (* includes \verb+bspush+, \verb+sspush+, \verb+sconst_<s>+ *)
  | Srem
  | Sshl
  | Sshr
  | Ssub
  | Store of asi * index
      (* includes \verb+astore+, \verb+istore+, \verb+sstore+, and their \verb+_<n>+ variants *)
  | Sushr
  | Swap_x of int * int
  | Sxor
  | Tableswitch of si * 'a * int * int * 'a array
      (* includes \verb+itableswitch+ and \verb+stableswitch+ *)

(* This signature represents a concrete JavaCard method. *)

module type Method = sig

  (* The method forms a graph where nodes are program points. The start node is the method's entry point. A node's
     successor is the instruction following it in the bytecode array, if there is one (i.e. not necessarily its
     logical successor in the control flow graph). *)

  include Graph.BasicIterStart

  (* This function maps a node to an instruction. *)

  val opcode: node -> node opcode

  (* This function enumerates all handlers which are in scope at a given node. Each handler is accompanied by an
     optional class specification, telling which exceptions it may catch. Inner-most handlers must be presented
     first. *)

  val handlers: (cpx option -> node -> unit) -> node -> unit

  (* A list of the exceptions which the method has been declared to return. The list must be exhaustive, i.e.
     it must be include a superclass of every exception possibly thrown by the method, be it checked or unchecked. *)

  type jclass

  val exceptions: jclass list

  (* The number of local variables used by the method. Locals are numbered from 0 to [locals]$ - 1$. *)

  val locals: int

end

