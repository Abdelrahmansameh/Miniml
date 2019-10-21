(* This file contains all the important type definitions for the project;
 * the syntax and semantics of MiniML is a subset of that of OCaml, thus
 * you may compare your results with the results produced by Ocaml itself! *)

(* Names (defined in let ... in ... or as function arguments) *)
type name = string

(* Constants *)
type const =
  | Cint    of int
  | Cbool   of bool

(* Operators: unary and binary *)
type uop =
  | Uineg (* integers: negation *)
  | Ubnot (* booleans: negation *)
  | Upfst (* pairs:    first    *)
  | Upsnd (* pairs:    second   *)
type bop =
  | Biadd (* integers: add      *)
  | Bisub (* integers: sub      *)
  | Bimul (* integers: mul      *)
  | Bidiv (* integers: div      *)
  | Bband (* booleans: and      *)
  | Bcleq (* compare:  integers, less or equal *)
  | Bceq  (* compare:  integers, equal *)

(* Types
 * Note: we do not consider higher order functions
 *       but we still need to give types to functions, hence we
 *       have a arrow type *)
type typ =
  | Tint                      (* base type int *)
  | Tbool                     (* base type bool *)
  | Tprod of typ * typ        (* product of two types *)
  | Tarrow of typ list * typ  (* function type *)

(* Argument of a function
 * Note: to make typing easier we assume that all argument types
 *       are user supplied with annotations *)
type arg = name * typ

(* Expressions (or programs) *)
type expr =
  | Econst  of const                     (* constant *)
  | Ename   of name                      (* value of a name *)
  | Eunary  of uop * expr                (* unary expression *)
  | Ebinary of bop * expr * expr         (* binary expression *)
  | Eif     of expr * expr * expr        (* if...then...else... expression *)
  | Epair   of expr * expr               (* construction of a pair *)
  | Elet    of bool * name * expr * expr (* let...=...in... expression;
                                          * bool arg: true iff recursive let *)
  | Efun    of arg list * expr           (* definition of a function *)
  | Eapply  of expr * expr list          (* application of a function *)
