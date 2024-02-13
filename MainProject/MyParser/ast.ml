type position =
{
  start : Lexing.position;
  length : int
}

type const = 
| IntConst of int                   (*Integers           *)
| FloatConst of float               (*Float              *)
| StringConst of string             (*Strings            *)

type exp =
| True
| False
| Atom of string                    (*zero-arn exp       *)
| VarExp of string                  (*Variable           *)
| ConstExp of const                 (*Constants          *)
| TermExp of string * exp list      (*func(arg1,arg2,...)*)

type dec =
| Fact of exp                       (*Head.              *)
| Rule of exp * exp list            (*Head :- Body.      *)
| Query of (exp list)               (*Queries            *)

type program = dec list