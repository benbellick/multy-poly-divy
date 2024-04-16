type var
type exp
type t

val of_string : string -> t
val pp : Format.formatter -> t -> unit
val show : t -> string
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t option
val equal : t -> t -> bool
val ord : t -> int

val const : t
(** This is the constant monomial, i.e. no variables  *)

module Order : sig
  type mon_compare = t -> t -> int

  val lex : mon_compare
  val grlex : mon_compare
  val grevlex : mon_compare
end
