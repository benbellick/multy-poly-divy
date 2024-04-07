type var
type exp
type t

val of_string : string -> t
val pp : Format.formatter -> t -> unit
val show : t -> string
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t option
val ord : t -> int

module Order : sig
  type mon_compare = t -> t -> int

  val lex : mon_compare
  val grlex : mon_compare
  val grevlex : mon_compare
end
