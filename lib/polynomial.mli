type coeff
type term
type t

val of_string : string -> t
val pp : Format.formatter -> t -> unit
val ( + ) : t -> t -> t
val neg : t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val sort_by_ord : order:Monomial.Order.mon_compare -> t -> t
val leading_term : order:Monomial.Order.mon_compare -> t -> term
