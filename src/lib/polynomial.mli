type coeff
type term
type t

val of_string : string -> t
val to_string : t -> string
val pp : Format.formatter -> t -> unit
val show : t -> string
val ( + ) : t -> t -> t
val neg : t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val equal : t -> t -> bool
val sort_by_ord : order:Monomial.Order.mon_compare -> t -> t
val leading_term : order:Monomial.Order.mon_compare -> t -> term
val div : term -> term -> term option
val poly_of_term : term -> t
val leading_mon : order:Monomial.Order.mon_compare -> t -> Monomial.t
val leading_coeff : order:Monomial.Order.mon_compare -> t -> coeff
val zero : t
val one : t
