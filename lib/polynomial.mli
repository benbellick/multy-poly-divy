type coeff
type t

val of_string : string -> t
val pp : Format.formatter -> t -> unit
val ( + ) : t -> t -> t
val neg : t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
