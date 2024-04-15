type t

val recip : t -> t
val neg : t -> t
val mk : int -> int -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val zero : t
val one : t
val eq : t -> t -> bool
val of_string : string -> t
