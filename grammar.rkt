#lang brag

program: (term | assignment)*
unary-operator: "sum"
binary-operator: "+" | "*" | "-" | "/" | "mod"
@term: variable | binary-term | unary-term | /"(" term /")" | roll | integer
@roll: unscaled-roll | scaled-roll
scaled-roll: integer /"D" integer
  | /"(" unscaled-roll /")" /"D" integer
unscaled-roll: /"D" integer

; binary terms:
@binary-term: binary-sum
  | binary-product
  | binary-difference
  | binary-quotient
  | binary-mod

binary-sum: term /"+" term
binary-difference: term /"-" term
binary-product: term /"*" term
binary-quotient: term /"/" term
binary-mod: term /"mod" term
unary-minimum: /"min" term
unary-maximum: /"max" term

; unary terms
@unary-term: unary-sum
  | unary-minimum
  | unary-maximum

unary-sum: /"sum" term

integer: INTEGER
assignment: VARIABLE /":=" term
variable: VARIABLE
