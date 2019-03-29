#lang brag

program: (term | assignment)*
unary-operator: "sum"
binary-operator: "+" | "*" | "mod" | "/"
term: roll | integer | unary-operator term | term binary-operator term | variable
roll: [ scale ] ROLL
scale: INTEGER
  | INTEGER binary-operator INTEGER
  | "(" roll ")"
integer: INTEGER
assignment: VARIABLE ":=" term
variable: VARIABLE