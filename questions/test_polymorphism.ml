
type ('a, 'b) t =
    A of 'a * 'a
  | B of 'b * 'a

let rec f t = match t with
    A (x, y) as t0 -> print_int (x + y); f t0
  | B (x, y) as t0 -> print_float x; print_int y; f t0

