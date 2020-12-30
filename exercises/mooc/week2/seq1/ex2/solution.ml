let exchange k =
  assert (k >= 10 && k <= 99);
  (k mod 10) * 10 + (k / 10)

let is_valid_answer (grand_father_age, grand_son_age) =
  (grand_son_age * 4 = grand_father_age)
  && (exchange grand_father_age * 3 = exchange grand_son_age)

let find (lgrandpa, lgrandson) =
  let rec find ((grandpa, grandson) as answer) =
    if is_valid_answer answer then answer
    else if grandson >= grandpa then
      if grandpa <= lgrandson then
	(-1, -1)
      else
	find (grandpa - 1, lgrandson)
    else
      find (grandpa, grandson + 1) in
  find (lgrandpa, lgrandson)
