let wellformed { year; month; day; hour; minute } =
  let bound a b = fun x -> a <= x && x <= b in
  year >= 1 && bound 1 5 month && bound 1 4 day && bound 0 2 hour && bound 0 1 minute

let next ({ year; month; day; hour; minute } as date) =
  assert (wellformed date);
  if minute < 1 then
    { date with minute = minute + 1 }
  else if hour < 2 then
    { date with hour = hour + 1; minute = 0 }
  else if day < 4 then
    { date with day = day + 1; hour = 0; minute = 0 }
  else if month < 5 then
    { date with month = month + 1; day = 1; hour = 0; minute = 0 }
  else
    { year = year + 1; month = 1; day = 1; hour = 0; minute = 0 }

let of_int k =
  let rec aux date k =
    if k = 0 then date else aux (next date) (pred k) in
  aux the_origin_of_time k
