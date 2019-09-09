let create size =
  Array.make (size + 1) 0

let push buf elt =
  if buf.(0) = Array.length buf - 1 then
    raise Full
  else begin
    buf.(buf.(0) + 1) <- elt ;
    buf.(0) <- buf.(0) + 1
  end

let pop buf =
  if buf.(0) <= 0 then
    raise Empty
  else begin
    buf.(0) <- buf.(0) - 1 ;
    buf.(buf.(0) + 1)
  end

let append buf arr =
  for i = Array.length arr - 1 downto 0 do
    push buf arr.(i)
  done
