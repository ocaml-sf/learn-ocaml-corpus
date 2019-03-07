type 'a parray =
  'a representation ref

and 'a representation =
  | Array of { data: 'a array }
  | Apply of { base: 'a parray; i: int; value: 'a }
