type 'a p1 = 'a ref -> 'a

type 'a p2 = ('a -> bool) -> 'a -> ('a -> 'a -> bool) -> 'a -> bool

type 'a p3 = (bool ref -> bool) -> bool