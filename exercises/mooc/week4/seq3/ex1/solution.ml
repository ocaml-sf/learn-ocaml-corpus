let ccr =
  fun a -> let r = 8. *. (cos (a /. 2.)) in
    fun b -> let r = r *. (cos (b /. 2.)) in
      fun c -> let r = r *. (cos (c /. 2.)) in
        fun s -> s /. r
