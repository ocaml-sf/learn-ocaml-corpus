open Report
open Test_lib

let sample_exp () =
  let rec sample level =
    match Random.int level with
    | 0 -> EInt (Random.int 20 - 10)
    | _ ->
        if Random.bool () then
          EAdd (sample (level - 1), sample (level - 1))
        else
          EMul (sample (level - 1), sample (level - 1)) in
  sample 5

let sample_factorizable () =
  let a = sample_exp () in
  let b = sample_exp () in
  let c = sample_exp () in
  EAdd (EMul (a, b), EMul (a, c))

let sample_expandable () =
  let a = sample_exp () in
  let b = sample_exp () in
  let c = sample_exp () in
  EMul (a, EAdd (b, c))

let sample_simplifiable =
  sample_alternatively
    [ (fun () -> EMul (EInt 0, sample_exp ())) ;
      (fun () ->  EMul (sample_exp (), EInt 0)) ;
      (fun () ->  EAdd (EInt 0, sample_exp ()));
      (fun () ->  EAdd (sample_exp (), EInt 0));
      (fun () ->  EMul (EInt 1, sample_exp ()));
      (fun () ->  EMul (sample_exp (), EInt 1)) ]


let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ Section ([ Text "Exercise 1: " ; Code "my_example" ],
             test_variable_property
               [%ty: exp] "my_example"
               (function
                 | EAdd (EMul (EInt 3, EInt 3), EMul (EInt 2, EInt 2)) ->
                     [ Message ([ Text "Almost correct expression found"], Success 2) ;
                       Message ([ Text "Operands in wrong order"], Failure) ]
                 | EAdd (EMul (EInt 2, EInt 2), EMul (EInt 3, EInt 3)) ->
                     [ Message ([ Text "Correct expression found"], Success 4) ]
                 | e ->
                     if Solution.eval e = 13 then
                       [ Message ([ Text "Expression with the same result found"], Success 1) ;
                         Message ([ Text "You need to write the exact expression asked, \
                                          not just one with the same result."], Failure) ]
                     else
                       [ Message ([ Text "Wrong expression"], Failure) ]
               )) ;
    Section ([ Text "Exercise 2: " ; Code "eval" ],
             test_function_1_against_solution
               [%ty: exp -> int] "eval"
               []) ;
    Section ([ Text "Exercise 3: " ; Code "factorize" ],
             test_function_1_against_solution
               ~sampler:(sample_alternatively [ sample_factorizable ; sample_factorizable ; sample_exp ])
               [%ty: exp -> exp] "factorize"
               []) ;
    Section ([ Text "Exercise 4: " ; Code "expand" ],
             test_function_1_against_solution
               ~sampler:(sample_alternatively [ sample_expandable ; sample_expandable ; sample_exp ])
               [%ty: exp -> exp] "expand"
               []) ;
    Section ([ Text "Exercise 5: " ; Code "simplify" ],
             test_function_1_against_solution
               ~sampler:(sample_alternatively [ sample_simplifiable ; sample_simplifiable ; sample_exp ])
               [%ty: exp -> exp] "simplify"
               []) ]
