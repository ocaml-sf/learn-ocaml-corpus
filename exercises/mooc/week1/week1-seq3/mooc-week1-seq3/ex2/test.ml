open Report
open Test_lib

let () = nbconcs := !nbconcs - 5 (* solution *) ;;

let () =
  set_result @@
  [ Message ([ Text "This time, " ; Code "word" ; Text " is " ;
               Code (Printf.sprintf "%S" word) ; Text "."], Important) ] @
  ast_sanity_check code_ast @@ fun () ->
  let sanity_report =
    ast_check_structure
      ~on_pattern:(function
          | [%pat? ( ^ )] -> [ Message ([ Text "Don't redefine " ; Code "(^)" ; Text ". Please, don't." ], Failure)]
          | _ -> [])
      ~on_open:(forbid_syntax "open")
      ~on_include:(forbid_syntax "include")
      ~on_function_call:(fun (expr, _) -> restrict_expr "function" [ [%expr ( ^ )] ] expr)
      code_ast |> List.sort compare in
  if snd (Report.result sanity_report) then
    sanity_report
  else
    test_variable_against_solution [%ty: string] "sentence" @
    [ Message ([ Text "Testing how many times you concatenated."], Informative) ] @
    test_ref [%ty: int] nbconcs 5
