open Report
open Test_lib

module type EXPECTED_SIGNATURE = sig
  type e
  val int : int -> e
  val mul : e -> e -> e
  val add : e -> e -> e
  val to_string : e -> string
end

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  set_progress "Grading exercise." ;
  test_variable_property [%ty: (module EXPECTED_SIGNATURE) ] "Exp" @@ fun (module Exp : EXPECTED_SIGNATURE) ->
  Message ([Text "Your module" ; Code "Exp" ; Text "is compatible." ], Success 5) ::
  let ok, message = abstract_type ~allow_private:true "Exp.e" in
  message @ if not ok then [] else
    [ Message ([Text "Now I will check that you didn't change the behaviour."], Important) ;
      let rec test nb =
        let buf = Buffer.create 100 in
        let ppf = Format.formatter_of_buffer buf in
        let print fmt = Format.fprintf ppf fmt in
        let rec sample level =
          match Random.int level with
          | 0 ->
              let n = Random.int 5 in
              print "(Exp.int %d)" n ;
              Exp.int n, Solution.Exp.int n
          | _ ->
              let add = Random.bool () in
              if add then print "(@[<hov 2>Exp.add@ "
              else print "(@[<hov 2>Exp.mul@ " ;
              let ul, sl = sample (level - 1) in
              print "@ " ;
              let ur, sr = sample (level - 1) in
              print "@])" ;
              if add then
                Exp.add ul ur, Solution.Exp.add sl sr
              else
                Exp.mul ul ur, Solution.Exp.mul sl sr in
        if nb = 0 then [] else
          let uexp, sexp = sample 7 in
          let str = print "@." ; Buffer.contents buf in
          Message ([ Text "Computing and printing " ; Code str ], Informative) ::
          test_eq (=) [%ty: string]
            (result @@ fun () -> Exp.to_string uexp)
            (result @@ fun () -> Solution.Exp.to_string sexp) @
          test (nb - 1) in
      Section ([ Text "Testing " ; Code "Exp.to_string" ], test 10) ]
