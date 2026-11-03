(* A backtrackable parser keeps the token it consumes. Keeping them must cost
   memory in proportion to their number. *)

module CP = Character.Make (Unit) (Int) (Unit)

let bytes_to_parse (n: int): float =
    let open CP in
    let p = backtrack (map List.length (zero_or_more letter)) "letters" in
    let before = Gc.allocated_bytes () in
    let parser = Parser.run_on_string (String.make n 'a') (make () p) in
    assert (Parser.has_succeeded parser);
    Gc.allocated_bytes () -. before


let%test _ =
    bytes_to_parse 8000 < 3.0 *. bytes_to_parse 4000


(* A parser value kept from earlier can still be resumed with a different
   token, and the value resumed first keeps its token. *)

module SP = Character.Make (Unit) (String) (Unit)

let%test _ =
    let open SP in
    let p =
        backtrack
            (map (fun cs -> String.of_seq (List.to_seq cs)) (zero_or_more letter))
            "letters"
    in
    let p0 = make () p |> Parser.put 'a' |> Parser.put 'a' in
    let p1 = p0 |> Parser.put 'b' |> Parser.put_end in
    let p2 = p0 |> Parser.put 'c' |> Parser.put_end in
    Parser.final p1 = "aab" && Parser.final p2 = "aac"
