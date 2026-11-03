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


(* Two parser values that share buffered token: [p1] buffers "ab", then [p2]
   resumes the earlier value [p0] with 'c'. When [p1] backtracks, it reads its
   buffered token again, and they are still "ab". *)

module SP = Character.Make (Unit) (String) (Unit)

let%test _ =
    let open SP in
    let p = backtrack (string "abx") "abx" </> string "aby" in
    let p0 = make () p |> Parser.put 'a' in
    let p1 = Parser.put 'b' p0 in
    let p2 = Parser.put 'c' p0 in
    let p1 = p1 |> Parser.put 'y' |> Parser.put_end in
    Parser.has_succeeded p1
    && Parser.final p1 = "aby"
    && not (Parser.has_succeeded (Parser.put_end p2))
