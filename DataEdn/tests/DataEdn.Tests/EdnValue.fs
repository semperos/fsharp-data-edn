module DataEdn.Tests.EdnValue

open NUnit.Framework
open FSharp.Data
open FsUnit

open DataEdn

[<Test>]
let ``String encoding is a no-op`` () =
    let input = "foo bar baz" in
    let actual = EdnValue.EdnStringEncode input in
    let expected = "foo bar baz" in
    actual |> should equal expected

[<Test>]
let ``Boolean serialization`` () =
    let input = [true; false] in
    let actual = List.map (fun b -> (EdnValue.Boolean b).ToString()) input in
    let expected = ["true"; "false"] in
    actual |> should equal expected

[<Test>]
let ``EDN Parser integers`` () =
    let inputs = ["42"; "-42"; "+42"] in
    let actual = List.map (fun x -> EdnValue.Parse(x)) inputs in
    let expected = [EdnValue.Integer 42; EdnValue.Integer -42; EdnValue.Integer 42] in
    actual |> should equal expected

[<Test>]
let ``EDN Parser floats`` () =
    let inputs = ["42.0"; "-42.0"; "+42.0"; "16.3"] in
    let actual = List.map (fun x -> EdnValue.Parse(x)) inputs in
    let expected = [EdnValue.Float 42.0; EdnValue.Float -42.0; EdnValue.Float 42.0; EdnValue.Float 16.3] in
    actual |> should equal expected

[<Test>]
let ``EDN Parser simple list`` () =
    let input = "(1 2 3)" in
    let actual = EdnValue.Parse input in
    let expected = EdnValue.EdnList [| EdnValue.Integer 1; EdnValue.Integer 2; EdnValue.Integer 3 |]
    actual |> should equal expected

[<Test>]
let ``EDN Parser nested list`` () =
    let input = "(1 2 (3 4))" in
    let actual = EdnValue.Parse input in
    let expected = EdnValue.EdnList [| EdnValue.Integer 1;
                                       EdnValue.Integer 2;
                                       (EdnValue.EdnList [| EdnValue.Integer 3; EdnValue.Integer 4 |]) |]
    actual |> should equal expected

[<Test>]
let ``EDN Parser simple vector`` () =
    let input = "[1 2 3]" in
    let actual = EdnValue.Parse input in
    let expected = EdnValue.EdnVector [| EdnValue.Integer 1; EdnValue.Integer 2; EdnValue.Integer 3 |]
    actual |> should equal expected

[<Test>]
let ``EDN Parser nested vector`` () =
    let input = "[1 2 [3 4]]" in
    let actual = EdnValue.Parse input in
    let expected = EdnValue.EdnVector [| EdnValue.Integer 1;
                                       EdnValue.Integer 2;
                                       (EdnValue.EdnVector [| EdnValue.Integer 3; EdnValue.Integer 4 |]) |]
    actual |> should equal expected
                                       
[<Test>]
let ``EDN Parser simple set`` () =
    let input = "#{1 2 3}" in
    let actual = EdnValue.Parse input in
    let expected = EdnValue.EdnSet [| EdnValue.Integer 1; EdnValue.Integer 2; EdnValue.Integer 3 |]
    actual |> should equal expected

[<Test>]
let ``EDN Parser nested set`` () =
    let input = "#{1 2 #{3 4}}" in
    let actual = EdnValue.Parse input in
    let expected = EdnValue.EdnSet [| EdnValue.Integer 1;
                                       EdnValue.Integer 2;
                                       (EdnValue.EdnSet [| EdnValue.Integer 3; EdnValue.Integer 4 |]) |]
    actual |> should equal expected

[<Test>]
let ``EDN Parser arbitrary nested collections`` () =
    let input = "[1 2 #{3 4} (5 [6 7]) 8]" in
    let actual = EdnValue.Parse input in
    let expected = EdnValue.EdnVector [| EdnValue.Integer 1;
                                         EdnValue.Integer 2;
                                         EdnValue.EdnSet [| EdnValue.Integer 3;
                                                            EdnValue.Integer 4; |];
                                         EdnValue.EdnList [| EdnValue.Integer 5;
                                                             EdnValue.EdnVector [| EdnValue.Integer 6;
                                                                                   EdnValue.Integer 7 |]
                                                          |];
                                         EdnValue.Integer 8 |]
    actual |> should equal expected