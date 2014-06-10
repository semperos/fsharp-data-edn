module DataEdn.Tests.EdnValue

open System
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

[<TestFixture>]
type ``Given the EDN Parser`` () =

    [<Test>] member x.
        ``when parsing integers`` () =
            let inputs = ["42"; "-42"; "+42"] in
            let actual = List.map (fun x -> EdnValue.Parse(x)) inputs in
            let expected = List.map EdnValue.Integer [42; -42; 42] in
            actual |> should equal expected

    [<Test>] member x.
        ``when parsing floats`` () =
            let inputs = ["42.0"; "-42.0"; "+42.0"; "16.3"] in
            let actual = List.map (fun x -> EdnValue.Parse(x)) inputs in
            let expected = List.map EdnValue.Float [42.0; -42.0; 42.0; 16.3] in
            actual |> should equal expected

    [<Test>] member x.
        ``when parsing simple lists`` () =
            let input = "(1 2 3)" in
            let actual = EdnValue.Parse input in
            let expected = EdnValue.EdnList (Array.map EdnValue.Integer [| 1; 2; 3 |]) in
            actual |> should equal expected

    [<Test>] member x.
        ``when parsing nested lists`` () =
            let input = "(1 2 (3 4))" in
            let actual = EdnValue.Parse input in
            let expected = EdnValue.EdnList [| EdnValue.Integer 1;
                                               EdnValue.Integer 2;
                                               (EdnValue.EdnList [| EdnValue.Integer 3; EdnValue.Integer 4 |]) |] in
            actual |> should equal expected

    [<Test>] member x.
        ``when parsing simple vectors`` () =
            let input = "[1 2 3]" in
            let actual = EdnValue.Parse input in
            let expected = EdnValue.EdnVector [| EdnValue.Integer 1; EdnValue.Integer 2; EdnValue.Integer 3 |] in
            actual |> should equal expected

    [<Test>] member x.
        ``when parsing nested vectors`` () =
            let input = "[1 2 [3 4]]" in
            let actual = EdnValue.Parse input in
            let expected = EdnValue.EdnVector [| EdnValue.Integer 1;
                                                 EdnValue.Integer 2;
                                               (EdnValue.EdnVector [| EdnValue.Integer 3; EdnValue.Integer 4 |]) |] in
            actual |> should equal expected

    [<Test>] member x.
        ``when parsing simple sets`` () =
            let input = "#{1 2 3}" in
            let actual = EdnValue.Parse input in
            let expected = EdnValue.EdnSet [| EdnValue.Integer 1; EdnValue.Integer 2; EdnValue.Integer 3 |] in
            actual |> should equal expected

    [<Test>] member x.
        ``when parsing nested sets`` () =
            let input = "#{1 2 #{3 4}}" in
            let actual = EdnValue.Parse input in
            let expected = EdnValue.EdnSet [| EdnValue.Integer 1;
                                              EdnValue.Integer 2;
                                              (EdnValue.EdnSet [| EdnValue.Integer 3; EdnValue.Integer 4 |]) |] in
            actual |> should equal expected

    [<Test>] member x.
        ``when parsing arbitrarily nested collections`` () =
            let input = "[1 2 #{3 4} (5 [6 7]) 8]" in
            let actual = EdnValue.Parse input in
            let expected = EdnValue.EdnVector [| EdnValue.Integer 1;
                                                 EdnValue.Integer 2;
                                                 EdnValue.EdnSet [| EdnValue.Integer 3;
                                                                    EdnValue.Integer 4; |];
                                                 EdnValue.EdnList [| EdnValue.Integer 5;
                                                                     EdnValue.EdnVector [| EdnValue.Integer 6;
                                                                                           EdnValue.Integer 7 |] |];
                                                 EdnValue.Integer 8 |] in
            actual |> should equal expected

    [<Test>] member x.
        ``when parsing strings`` () =
            let inputs = ["\"foo\""; "\"foo\\\"bar\"";
                          "\"foo\\nbar\""; "\"foo\\rbar\""; "\"foo\\tbar\""] in
            let actual = List.map EdnValue.Parse inputs in
            let expected = List.map EdnValue.String ["foo"; "foo\"bar";
                                                     "foo\nbar"; "foo\rbar"; "foo\tbar"] in
            actual |> should equal expected

    [<Test>] member x.
        ``when parsing keywords`` () =
            let inputs = [":foo"; ":user/foo"] in
            let actual = List.map EdnValue.Parse inputs in
            let expected = [EdnValue.Keyword { ns = None; symbol = "foo" }
                            EdnValue.Keyword { ns = Some "user"; symbol = "foo" }] in
            actual |> should equal expected

    [<Test>] member x.
        ``when parsing keywords with two colons`` () =
            (fun () -> EdnValue.Parse "::foo" |> ignore) |> should throw typeof<System.Exception>

    [<Test>] member x.
        ``when parsing keywords with two slashes`` () =
            (fun () -> EdnValue.Parse ":foo/bar/baz" |> ignore) |> should throw typeof<System.Exception>

    [<Test>] member x.
        ``when parsing symbols`` () =
            let inputs = ["foo"; "true"; "false"; "nil"; "bar/bam" ] in
            let actual = List.map EdnValue.Parse inputs in
            let expected = [EdnValue.Symbol { ns = None; symbol = "foo" };
                            EdnValue.Boolean true;
                            EdnValue.Boolean false;
                            EdnValue.Null;
                            EdnValue.Symbol { ns = Some "bar"; symbol = "bam" }] in
            actual |> should equal expected

    [<Test>] member x.
        ``when parsing tagged uuid`` () =
            let uuid = System.Guid.NewGuid () in
            let inputs = [uuid] in
            let actual = List.map (fun x -> EdnValue.Parse ("#uuid \"" + x.ToString() + "\"")) inputs in
            let expected = [EdnValue.TaggedUuid uuid] in
            actual |> should equal expected

    [<Test>] member x.
        ``when parsing tagged inst`` () =
            let inputs = ["#inst \"2014-01-01T12:00:00.00Z\""] in
            let actual = List.map EdnValue.Parse inputs in
            let expected = [EdnValue.TaggedInst(new DateTime(2014, 1, 1, 12, 0, 0))] in
            actual |> should equal expected
