﻿// --------------------------------------------------------------------------------------
// Copyright (c) Daniel Gregoire (semperos) 2014.
// This sample code is provided "as is" without warranty of any kind.
// We disclaim all warranties, either express or implied, including the
// warranties of merchantability and fitness for a particular purpose.
//
// A simple F# portable parser for EDN data
//
// Based on https://github.com/fsharp/FSharp.Data/blob/master/src%2FJson%2FJsonValue.fs
// --------------------------------------------------------------------------------------

namespace DataEdn

open System
open System.IO
open System.Text
open System.Globalization
open FSharp.Data
open FSharp.Data.Runtime
open FSharp.Data.Runtime.IO

/// Specifies the formatting behaviour of EDN values
[<RequireQualifiedAccess>]
type EdnSaveOptions =
  /// Format (indent) the EdnValue
  | None = 0
  /// Print the EdnValue in one line in a compact way
  | DisableFormatting = 1

type EdnKeyword =
  { ns: string option;
    keyword: string }

  member x.ToString =
    let sb = StringBuilder() in
    sb.Append ":" |> ignore
    match x.ns with
    | Some s -> sb.Append s
    | None -> sb
    |> ignore
    sb.Append x.keyword |> ignore
    sb.ToString()

type EdnSymbol =
  { ns: string option;
    symbol: string }
  
  member x.ToString =
    let sb = StringBuilder() in
    match x.ns with
    | Some s -> sb.Append s
    | None -> sb
    |> ignore
    sb.Append x.symbol |> ignore
    sb.ToString()

[<RequireQualifiedAccess>]
type EdnValue =
  | String of string
  | Keyword of EdnKeyword
  | Symbol of EdnSymbol
  | Integer of int
  | Float of float
  | EdnMap of properties:(EdnValue * EdnValue)[]
  | EdnList of elements:EdnValue[]
  | EdnVector of elements:EdnValue[]
  | EdnSet of elements:EdnValue[]
  | Boolean of bool
  | Null

  override x.ToString() = x.ToString(EdnSaveOptions.None)

  member x.ToString saveOptions =
    let rec serialize (sb:StringBuilder) indentation edn =
      let newLine plus =
        if saveOptions = EdnSaveOptions.None then
          sb.AppendLine() |> ignore
          System.String(' ', indentation + plus) |> sb.Append |> ignore
      let delimited (l : EdnValue[]) (left : string) (right : string) =
        sb.Append left |> ignore
        for element in l do
          newLine 2
          serialize sb (indentation + 2) element |> ignore
        if l.Length > 0 then newLine 0
        sb.Append right
      match edn with
      | Null -> sb.Append "nil"
      | Boolean b -> sb.Append(if b then "true" else "false")
      | Integer i -> sb.Append(i.ToString(CultureInfo.InvariantCulture))
      | Float f -> sb.Append(f.ToString(CultureInfo.InvariantCulture))
      | String s -> sb.Append("\"" + EdnValue.EdnStringEncode(s) + "\"")
      | Keyword k ->  sb.Append k.ToString
      | Symbol sym -> sb.Append sym.ToString
      | EdnMap m ->
          sb.Append "{" |> ignore
          for k, v in m do
            newLine 2
            sb.Append(k.ToString()) |> ignore
            serialize sb (indentation + 2) v |> ignore
          newLine 0
          sb.Append "}"
      | EdnList l -> delimited l "(" ")"
      | EdnVector v -> delimited v "[" "]"
      | EdnSet st -> delimited st "#{" "}"
    (serialize (new StringBuilder()) 0 x).ToString()

  static member internal EdnStringEncode (value : string) = 
    value

type EdnParser(ednText : string, tolerateErrors) =
    let cultureInfo = CultureInfo.InvariantCulture
    let mutable i = 0
    let s = ednText

    let skipWhiteSpace () =
      while i < s.Length && (Char.IsWhiteSpace s.[i] || s.[i] = ',') do
        i <- i + 1

    let isIntChar c =
      Char.IsDigit c || c = '+' || c = '-'// TODO Support suffix 'N'
    
    let isFloatChar c =
      Char.IsDigit c || c = '.' || c = 'e' || c = 'E' || c = '+' || c = '-' || c = 'M'

    let throw() =
      let msg =
        sprintf
          "Invalid EDN starting at character %d, snippet = \n----\n%s\n-----\nEDN = \n------\n%s\n-------"
          i (ednText.[(max 0 (i-10))..(min (ednText.Length-1) (i+10))]) (if ednText.Length > 1000 then ednText.Substring(0, 1000) else ednText)
      failwith msg
    let ensure cond = if not cond then throw()

    // Recursive decent parser for EDN using mutable index
    let rec parseValue () =
        skipWhiteSpace ()
        ensure (i < s.Length)
        // TODO Handle keywords, symbols, booleans, nil, and catch-all throw()
        match s.[i] with
        | '"' -> EdnValue.String(parseString ())
        | '-' -> parseNum ()
        | '+' -> parseNum ()
        | c when Char.IsDigit(c) -> parseNum ()
        | '{' -> parseMap ()
        | '(' -> parseDelimited EdnValue.EdnList '(' ')'
        | '[' -> parseDelimited EdnValue.EdnVector '[' ']'
        | '#' ->
            i <- i + 1
            ensure (i < s.Length)
            match s.[i] with
            | '{' -> parseDelimited EdnValue.EdnSet '{' '}'
            | _ -> throw ()
    
    and parseString () =
        ensure (i < s.Length && s.[i] = '"')
        i <- i + 1
        let buf = new StringBuilder ()
        while i < s.Length && s.[i] <> '"' do
            if s.[i] = '\\' then
                ensure (i+1 < s.Length)
                match s.[i+1] with
                | 'n' -> buf.Append('\n') |> ignore
                | 't' -> buf.Append('\t') |> ignore
                | 'r' -> buf.Append('\r') |> ignore
                | '\\' -> buf.Append('\\') |> ignore
                | '"' -> buf.Append('"') |> ignore
                | _ -> throw ()
                i <- i + 2 // skip past \ and next char
            else
                buf.Append(s.[i]) |> ignore
        ensure (i < s.Length && s.[i] = '"')
        i <- i + 1
        buf.ToString ()
    
    and parseNum () =
        let start = i
        while i < s.Length && (isIntChar s.[i] || isFloatChar s.[i]) do
            i <- i + 1
        let len = i - start
        match TextConversions.AsInteger cultureInfo (s.Substring(start,len)) with
        | Some x -> EdnValue.Integer x
        | _ ->
            match TextConversions.AsFloat [| |] false cultureInfo (s.Substring(start,len)) with
            | Some x -> EdnValue.Float x
            | _ -> throw ()

    and parseMap () =
        ensure (i < s.Length && s.[i] = '{')
        i <- i + 1
        skipWhiteSpace ()
        let items = ResizeArray<_>()
        while i < s.Length && s.[i] <> '}' do
            items.Add(parseValue())
            skipWhiteSpace ()
        ensure(items.Count % 2 = 0 && i < s.Length && s.[i] = '}')
        let pairs = ResizeArray<_>()
        let mutable j = 0
        while j < items.Count do
            pairs.Add (items.[j], items.[j+1])
            j <- j + 2
        i <- i + 1 // right brace
        EdnValue.EdnMap(pairs |> Array.ofSeq)

    and parseDelimited T (left : char) (right : char) =
        ensure (i < s.Length && s.[i] = left)
        i <- i + 1
        skipWhiteSpace ()
        let items = ResizeArray<_>()
        while i < s.Length && s.[i] <> right do
            items.Add(parseValue())
            skipWhiteSpace ()
        ensure (i < s.Length && s.[i] = right)
        i <- i + 1
        T(items |> Array.ofSeq)

    member x.Parse () =
        let value = parseValue()
        skipWhiteSpace ()
        if i <> s.Length then
            throw()
        value

type EdnValue with
        static member Parse(text) =
            EdnParser(text, false).Parse()