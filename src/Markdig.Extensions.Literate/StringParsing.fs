// --------------------------------------------------------------------------------------
// F# Markdown (StringParsing.fs)
// (c) Tomas Petricek, 2012, Available under Apache 2.0 license.
// --------------------------------------------------------------------------------------

// note that @cannorin has removed some part of the original code
// as of https://github.com/fsprojects/FSharp.Formatting/commit/16383d8894bef27d0d273b7775bda703a6238072

module internal FSharp.Patterns

open System
open FSharp.Collections

// --------------------------------------------------------------------------------------
// Active patterns that simplify parsing of strings and lists of strings (lines)
// --------------------------------------------------------------------------------------

module String =
  /// Matches when a string is a whitespace or null
  let (|WhiteSpace|_|) (s) = 
    if String.IsNullOrWhiteSpace(s) then Some() else None

  /// Returns a string trimmed from both start and end
  let (|TrimBoth|) (text:string) = text.Trim()

  /// Matches when a string starts with the specified sub-string
  let (|StartsWith|_|) (start:string) (text:string) = 
    if text.StartsWith(start) then Some(text.Substring(start.Length)) else None

  /// Matches when a string starts with the specified sub-string
  /// The matched string is trimmed from all whitespace.
  let (|StartsWithTrim|_|) (start:string) (text:string) = 
    if text.StartsWith(start) then Some(text.Substring(start.Length).Trim()) else None

  /// Matches when a string starts with the given value and ends 
  /// with a given value (and returns the rest of it)
  let (|StartsAndEndsWith|_|) (starts, ends) (s:string) =
    if s.StartsWith(starts) && s.EndsWith(ends) && 
       s.Length >= starts.Length + ends.Length then 
      Some(s.Substring(starts.Length, s.Length - starts.Length - ends.Length))
    else None

  /// Matches when a string starts with the given value and ends 
  /// with a given value (and returns trimmed body)
  let (|StartsAndEndsWithTrim|_|) args = function
    | StartsAndEndsWith args (TrimBoth res) -> Some res
    | _ -> None

  /// Matches when a string starts with a sub-string wrapped using the 
  /// opening and closing sub-string specified in the parameter.
  /// For example "[aa]bc" is wrapped in [ and ] pair. Returns the wrapped
  /// text together with the rest.
  let (|StartsWithWrapped|_|) (starts:string, ends:string) (text:string) = 
    if text.StartsWith(starts) then 
      let id = text.IndexOf(ends, starts.Length)
      if id >= 0 then 
        let wrapped = text.Substring(starts.Length, id - starts.Length)
        let rest = text.Substring(id + ends.Length, text.Length - id - ends.Length)
        Some(wrapped, rest)
      else None
    else None

  /// Ignores everything until a end-line character is detected, returns the remaining string.
  let (|SkipSingleLine|) (text:string) =
    let rec tryEol eolList =
      match eolList with
      | h : string :: t ->
        match text.IndexOf(h) with
        | i when i < 0 -> tryEol t
        | i ->
          text.Substring (i + h.Length)
      | _ ->
        text
    let result = tryEol [ "\r\n"; "\n" ]
    let skipped = text.Substring(0, text.Length - result.Length)
    result

  /// Given a list of lines indented with certan number of whitespace 
  /// characters (spaces), remove the spaces from the beginning of each line 
  /// and return the string as a list of lines
  let removeSpaces lines =
    let spaces =
      lines 
      |> Seq.filter (String.IsNullOrWhiteSpace >> not)
      |> Seq.map (fun line -> line |> Seq.takeWhile Char.IsWhiteSpace |> Seq.length)
      |> fun xs -> if Seq.isEmpty xs then 0 else Seq.min xs
    lines 
    |> Seq.map (fun line -> 
        if String.IsNullOrWhiteSpace(line) then ""
        else line.Substring(spaces))

module List =
  /// Matches a list if it starts with a sub-list that is delimited
  /// using the specified delimiters. Returns a wrapped list and the rest.
  let inline (|DelimitedWith|_|) startl endl input = 
    if List.startsWith startl input then
      match List.partitionUntilEquals endl (List.skip startl.Length input) with 
      | Some(pre, post) -> Some(pre, List.skip endl.Length post, startl.Length, endl.Length)
      | None -> None
    else None

  /// Matches a list if it starts with a sub-list. Returns the list.
  let inline (|StartsWith|_|) startl input = 
    if List.startsWith startl input then Some input else None

  /// Matches a list if it starts with a sub-list that is delimited
  /// using the specified delimiter. Returns a wrapped list and the rest.
  let inline (|Delimited|_|) str = (|DelimitedWith|_|) str str

  let inline (|DelimitedNTimes|_|) str input =
    let strs, items = List.partitionWhile (fun i -> i = str) input
    match strs with
    | h :: _ ->
      (|Delimited|_|) (List.init strs.Length (fun _ -> str)) input
    | _ -> None

  /// Matches a list if it starts with a bracketed list. Nested brackets
  /// are skipped (by counting opening and closing brackets) and can be 
  /// escaped using the '\' symbol.
  let (|BracketDelimited|_|) startc endc input =
    let rec loop acc count = function
      | '\\'::x::xs when x = endc -> loop (x::acc) count xs
      | x::xs when x = endc && count = 0 -> Some(List.rev acc, xs)
      | x::xs when x = endc -> loop (x::acc) (count - 1) xs
      | x::xs when x = startc -> loop (x::acc) (count + 1) xs
      | x::xs -> loop (x::acc) count xs
      | [] -> None
    match input with
    | x::xs when x = startc -> loop [] 0 xs
    | _ -> None

  /// Returns a list of characters as a string.
  let (|AsString|) chars = String(Array.ofList chars)

module Lines = 
  /// Removes blank lines from the start and the end of a list
  let (|TrimBlank|) lines = 
    lines
    |> List.skipWhile (fun (s, n) -> String.IsNullOrWhiteSpace s) |> List.rev
    |> List.skipWhile (fun (s, n) -> String.IsNullOrWhiteSpace s) |> List.rev

  /// Matches when there are some lines at the beginning that are 
  /// either empty (or whitespace) or start with the specified string.
  /// Returns all such lines from the beginning until a different line.
  let (|TakeStartingWithOrBlank|_|) start input = 
    match List.partitionWhile (fun s -> 
            String.IsNullOrWhiteSpace s || s.StartsWith(start)) input with
    | matching, rest when matching <> [] -> Some(matching, rest)
    | _ -> None

/// Parameterized pattern that assigns the specified value to the 
/// first component of a tuple. Usage:
///
///    match str with
///    | Let 1 (n, "one") | Let 2 (n, "two") -> n
/// 
let (|Let|) a b = (a, b)

open System.Collections.Generic

/// Utility for parsing commands. Commands can be used in different places. We 
/// recognize `key1=value, key2=value` and also `key1:value, key2:value`
/// The key of the command should be identifier with just 
/// characters in it - otherwise, the parsing fails.
let (|ParseCommands|_|) (str:string) = 
  let kvs = 
    [ for cmd in str.Split(',') do
        let kv = cmd.Split([| '='; ':' |])
        if kv.Length = 2 then yield kv.[0].Trim(), kv.[1].Trim()
        elif kv.Length = 1 then yield kv.[0].Trim(), "" ] 
  let allKeysValid = 
    kvs |> Seq.forall (fst >> Seq.forall (fun c -> Char.IsLetter c || c = '_' || c = '-'))
  if allKeysValid && kvs <> [] then Some(dict kvs) else None

/// Utility for parsing commands - this deals with a single command.
/// The key of the command should be identifier with just 
/// characters in it - otherwise, the parsing fails.
let (|ParseCommand|_|) (cmd:string) = 
  let kv = cmd.Split([| '='; ':' |])
  if kv.Length >= 1 && not (Seq.forall Char.IsLetter kv.[0]) then None
  elif kv.Length = 2 then Some(kv.[0].Trim(), kv.[1].Trim())
  elif kv.Length = 1 then Some(kv.[0].Trim(), "")
  else None
  
/// Lookup in a dictionary
let (|Command|_|) k (d:IDictionary<_, _>) =
  match d.TryGetValue(k) with
  | true, v -> Some v
  | _ -> None 
