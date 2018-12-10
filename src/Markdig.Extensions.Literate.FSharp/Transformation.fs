// Copyright 2011-2012, Tomas Petricek (http://tomasp.net)

// forked from https://github.com/fsprojects/FSharp.Formatting/blob/6ca4b3f/src/FSharp.Literate/Transformations.fs
// Copyright (c) 2018 cannorin
// Available under Apache 2.0 License

namespace FSharp.Literate.Markdig

open System
open System.IO

open Markdig.Extensions.Literate

open FSharp.CodeFormat
open FSharp.Literate
open FSharp.Literate.Transformations
open FSharp.Markdown
// open CSharpFormat

/// Functions from FSharp.Literate.Transformations modified to work with `LiterateCodeBlockSummary<string>`
module Transformations =
  let inline isFSharpSnippet x =
       x.commands |> Dict.tryFind "lang" |> Option.exists (String.contains "fsharp")
    || (not (String.IsNullOrEmpty x.language) && String.contains "fsharp" x.language)

  let inline getLanguage x =
    if String.IsNullOrEmpty x.language && x.commands |> Dict.containsKey "lang" then
      x.commands.["lang"]
    else x.language

  let collectCodeSnippet x = seq {
      if not <| String.IsNullOrWhiteSpace x.language
      && x |> isFSharpSnippet
      && x.commands |> Dict.containsKey "do-not-eval" |> not then
        let modul = x.commands |> Dict.tryFind "module"
        yield (modul, x.code)
    }

  let replaceCodeSnippet (path: string) codeLookup x =
    let cmds = x.commands
    if cmds |> Dict.containsKey "hide" then None else
    let code =
      if cmds |> Dict.containsKey "file" && cmds |> Dict.containsKey "key" then
        let file = Path.Combine (Path.GetDirectoryName path, cmds.["file"])
        let startTag, endTag = "[" + cmds.["key"] + "]", "[/" + cmds.["key"] + "]"
        let lines = File.ReadAllLines(file)
        let startIdx = lines |> Seq.findIndex (fun l -> l.Contains startTag)
        let endIdx = lines |> Seq.findIndex (fun l -> l.Contains endTag)
        lines.[startIdx + 1 .. endIdx - 1]
        
        |> String.concat "\n"
      else x.code
    if x |> isFSharpSnippet then
      let outputName = cmds |> Dict.tryFind "define-output"
      let snippetName = cmds |> Dict.tryFind "define"
      let donoteval = cmds |> Dict.containsKey "do-not-eval"

      let codeRef = cmds |> Dict.tryFind "include"
      let outputRef = cmds |> Dict.tryFind "include-output"
      let itRef = cmds |> Dict.tryFind "include-it"
      let valueRef = cmds |> Dict.tryFind "include-value"

      if outputName.IsSome || snippetName.IsSome || donoteval then
        Some <|
          LiterateCode (codeLookup |> Dict.find code,
            { Evaluate = not donoteval; OutputName = outputName;
              Visibility = snippetName |> Option.map NamedCode ?| VisibleCode})
      else
        match codeRef, outputRef, itRef, valueRef with
          | Some x, _, _, _ -> CodeReference x |> Some
          | _, Some x, _, _ -> OutputReference x |> Some
          | _, _, Some x, _ -> ItValueReference x |> Some
          | _, _, _, Some x -> ValueReference x |> Some
          | None, None, None, None ->
            Some <| FormattedCode (codeLookup |> Dict.find code)
    else

      Some <| LanguageTaggedCode (x |> getLanguage, code)

  let formatCodeSnippets ctx (path: string) (doc: LiterateCodeBlockSummary<string> list) =
    let name = Path.GetFileNameWithoutExtension(path)

    // Extract all CodeBlocks and pass them to F# snippets
    let codes = doc |> Seq.collect collectCodeSnippet |> Array.ofSeq
    let snippetLookup, errors = 
      if codes.Length = 0 then dict [], [||] else
        // If there are some F# snippets, we build an F# source file
        let blocks = codes |> Seq.mapi (fun index -> function
          | Some modul, code ->
              // Generate module & add indentation
              "module " + modul + " =\n" +
              "// [snippet:" + (string index) + "]\n" +
              "    " + code.Replace("\n", "\n    ") + "\n" +
              "// [/snippet]"
          | None, code ->
              "// [snippet:" + (string index) + "]\n" +
              code + "\n" +
              "// [/snippet]" ) 
        let modul = "module " + (new String(name |> Seq.filter Char.IsLetter |> Seq.toArray))
        let source = modul + "\r\n" + (String.concat "\n\n" blocks)

        // Process F# script file & build lookup table for replacement
        let snippets, errors = 
          ctx.FormatAgent.ParseSource
            ( Path.ChangeExtension(path, ".fsx"), source, 
              ?options = ctx.CompilerOptions, ?defines = ctx.DefinedSymbols )
        [ for (_, code), (Snippet(_, fs)) in Array.zip codes snippets -> 
            code, fs |> List.map (fun x -> x)
        ] |> dict, errors
    
    // Replace code blocks with formatted snippets in the document
    let newPars = doc |> List.map (replaceCodeSnippet path snippetLookup)
    newPars, errors

  (*
  /// Represents key in a dictionary with evaluation results
  type EvalKey = OutputRef of string | ValueRef of string
  
  /// Unparse a Line list to a string - for evaluation by fsi.
  let unparse (lines: Line list) =
    let joinLine (Line spans) =
      spans
      |> Seq.map (fun span -> match span with Token (_,s,_) -> s | Omitted (s1,s2) -> s2 | _ -> "")
      |> String.concat ""
    lines
    |> Seq.map joinLine
    |> String.concat "\n"

  /// Replace all special 'LiterateParagraph' elements recursively using the given lookup dictionary
  let rec replaceSpecialCodes ctx (formatted: dict<_, _>) = function
    | Matching.LiterateParagraph(special) -> 
        match special with
        | RawBlock lines -> Some (InlineBlock(unparse lines, None))
        | LiterateCode(_, { Visibility = (HiddenCode | NamedCode _) }) -> None
        | FormattedCode lines 
        | LiterateCode(lines, _) -> Some (formatted.[Choice1Of2 lines])
        | CodeReference ref -> Some (formatted.[Choice2Of2 ref])
        | OutputReference _  
        | ItValueReference _  
        | ValueReference _ -> 
            failwith "Output, it-value and value references should be replaced by FSI evaluator"
        | LanguageTaggedCode(lang, code) -> 
            let inlined = 
              match ctx.OutputKind with
              | OutputKind.Html ->
                  let sb = new System.Text.StringBuilder()
                  let writer = new System.IO.StringWriter(sb)
                  writer.Write("<table class=\"pre\">")
                  writer.Write("<tr>")
                  if ctx.GenerateLineNumbers then 
                    // Split the formatted code into lines & emit line numbers in <td>
                    // (Similar to formatSnippets in FSharp.CodeFormat\HtmlFormatting.fs)
                    let lines = code.Trim('\r', '\n').Replace("\r\n", "\n").Replace("\n\r", "\n").Replace("\r", "\n").Split('\n')
                    let numberLength = lines.Length.ToString().Length
                    let linesLength = lines.Length
                    writer.Write("<td class=\"lines\"><pre class=\"fssnip\">")
                    for index in 0..linesLength-1 do
                      let lineStr = (index + 1).ToString().PadLeft(numberLength)
                      writer.WriteLine("<span class=\"l\">{0}: </span>", lineStr)
                    writer.Write("</pre>")
                    writer.WriteLine("</td>")

                  writer.Write("<td class=\"snippet\">")
                  
                  match SyntaxHighlighter.FormatCode(lang, code) with
                  | true, code -> Printf.fprintf writer "<pre class=\"fssnip highlighted\"><code lang=\"%s\">%s</code></pre>" lang code
                  | false, code -> Printf.fprintf writer "<pre class=\"fssnip\"><code lang=\"%s\">%s</code></pre>" lang code

                  writer.Write("</td></tr></table>")
                  sb.ToString()

              | OutputKind.Latex ->
                  sprintf "\\begin{lstlisting}\n%s\n\\end{lstlisting}" code
            Some(InlineBlock(inlined, None))
    // Traverse all other structures recursively
    | Matching.ParagraphNested(pn, nested) ->
        let nested = List.map (List.choose (replaceSpecialCodes ctx formatted)) nested
        Some(Matching.ParagraphNested(pn, nested))
    | par -> Some par
  *)
  
  let rec evalBlocks (fsi:IFsiEvaluator) file acc (paras:LiterateParagraph list) = 
    match paras with
      | [] -> acc
      | para :: paras ->
        match para with
          | LiterateCode(snip, opts) ->
              let acc =
                if opts.Evaluate then
                  let text = unparse snip
                  let result = fsi.Evaluate(text, false, Some file)
                  match opts.OutputName with
                  | Some n -> (OutputRef n, result) :: acc
                  | _ -> acc
                else acc
              evalBlocks fsi file acc paras

          | FormattedCode(snip) ->
              // Need to eval because subsequent code might refer it, but we don't need result
              let text = unparse snip
              let result = fsi.Evaluate(text, false, Some file)
              evalBlocks fsi file acc paras 

          | ValueReference(ref) -> 
              let result = fsi.Evaluate(ref, true, Some file)
              evalBlocks fsi file ((ValueRef ref,result) :: acc) paras

          | _ -> evalBlocks fsi file acc paras


  /// Given an evaluator and document, evaluate all code snippets and return a map with
  /// their results - the key is `ValueRef(name)` for all value references and 
  /// `OutputRef(name)` for all references to the snippet console output
  let evalAllSnippets fsi sourceFile doc = 
    evalBlocks fsi sourceFile [] doc |> Map.ofList

  // ---------------------------------------------------------------------------------------------
  // Evaluate all snippets and replace evaluation references with the results
  // ---------------------------------------------------------------------------------------------

  let rec replaceEvaluations ctx (evaluationResults:Map<_, IFsiEvaluationResult>) special =
    let (|EvalFormat|_|) = function
      | OutputReference(ref) -> Some(evaluationResults.TryFind(OutputRef ref), ref, FsiEmbedKind.Output)
      | ItValueReference(ref) -> Some(evaluationResults.TryFind(OutputRef ref), ref, FsiEmbedKind.ItValue)
      | ValueReference(ref) -> Some(evaluationResults.TryFind(ValueRef ref), ref, FsiEmbedKind.Value)
      | _ -> None
    match special with 
      | EvalFormat(Some result, _, kind) -> ctx.Evaluator.Value.Format(result, kind)
      | EvalFormat(None, ref, _) -> [ CodeBlock("Could not find reference '" + ref + "'", "", "", None) ]
      | other -> [ EmbedParagraphs(other, None) ]

  /// Transform the specified literate document & evaluate all F# snippets
  let evaluateCodeSnippets ctx sourceFile doc =
    match ctx.Evaluator with
    | Some fsi ->
        let evaluationResults = evalAllSnippets fsi sourceFile doc
        List.map (replaceEvaluations ctx evaluationResults) doc
    | None -> failwith "no evaluator available"
  
  let createEvaluationMapping ctx path (codeBlocks: LiterateCodeBlockSummary<string> list) =
    let formattedSnippets, errors =
      formatCodeSnippets ctx path codeBlocks
    let mapping =
      match ctx.Evaluator with
        | Some fsi -> evalAllSnippets fsi path (List.choose id formattedSnippets)
        | None -> failwith "no evaluator available"
    List.zip codeBlocks formattedSnippets
    |> List.map (fun (code, snippet) ->
      code.index, Option.map (replaceEvaluations ctx mapping) snippet ?| [])
    |> dict, errors
  
  (*
  /// Collect all code snippets in the document (so that we can format all of them)
  /// The resulting dictionary has Choice as the key, so that we can distinguish 
  /// between moved snippets and ordinary snippets
  let rec collectCodes par = seq {
    match par with 
    | Matching.LiterateParagraph(LiterateCode(lines, { Visibility = NamedCode id })) -> 
        yield Choice2Of2(id), lines
    | Matching.LiterateParagraph(LiterateCode(lines, _)) 
    | Matching.LiterateParagraph(FormattedCode(lines)) -> 
        yield Choice1Of2(lines), lines
    | Matching.ParagraphNested(pn, nested) ->
        yield! Seq.collect (Seq.collect collectCodes) nested
    | _ -> () }
  *)
  
  let createReplacementMapping ctx (doc: MarkdownParagraphs) = 
    let replacements = Seq.collect collectCodes doc
    let snippets = [| for _, r in replacements -> Snippet("", r) |]
    
    // Format all snippets and build lookup dictionary for replacements
    let formatted =
      match ctx.OutputKind with
      | OutputKind.Html -> 
          let openTag = "<pre class=\"fssnip highlighted\"><code lang=\"fsharp\">"
          let closeTag = "</code></pre>"
          let openLinesTag = "<pre class=\"fssnip\">"
          let closeLinesTag = "</pre>"
          CodeFormat.FormatHtml
            ( snippets, ctx.Prefix, openTag, closeTag, 
              openLinesTag, closeLinesTag, ctx.GenerateLineNumbers, false)
      | OutputKind.Latex -> CodeFormat.FormatLatex(snippets, ctx.GenerateLineNumbers)
    let lookup = 
      [ for (key, _), fmtd in Seq.zip replacements formatted.Snippets -> 
          key, InlineBlock(fmtd.Content, None) ] |> dict 
    lookup, formatted.ToolTip
