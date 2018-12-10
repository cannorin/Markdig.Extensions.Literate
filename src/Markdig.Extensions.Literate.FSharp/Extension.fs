// Copyright (c) 2018 cannorin
// Available under Apache 2.0 License

namespace Markdig.Extensions.Literate.FSharp

open System
open System.Text
open System.Runtime.CompilerServices
open System.IO

open Markdig
open Markdig.Renderers
open Markdig.Renderers.Html
open Markdig.Extensions.Literate

open FSharp.Literate
open FSharp.Literate.Markdig
open FSharp.Markdown
open FSharp.CodeFormat

type private Impl () =
  static let parsingContext formatAgent evaluator compilerOptions definedSymbols =
    let agent =
      match formatAgent with
      | Some agent -> agent
      | _ -> CodeFormat.CreateAgent()
    { FormatAgent = agent
      CompilerOptions = compilerOptions
      Evaluator = evaluator
      DefinedSymbols = Option.map (String.concat ",") definedSymbols }

  static let formattingContext format prefix lineNumbers includeSource generateAnchors replacements =
    { Replacements = defaultArg replacements []
      GenerateLineNumbers = defaultArg lineNumbers true
      IncludeSource = defaultArg includeSource false
      Prefix = defaultArg prefix "fs"
      OutputKind = defaultArg format OutputKind.Html
      GenerateHeaderAnchors = defaultArg generateAnchors false
    }

  static member Initialize
    (path, fsiObj, fsiOptions,
     formatAgent, compilerOptions, definedSymbols,
     lineNumbers, includeSource, generateAnchors, replacements) =
    fun codeBlocks ->
      let path = path ?| "C:\\Document.fsx"
      let fsiOptions = fsiOptions ?| Array.empty

      let fsi =
        match fsiObj with
          | Some fsi -> new FsiEvaluator(fsiOptions, fsi)
          | None     -> new FsiEvaluator(fsiOptions)
      
      let parsingCtx =
        parsingContext formatAgent (Some (fsi :> IFsiEvaluator)) compilerOptions definedSymbols
      let formatCtx =
        formattingContext (Some OutputKind.Html) None lineNumbers includeSource generateAnchors replacements

      let evalMapping, errors =
        Transformations.createEvaluationMapping parsingCtx path codeBlocks
      let cbsEvaluated =
        codeBlocks |> List.map (fun code -> evalMapping |> Dict.tryFind code.index)
      let cbs' = cbsEvaluated |> List.choose id |> List.concat

      let replaceMapping, toolTip =
        Transformations.createReplacementMapping formatCtx cbs'
      let sb = new StringBuilder()
      use writer = new StringWriter(sb)
      let cbsReplaced =
        cbsEvaluated |> List.map (
          Option.map (
            List.map (Transformations.replaceSpecialCodes formatCtx replaceMapping)
            >> List.choose id
            >> fun xs ->
              Html.formatMarkdown
                writer formatCtx.GenerateHeaderAnchors Environment.NewLine
                true Dict.empty xs
              let html = sb.ToString()
              sb.Clear() |> ignore
              html
            ))
      
      List.zip
        (codeBlocks |> List.map (fun x -> x.index))
        cbsReplaced
      |> dict, toolTip, errors

  static member Render (mapping: dict<int, string option>, toolTip, errors) code =
    let cont =
      match mapping |> Dict.tryFind code.index with
        | None -> None
        | Some str ->
          Some <|
            fun (renderer: HtmlRenderer) (attributes: HtmlAttributes) ->
              str |> Option.iter (renderer.Write >> ignore)
    (mapping, toolTip, errors), cont

  static member Finalize handler (_, toolTip: string, errors) (renderer: HtmlRenderer) =
    renderer.Write toolTip |> ignore
    handler errors

module internal Interop =
  let inline fromCSAction1 (a: Action<_>) x = a.Invoke(x)

[<Extension; Sealed>]
type FSharpLiterateExtensions =
  [<Extension>]
  static member UseFSharpLiterate
    (this: MarkdownPipelineBuilder,
     ?path, ?fsiObj, ?fsiOptions,
     ?formatAgent, ?compilerOptions, ?definedSymbols,
     ?lineNumbers, ?includeSource, ?generateAnchors, ?replacements,
     ?errorHandler) =
    let handler =
      errorHandler |> Option.map Interop.fromCSAction1 ?| ignore
    Pipeline.useLiterateCodeBlock
      (Impl.Initialize (path, fsiObj, fsiOptions,
        formatAgent, compilerOptions, definedSymbols,
        lineNumbers, includeSource, generateAnchors, replacements))
      Impl.Render
      (Impl.Finalize handler)
      this

[<Sealed>]
type Pipeline =
  static member useFSharpLiterate
    (?path, ?fsiObj, ?fsiOptions,
     ?formatAgent, ?compilerOptions, ?definedSymbols,
     ?lineNumbers, ?includeSource, ?generateAnchors, ?replacements,
     ?errorHandler) =
    let handler = errorHandler ?| ignore
    Pipeline.useLiterateCodeBlock
      (Impl.Initialize (path, fsiObj, fsiOptions,
        formatAgent, compilerOptions, definedSymbols,
        lineNumbers, includeSource, generateAnchors, replacements))
      Impl.Render
      (Impl.Finalize handler)