// Copyright (c) 2018 cannorin
// Available under Apache 2.0 License

namespace Markdig.Extensions.Literate

open System
open System.Runtime.CompilerServices

open Markdig
open Markdig.Syntax
open Markdig.Parsers
open Markdig.Renderers
open Markdig.Renderers.Html

open FSharp.Patterns

type LiterateCodeBlockSummary<'code> = {
  index: int
  language: string
  commands: dict<string, string>
  code: 'code
} with
  static member inline Map (x, f) =
    {
      index = x.index
      language = x.language
      commands = x.commands
      code = f x.code
    } 

module LiterateCodeBlockSummary =
  let inline map f x = LiterateCodeBlockSummary<_>.Map (x, f)

type LiterateCodeBlock(parser, index, ?indentCount: int) as this =
  inherit FencedCodeBlock(parser)

  let mutable parsed = None
  let mutable code = None

  let getCode () =
    (?|) code <|
      let c =
        seq {
          for line in this.Lines.Lines do
            match line.Slice.Text with
              | null -> ()
              | x ->
                yield x |> String.substring line.Slice.Start line.Slice.Length
        } |> String.concat Environment.NewLine
      code <- Some c
      c
  
  let parse () =
    (?|) parsed <|
      let x =
        match getCode () with
          | (String.StartsWithWrapped ("[", "]") (ParseCommands cmds, String.SkipSingleLine code))
          | Let (dict []) (cmds, code) ->
            Some cmds, code
          | code -> None, code
      parsed <- Some x
      x

  member val internal IndentCount = indentCount ?| 0 with get,set
  member val Index = index
  member __.Code with get() = parse () |> snd
  member __.Commands with get() = parse () |> fst ?| Dict.empty
  member __.Summary with get () = { index = __.Index; language = __.Info; commands = __.Commands; code = __.Code }

#nowarn "40" // suppress recursive object warning 
type EndOfDocument(parser) =
  inherit CodeBlock(parser)

and LiterateCodeBlockParser() as this =
  inherit FencedBlockParserBase<LiterateCodeBlock>()
  [<Literal>] static let defaultInfoPrefix = "language-"
  
  do this.OpeningCharacters <- [| '`'; '~' |]
  do this.InfoPrefix <- defaultInfoPrefix
  
  let counter = ref 0
  let mutable allCodeBlocks = []

  let rec addCodeBlock =
    new ProcessBlockDelegate(fun _ b ->
      this.remove_Closed addCodeBlock
      match b with
        | :? LiterateCodeBlock as x ->
          allCodeBlocks <- 
           x.Summary :: allCodeBlocks
        | _ -> ()
    )

  let rec addEndOfDocument =
    new ProcessInlineDelegate(fun state _ ->
      state.Document.remove_ProcessInlinesEnd addEndOfDocument
      state.Document.Add(new EndOfDocument(this :> BlockParser))
    )

  static member inline DefaultInfoPrefix = defaultInfoPrefix

  member __.Reset() =
    counter := 0
    allCodeBlocks <- []
  
  member __.ParsedCodeBlocks with get() = allCodeBlocks

  override this.CreateFencedBlock proc =
    let x = new LiterateCodeBlock(this :> BlockParser, !counter, proc.Indent)
    let i = !counter
    this.add_Closed addCodeBlock
    if i = 0 then
      proc.Document.add_ProcessInlinesEnd addEndOfDocument
    incr counter
    x

  override this.TryContinue (proc, block) =
    let result = base.TryContinue (proc, block)
    if result = BlockState.Continue then
      let l = block :?> LiterateCodeBlock
      let mutable c = proc.CurrentChar
      let mutable i = l.IndentCount
      while i > 0 && Char.IsWhiteSpace c do
        i <- i - 1
        c <- proc.NextChar()
    result

type LiterateCodeBlockRenderer<'State>
  (
    underlyingRenderer: HtmlObjectRenderer<CodeBlock>,
    initializeState:    LiterateCodeBlockSummary<string> list -> 'State,
    renderContinuation: 'State -> LiterateCodeBlockSummary<string> ->
                        'State * (HtmlRenderer -> HtmlAttributes -> unit) option,
    finalize: 'State -> HtmlRenderer -> unit
  ) =
  inherit HtmlObjectRenderer<CodeBlock>()

  let mutable state : 'State option = None

  override __.Write(renderer: HtmlRenderer, block: CodeBlock) =
    let inline fallback () =
      underlyingRenderer.Write(renderer, block)

    match block with
      | :? EndOfDocument as eod ->
        state |> Option.iter (fun state -> finalize state renderer)
        state <- None
        match eod.Parser with
          | :? LiterateCodeBlockParser as parser ->
            parser.Reset()
          | _ -> ()
      | :? LiterateCodeBlock as lcb ->
        match lcb.Parser with
          | :? LiterateCodeBlockParser as parser ->
            let attrs =
              block.TryGetAttributes() |> Coalesce.obj (lazy new HtmlAttributes())
            let st =
              match state with
                | Some x -> x
                | None ->
                  let x = initializeState (List.rev parser.ParsedCodeBlocks)
                  state <- Some x
                  x
            let st', k =
              renderContinuation st lcb.Summary
            match k with
              | Some f -> f renderer attrs
              | None   -> fallback ()
            state <- Some st'
          | _ -> fallback()
      | _ -> fallback ()

type LiterateCodeBlockExtension<'State>
  (
    initializeState:    LiterateCodeBlockSummary<string> list -> 'State,
    renderContinuation: 'State -> LiterateCodeBlockSummary<string> ->
                        'State * (HtmlRenderer -> HtmlAttributes -> unit) option,
    finalize: 'State -> HtmlRenderer -> unit
  ) =
  interface IMarkdownExtension with
    member __.Setup (pipeline: MarkdownPipelineBuilder) =
      if not <| pipeline.BlockParsers.Contains<LiterateCodeBlockParser>() then
        if not <| 
           pipeline.BlockParsers.Replace<FencedCodeBlockParser>(
             new LiterateCodeBlockParser())
        then
          failwith "Error in LiterateCodeBlockExtension.Setup(_): failed to replace the default FencedCodeBlockParser."
    member __.Setup (_, renderer) =
      match renderer with
        | null -> ()
        | :? TextRendererBase<HtmlRenderer> as trb ->
          let orig, exists =
            match trb.ObjectRenderers.Find<HtmlObjectRenderer<CodeBlock>>() with
              | null -> new CodeBlockRenderer() :> HtmlObjectRenderer<CodeBlock>, false
              | x -> x, true
          let lcbr = new LiterateCodeBlockRenderer<_>(orig, initializeState, renderContinuation, finalize)
          if exists then
            trb.ObjectRenderers.Remove orig |> ignore
          trb.ObjectRenderers.Add(lcbr)
        | _ -> ()

module internal Interop =
  let inline fromCSFunc1 (f: Func<'a, 'b>) x = f.Invoke x
  let inline fromCSFunc2 (f: Func<'a, 'b, 'c>) x y = f.Invoke(x, y)
  let inline fromCSAction2 (a: Action<_, _>) x y = a.Invoke(x, y)

[<Extension; Sealed>]
type LiterateCodeBlockExtensions =
  [<Extension>]
  /// <summary>
  /// Hooks the default `FencedCodeBlock` behaviour and allow the user to implement
  /// iterate programming feature on any language.
  /// </summary>
  /// <param name="initState">
  /// Create an initial state value from all the code blocks found in the markdown document.
  /// If you need the entire source code, it is only accessible from here.
  /// </param>
  /// <param name="renderCont">
  /// Process each code block and return a tuple of a new state and a continuation.
  /// If you do not need to manually generate the HTML output,
  /// return `None` as a continuation and it will fallback to the default implementation.
  /// </param>
  /// <param name="finalize">
  /// Used to insert something to the end of the document.
  /// </param>
  static member UseLiterateCodeBlock
    (
      this: MarkdownPipelineBuilder,
      initState: Func<_, 'State>,
      renderCont,
      finalize
    ) =
    this.Extensions.Add(
      new LiterateCodeBlockExtension<_>(
        Interop.fromCSFunc1 initState,
        (fun x y ->
          (Interop.fromCSFunc2 renderCont) x y
          |> Tuple.map2 id (Option.map Interop.fromCSFunc2)),
        Interop.fromCSAction2 finalize
      ))
    this

module Pipeline =
  let inline build (this: MarkdownPipelineBuilder) = this.Build()
  let inline useExt<'Ext when 'Ext: not struct and 'Ext :> IMarkdownExtension and 'Ext: (new: unit -> 'Ext)>
    (this: MarkdownPipelineBuilder) = this.Use<'Ext>()
  let inline useAdvancedExts (this: MarkdownPipelineBuilder) = this.UseAdvancedExtensions()

  /// <summary>
  /// Hooks the default `FencedCodeBlock` behaviour and allow the user to implement
  /// iterate programming feature on any language.
  /// </summary>
  /// <param name="initState">
  /// Create an initial state value from all the code blocks found in the markdown document.
  /// If you need the entire source code, it is only accessible from here.
  /// </param>
  /// <param name="renderCont">
  /// Process each code block and return a tuple of a new state and a continuation.
  /// If you do not need to manually generate the HTML output,
  /// return `None` as a continuation and it will fallback to the default implementation.
  /// </param>
  /// <param name="finalize">
  /// Used to insert something to the end of the document.
  /// </param>
  let inline useLiterateCodeBlock (initState: _ -> 'State) renderCont finalize (this: MarkdownPipelineBuilder) =
    this.Extensions.Add(
      new LiterateCodeBlockExtension<_>(initState, renderCont, finalize))
    this
