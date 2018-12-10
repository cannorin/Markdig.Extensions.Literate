Markdig.Extensions.Literate
===========================

This library provides a way to implement [literate programming](https://en.wikipedia.org/wiki/Literate_programming) of essentially any language within [Markdig](https://github.com/lunet-io/markdig) markdown parser.

It collects all the code blocks (not inline ones) found in a document and allow you to emit custom HTML instead of the default renderer.

You have to create three functions:

### `initState: LiterateCodeBlockSummary<string> list -> 'State`

You receive all the code blocks as an argument, and returns your 'state' object.

Code blocks are represented in a record type:

```fsharp
type LiterateCodeBlockSummary<string> = {
  index: int
  language: string
  commands: dict<string, string>
  code: string
}
```

The 'state' object may be the information of the parsed source code, if it is needed prior to actually render each code block.

### `renderCont: 'State -> LiterateCodeBlockSummary<string> -> 'State * (HtmlRenderer -> HtmlAttributes -> unit) option`

You receive the 'state' object and a code block as arguments, and returns a new 'state' object and (optionally) a continuation (callback function) to render a custom HTML.

The continuation can be `None` if you do not want to render HTML manually and use the default renderer provided by Markdig instead.

### `finalize: 'State -> HtmlRenderer -> unit`

You receive the 'state' object and a `HtmlRenderer`. If you write a HTML here, it is inserted to the end of the document.

## The simplest example

This just extracts all the F# source code and let the default renderer do the job.

```fsharp
let init callback codeBlocks =
  let code =
    codeBlocks
      |> List.map (fun codeBlock -> codeBlock.code)
      |> String.concat Environment.NewLine
  callback code
  ()

let cont state codeBlock = (state, None)

let finalize state renderer = ()

let code = ref ""

let pipeline =
  new MarkdownPipelineBuilder()
  |> Pipeline.useLiterateCodeBlock (init (fun s -> code := s)) cont finalize
  |> Pipeline.build

let toHtml md = Markdown.ToHtml(md, pipeline)
```

## Markdig.Extensions.Literate.FSharp

Currently, this does not work on .NET Core.

See Issue #1 .

``````fsharp
module Main
open Markdig
open Markdig.Extensions.Literate
open Markdig.Extensions.Literate.FSharp

[<EntryPoint>]
let main argv =
  let pipe =
    new MarkdownPipelineBuilder()
    |> Pipeline.useAdvancedExts
    |> Pipeline.useFSharpLiterate(lineNumbers=false)
    |> Pipeline.build

  let md = """
# Hello!

```fsharp
[hide]
let answer = 42
```

## 1

The *quick* fox **jumps** over the _lazy_ dog.

## 2 

```fsharp
let f x = x + answer
```

### 4

```fsharp
let g x y = f x + f y
```
"""

  Markdown.ToHtml(md, pipe) |> printfn "%s"
``````

## License

Available under Apache 2.0 license. See LICENSE.txt.
