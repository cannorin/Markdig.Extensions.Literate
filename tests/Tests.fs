module Tests

open System
open Xunit
open FsUnit.Xunit
open Markdig
open Markdig.Extensions.Literate
open Markdig.Extensions.Literate.FSharp

let errors = ref [||]
let pipeline =
  new MarkdownPipelineBuilder()
  |> Pipeline.useAdvancedExts
  |> Pipeline.useFSharpLiterate(errorHandler = fun errs -> errors := errs)
  |> Pipeline.build

[<Fact>]
let ``Can parse and format markdown with Github-flavoured F# snippet`` () =
  let content = """
**hello**
```fsharp followed by some random text
let test = 42
```
"""
  ignore <| Markdown.ToHtml(content, pipeline)
  !errors |> Array.length |> should equal 0

[<Fact>]
let ``Can parse and format markdown with Github-flavoured F# snippet starting and ending with empty lines`` () =
  let content = """
```fsharp

let test = 42

```"""
  ignore <| Markdown.ToHtml(content, pipeline)
  !errors |> Array.length |> should equal 0

[<Fact>]
let ``Can report errors in F# code snippets (in Markdown document)`` () =
  let content = """
(** **hello** *)
let test = 4 + 1.0"""
  ignore <| Markdown.ToHtml(content, pipeline)
  !errors |> Array.length |> should be (greaterThan 0)

