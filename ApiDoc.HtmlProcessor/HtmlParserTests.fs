module HtmlParserTests

open System
open NUnit.Framework
open Shared
open Parser
open HtmlParser

let to_char_list (str : string) = 
    str.ToCharArray()
    |> List.ofArray

let is_success (okOrThrow, symbols') = 
    printfn "okOrThrow = %A\nsymbol's = %A" okOrThrow symbols'
    okOrThrow |> OkOrThrow.isOk && symbols' = []
let assert_is_true (x : bool) = Assert.IsTrue(x)

[<Test>]
let ``Should parse self-standing tag with no attributes and no whitespaces.``() = 
    (to_char_list "<div/>") --> HtmlParser.self_standing_tag_parser 
    |> is_success
    |> assert_is_true
    
[<Test>]
let ``Should parse self-standing tag with no attributes and single whitespace.``() = 
    (to_char_list "<div />") --> HtmlParser.self_standing_tag_parser 
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse self-standing tag with no attributes and multiple whitespaces.``() = 
    (to_char_list "<div   />") --> HtmlParser.self_standing_tag_parser 
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse self-standing tag with single dobule-quoted attribute and single whitespace from the left.``() = 
    (to_char_list "<div att=\"value\"/>") --> HtmlParser.self_standing_tag_parser 
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse self-standing tag with single dobule-quoted attribute and single whitespaces from the left and right.``() = 
    (to_char_list "<div att=\"value\" />") --> HtmlParser.self_standing_tag_parser 
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse self-standing tag with single single-quoted attribute and single whitespace from the left.``() = 
    (to_char_list "<div att='value'/>") --> HtmlParser.self_standing_tag_parser 
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse self-standing tag with single signle-quoted attribute and single whitespaces from the left and right.``() = 
    (to_char_list "<div att='value' />") --> HtmlParser.self_standing_tag_parser 
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse self-standing tag with multiple single-quoted attributes and single whitespace from the left.``() = 
    (to_char_list "<div att='value' attr='val'/>") --> HtmlParser.self_standing_tag_parser 
    |> is_success
    |> assert_is_true
   
[<Test>]
let ``Should parse self-standing tag with multiple signle-quoted attributes and single whitespaces from the left and right.``() = 
    (to_char_list "<div att='value' attr='val' />") --> HtmlParser.self_standing_tag_parser 
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse self-standing tag with multiple double-quoted attributes and single whitespace from the left.``() = 
    (to_char_list "<div att=\"value\" attr=\"val\"/>") --> HtmlParser.self_standing_tag_parser 
    |> is_success
    |> assert_is_true
   
[<Test>]
let ``Should parse self-standing tag with multiple double-quoted attributes and single whitespaces from the left and right.``() = 
    (to_char_list "<div att=\"value\" attr=\"val\" />") --> HtmlParser.self_standing_tag_parser 
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse both self-standing tags.``() = 
    (to_char_list "<div att=\"value\" attr=\"val\" /><div/>") --> 
    (self_standing_tag_parser >>== self_standing_tag_parser)
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse many self-standing tags.``() = 
    (to_char_list "<div att=\"value\" attr=\"val\" /><div/><div att=\"value\" attr=\"val\"/><div />") --> 
    (self_standing_tag_parser |>| stringify |> accumulator (+))
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse closing tag without whitespaces.``() = 
    (to_char_list "</tag>") --> closing_tag_parser
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse closing tag with single whitespace.``() = 
    (to_char_list "</ tag>") --> closing_tag_parser
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse closing tag with multiple whitespace.``() = 
    (to_char_list "</  tag>") --> closing_tag_parser
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse opening tag without attributes and no whitespaces.``() = 
    (to_char_list "<tag>") --> opening_tag_parser
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse opening tag with single attribute and single whitespace.``() = 
    (to_char_list "<tag att=\"value\">") --> opening_tag_parser
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse opening tag with single attribute and single whitespace from both sides.``() = 
    (to_char_list "<tag att=\"value\" >") --> opening_tag_parser
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse opening tag with multiple attributes and single whitespace from both sides.``() = 
    (to_char_list "<tag att=\"value\" attr=\"val\">") --> opening_tag_parser
    |> is_success
    |> assert_is_true
