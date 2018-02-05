module HtmlParserTests

open System
open NUnit.Framework
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
    (to_char_list "<div/>") --> HtmlParser.self_standing_tag_without_attributes_parser 
    |> is_success
    |> assert_is_true
    
[<Test>]
let ``Should parse self-standing tag with no attributes and single whitespace.``() = 
    (to_char_list "<div />") --> HtmlParser.self_standing_tag_without_attributes_parser 
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse self-standing tag with no attributes and multiple whitespaces.``() = 
    (to_char_list "<div   />") --> HtmlParser.self_standing_tag_without_attributes_parser 
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse self-standing tag with single dobule-quoted attribute and single whitespace from the left.``() = 
    (to_char_list "<div att=\"value\"/>") --> HtmlParser.self_standing_tag_with_1_or_more_atts_parser 
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse self-standing tag with single dobule-quoted attribute and single whitespaces from the left and right.``() = 
    (to_char_list "<div att=\"value\" />") --> HtmlParser.self_standing_tag_with_1_or_more_atts_parser 
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse self-standing tag with single single-quoted attribute and single whitespace from the left.``() = 
    (to_char_list "<div att='value'/>") --> HtmlParser.self_standing_tag_with_1_or_more_atts_parser 
    |> is_success
    |> assert_is_true

[<Test>]
let ``Should parse self-standing tag with single signle-quoted attribute and single whitespaces from the left and right.``() = 
    (to_char_list "<div att='value' />") --> HtmlParser.self_standing_tag_with_1_or_more_atts_parser 
    |> is_success
    |> assert_is_true
