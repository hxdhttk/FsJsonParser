module FsJsonParser

open System

open TextInput
open ParserLibrary

type JValue =
    | JString of string
    | JNumber of float
    | JBool of bool
    | JNull
    | JObject of Map<string, JValue>
    | JArray of JValue list

let (>>%) p x =
    p |>> (fun _ -> x)

let jNull =
    pstring "null"
    >>% JNull
    <?> "null"

let jBool =
    let jtrue =
        pstring "true"
        >>% JBool true
    let jfalse =
        pstring "false"
        >>% JBool false
    jtrue <|> jfalse
    <?> "bool"

