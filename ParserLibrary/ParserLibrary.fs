module ParserLibrary

open System

open TextInput

type Input = TextInput.InputState

type ParserPosition = {
    currentLine: string
    line: int
    column: int
}

let parserPositionFromInputState (inputState: Input) = {
    currentLine = TextInput.currentLine inputState
    line = inputState.position.line
    column = inputState.position.column
}

type ParserLabel = string
type ParserError = string

type Result<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * ParserPosition

type Parser<'T> = {
    parseFn: (Input -> Result<'T * Input>)
    label: ParserLabel
}

let printResult result =
    match result with
    | Success (value, _) ->
        printfn "%A" value
    | Failure (label, error, parserPos) ->
        let errorLine = parserPos.currentLine
        let colPos = parserPos.column
        let linePos = parserPos.line
        let failureCaret = sprintf "%*s^%s" colPos "" error
        printfn "Line: %i Col:%i Error parsing %s\n%s\n%s" linePos colPos label errorLine failureCaret

let getLabel parser =
    parser.label

let setLabel parser newLabel =
    let newInnerFn input =
        let result = parser.parseFn input
        match result with
        | Success s ->
            Success s
        | Failure (_, err, pos) ->
            Failure (newLabel, err, pos)
    {parseFn = newInnerFn; label = newLabel}

let (<?>) = setLabel

let runOnInput parser input =
    parser.parseFn input

let run parser inputStr =
    runOnInput parser (TextInput.fromStr inputStr)

let bindP f p =
    let label = "unknown"
    let innerFn input =
        let result1 = runOnInput p input
        match result1 with
        | Failure (label, err, pos) ->
            Failure (label, err, pos)
        | Success (value1, remainingInput) ->
            let p2 = f value1
            runOnInput p2 remainingInput
    {parseFn = innerFn; label = label}

let (>>=) p f = bindP f p

let returnP x =
    let label = sprintf "%A" x
    let innerFn input =
        Success (x, input)
    {parseFn = innerFn; label = label}

let andThen parser1 parser2 =
    let label = sprintf "%s andThen %s" (getLabel parser1) (getLabel parser2)
    parser1 >>= (fun p1Result ->
    parser2 >>= (fun p2Result ->
        returnP (p1Result, p2Result)))
    <?> label

let (.>>.) = andThen

let orElse parser1 parser2 =
    let label = sprintf "%A orElse %A" (getLabel parser1) (getLabel parser2)
    let innerFn input =
        let result1 = runOnInput parser1 input
        match result1 with
        | Success result -> 
            result1
        | Failure _ -> 
            let result2 = runOnInput parser2 input
            result2
    {parseFn = innerFn; label = label}

let (<|>) = orElse

let choice listOfParsers =
    List.reduce (<|>) listOfParsers

let satisfy predicate label =
    let innerFn input =
        let remainingInput, charOpt = TextInput.nextChar input
        match charOpt with
        |None ->
            let err = "No more input"
            let pos = parserPositionFromInputState input
            Failure (label, err, pos)
        | Some first ->
            if predicate first then
                Success (first, remainingInput)
            else
                let err = sprintf "Unexpected '%c'" first
                let pos = parserPositionFromInputState input
                Failure (label, err, pos)
    {parseFn = innerFn; label = label}

let pchar charToMatch =
    let predicate ch = (ch = charToMatch)
    let label = sprintf "%c" charToMatch
    satisfy predicate label

let digitChar =
    let predicate = Char.IsDigit
    let label = "digit"
    satisfy predicate label

let whitespaceChar =
    let predicate = Char.IsWhiteSpace
    let label = "whitesapce"
    satisfy predicate label

let anyOf listOfChars =
    let label = sprintf "any of %A" listOfChars
    listOfChars
    |> List.map pchar
    |> choice
    <?> label

let mapP f =
    bindP (f >> returnP)

let (<!>) = mapP

let (|>>) x f = mapP f x

let applyP fP xP =
    fP >>= (fun f ->
    xP >>= (fun x ->
        returnP (f x)))

let (<*>) = applyP

let lift2 f xP yP =
    returnP f <*> xP <*> yP

let rec sequence parserList =
    let cons head tail = head :: tail
    let consP = lift2 cons
    match parserList with
    | [] -> 
        returnP []
    | head :: tail ->
        consP head (sequence tail)

let charListToString charList =
    String(List.toArray charList)

let pstring str =
    let label = str
    str
    |> List.ofSeq
    |> List.map pchar
    |> sequence
    |> mapP charListToString
    <?> label

let rec parseZeroOrMore parser input =
    let firstResult = runOnInput parser input
    match firstResult with
    | Failure _ ->
        ([], input)
    | Success (firstValue, inputAfterFirstParse) ->
        let (subsequentValues, remainingInput) =
            parseZeroOrMore parser inputAfterFirstParse
        let values = firstValue :: subsequentValues
        (values, remainingInput)

let many parser =
    let label = sprintf "many %s" (getLabel parser)
    let innerFn input =
        Success (parseZeroOrMore parser input)
    {parseFn = innerFn; label = label}

let many1 parser =
    parser >>= (fun head ->
    many parser >>= (fun tail ->
        returnP (head :: tail)))

let manyChars cp =
    many cp
    |>> charListToString

let manyChars1 cp =
    many1 cp
    |>> charListToString

let spaces = many whitespaceChar

let spaces1 = many1 whitespaceChar

let opt p =
    let some = p |>> Some
    let none = returnP None
    some <|> none

let pint =
    let label = "integer"
    let resultToInt (sign, digits) =
        let i = digits |> int
        match sign with
        | Some ch -> -i
        | None -> i
    let digits = manyChars1 digitChar
    opt (pchar '-') .>>. digits
    |>> resultToInt
    <?> label

let pfloat =
    let label = "float"
    let resultToFloat (((sign, digits1), point), digits2) =
        let fl = sprintf "%s.%s" digits1 digits2 |> float
        match sign with
        | Some ch -> -fl
        | None -> fl
    let digits = manyChars1 digitChar
    opt (pchar '-') .>>. digits .>>. pchar '.' .>>. digits
    |> mapP resultToFloat
    <?> label

let (.>>) p1 p2 =
    p1 .>>. p2
    |> mapP (fun (a, _) -> a)

let (>>.) p1 p2 =
    p1 .>>. p2
    |> mapP (fun (_, b) -> b)

let between p1 p2 p3 =
    p1 >>. p2 .>> p3

let sepBy1 p sep =
    let sepThenP = sep >>. p
    p .>>. many sepThenP
    |>> fun (p, pList) -> p :: pList

let sepBy p sep =
    sepBy1 p sep <|> returnP []
