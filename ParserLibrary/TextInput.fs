module TextInput

open System

type Position = {
            line: int
            column: int
        }

let initialPos = {line = 0; column = 0}

let incrCol pos =
    {pos with column = pos.column + 1}

let incrLine pos =
    {line = pos.line + 1; column = 0}

type InputState = {
    lines: string[]
    position: Position
}

let fromStr str =
    if String.IsNullOrEmpty str then
        {lines = [||]; position = initialPos}
    else
        let separators = [| "\r\n"; "\n"|]
        let lines = str.Split(separators, StringSplitOptions.None)
        {lines = lines; position = initialPos}

let currentLine inputState =
    let linePos = inputState.position.line
    if linePos < inputState.lines.Length then
        inputState.lines.[linePos]
    else
        "end of file"

let nextChar input =
    let linePos = input.position.line
    let colPos = input.position.column
    if linePos >= input.lines.Length then
        input, None
        else
            let currentLine = currentLine input
            if colPos < currentLine.Length then
                let char = currentLine.[colPos]
                let newPos = incrCol input.position
                let newState = {input with position = newPos}
                newState, Some char
            else
                let char = '\n'
                let newPos = incrLine input.position
                let newState = {input with position = newPos}
                newState, Some char
