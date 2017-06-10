module Program

open ParserLibrary
open FsJsonParser

let example1 = """{
    "name" : "Scott",
    "isMale" : true,
    "bday" : {"year" : 2001, "month" : 12, "day" : 25},
    "favouriteColors" : ["blue", "green"]
}"""

let example2= """{"widget": {
    "debug": "on",
    "window": {
        "title": "Sample Konfabulator Widget",
        "name": "main_window",
        "width": 500,
        "height": 500
    },
    "image": { 
        "src": "Images/Sun.png",
        "name": "sun1",
        "hOffset": 250,
        "vOffset": 250,
        "alignment": "center"
    },
    "text": {
        "data": "Click Here",
        "size": 36,
        "style": "bold",
        "name": "text1",
        "hOffset": 250,
        "vOffset": 100,
        "alignment": "center",
        "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
    }
}}  """


[<EntryPoint>]
let main argv = 
    let res1 = run jValue example1
    let res2 = run jValue example2
    printfn "%A" res1
    printfn "%A" res2
    0 // 返回整数退出代码
