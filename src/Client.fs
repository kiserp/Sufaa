module Client
open Message
open System
open Person
open Server
//need Message queue, but not now

let PrintMessage message = 
    printfn "---- [ %s ] ------------------------------------------------" 
        message.Author.Value.Nickname
    printfn "|" 
    printfn "|>\t%s" message.Text
    printfn "|"
    printfn "---- [ %s ] ------------------------------------------------"
        (message.TimeStamp.ToLongTimeString())

let SendLogin
let Login () = 
    printf "Please, enter your name:"
    let nickname = Console.ReadLine()
    let user = {Nickname = nickname; LoggedAt = DateTime.Now; Connection = GetLocalEndpoint()}
    printfn "Nice to meet you, %s. You are logged at %s " user.Nickname (user.LoggedAt.ToLongTimeString())


let HandleInput (line: string) =
    let fstWord = line.Trim().Split(" ") |> Array.exactlyOne
    match fstWord with 
    |":help" -> ShowHelp()
    |":login" -> Login()
    |":logoff" -> LogOff()
    |":users" -> GetUsers()
    | _ -> NewMessage line