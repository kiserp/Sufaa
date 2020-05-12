module Person

open System
open System.Net.Sockets
open Common

    type Person = {Nickname: string; LoggedAt: DateTime; Connection: System.Net.IPEndPoint; }

    let mutable private localPerson = 
        {Nickname = ""
         LoggedAt = DateTime.MinValue
         Connection = new Net.IPEndPoint(0L,0)}

    let SetLocalPerson user = 
        localPerson <- user
    let GetLocalPerson () =
        localPerson

    let GetBytes person : byte [] = 
        let byteDilimeterArr = [|delimeter|]
        let nicknameBytes = System.Text.Encoding.ASCII.GetBytes(person.Nickname)
        let loggedBytes = System.BitConverter.GetBytes(person.LoggedAt.ToBinary())
        let cnBytes = person.Connection |> IPEndpointToBytes
        [|nicknameBytes
          byteDilimeterArr   
          loggedBytes
          byteDilimeterArr
          cnBytes|] 
        |> Array.concat

    let FromBytes idx bytes : Person * Int32= 
        let partiotions = SplitByDelimeter bytes [||] idx 3
        let nickname = (fst partiotions).[0] |> System.Text.Encoding.ASCII.GetString
        let logged = System.BitConverter.ToInt64((fst partiotions).[1],0) |> System.DateTime.FromBinary 
        let cn, index = IPEndpointFromBytes bytes (snd partiotions) 
        {Nickname = nickname
         LoggedAt = logged
         Connection = cn}, index
    
    let FromBytesStartFromBegining = 
        FromBytes 0
    
    