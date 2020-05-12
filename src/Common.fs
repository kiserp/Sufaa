module Common
open System.Net

    type Role = 
        | Server = 0
        | Client = 1

    type MessageType = 
        | Unknown = 0b0000uy
        | Ping = 0b0001uy
        | Pong = 0b10001uy
        | UserLogin = 0b10010uy
        | UserList = 0b100010uy
        | UserLogoff = 0b0010uy
        | UserMessage = 0b0100uy
        
    let AsMessageType byteVal =
        match byteVal with
        | 0b0001uy -> MessageType.Ping
        | 0b10001uy -> MessageType.Pong
        | 0b10010uy -> MessageType.UserLogin
        | 0b100010uy -> MessageType.UserList
        | 0b0010uy -> MessageType.UserLogoff
        | 0b0100uy -> MessageType.UserMessage
        | _        -> MessageType.Unknown
    
    let delimeter = 0b10011001uy
    
    let rec GetPartition (source: byte []) out idx =
        match source, out with
        | src, _ when idx >= src.Length -> out, (idx+1)
        | src, [||] when src.[idx] = delimeter -> GetPartition source out (idx + 1)
        | src, _ when src.[idx] = delimeter -> out, (idx+1)
        | _, _ -> GetPartition source  ( Array.append out [|source.[idx]|] ) (idx + 1)

    let rec SplitByDelimeter (source: byte []) out idx partLimit =
        match partLimit with 
        | 0 -> out, idx
        | _ -> 
            let part, newIdx = GetPartition source [||] idx 
            match part with
            | [||] -> out, newIdx
            | _ -> SplitByDelimeter source (Array.append out [|part|]) newIdx (partLimit - 1)

    let IPEndpointToBytes (endpoint: IPEndPoint): byte [] = 
        let addrBytes = endpoint.Address.MapToIPv4().GetAddressBytes()
        let portBytes = System.BitConverter.GetBytes(endpoint.Port)
        [| addrBytes
           [|delimeter|]
           portBytes
        |] |> Array.concat

    let IPEndpointFromBytes (bytes: byte []) idx = 
        let parts = SplitByDelimeter bytes [||] idx 2
        let addr = IPAddress(((fst parts).[0]))
        let port = System.BitConverter.ToInt32((fst parts).[1],0)
        IPEndPoint(addr,port), snd parts
    
    let rec GetFirstWord (text: char list) (outWord: string) = 
        match text with
        | h::t when h = ' ' -> outWord
        | [] -> outWord
        | h::t -> GetFirstWord t (sprintf "%s%c" outWord  h)