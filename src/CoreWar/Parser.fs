module Parser

open FParsec
open Redcode
open VM

// Workaround for the generic weirdness
type Parser<'t> = Parser<'t, unit>

let (parseInstruction : Parser<Instr>) = 
        (stringReturn "ADD" ADD)
    <|> (stringReturn "SUB" SUB)
    <|> (stringReturn "MUL" MUL)
    <|> (stringReturn "DIV" DIV)
    <|> (stringReturn "MOD" MOD)
    <|> (stringReturn "JMP" JMP)
    <|> (stringReturn "JMZ" JMZ)
    <|> (stringReturn "JMN" JMN)
    <|> (stringReturn "DJN" DJN)
    <|> (stringReturn "SPL" SPL)
    <|> (stringReturn "CMP" CMP)
    <|> (stringReturn "SEQ" SEQ)
    <|> (stringReturn "SNE" SNE)
    <|> (stringReturn "SLT" SLT)
    <|> (stringReturn "LDP" LDP)
    <|> (stringReturn "STP" STP)
    <|> (stringReturn "NOP" NOP)
    <|> (stringReturn "MOV" MOV)
    <|> (stringReturn "DAT" DAT)

let (parseLocation : Parser<LocationField>) =
    spaces >>.
    ((anyOf "#@" >>= function
          '#' -> pint32 |>> (fun x -> {AddrMode = Immediate ; Value = x} )
        | '@' -> pint32 |>> (fun x -> {AddrMode = Indirect ; Value = x} )  
     ) <|> (pint32 |>> (fun x -> {AddrMode = Direct ; Value = x} )))

let (parseCommand : Parser<Command>) =
    pipe3 parseInstruction parseLocation parseLocation 
        (fun instr loc1 loc2 -> {Instruction = instr ; Loc1 = loc1 ; 
                                 Loc2 = loc2} )

let parseWarrior lines =
    let name = List.head lines
    let instrs = List.tail lines
    // Parse each instruction individually
    let map_fn x = 
        printfn "Parsing %s" x
        match run parseCommand x with
            |  Success(result, _, _) -> result
            | Failure(err, _, _) -> failwith err
    let parsed_instrs = List.map map_fn instrs
    { Name = name ; Code = parsed_instrs }