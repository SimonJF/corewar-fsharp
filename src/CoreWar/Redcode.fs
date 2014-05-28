module Redcode

// Define the core instruction set
type Instr = 
    | DAT // Kill the process 
    | MOV // Copy data from one address to another
    | ADD // Add one number to another
    | SUB // Subtract one number from another
    | MUL // Multiply
    | DIV // Divide: if /0, kill process
    | MOD // Modulus
    | JMP // Unconditional jump
    | JMZ // Tests a number, jumps if 0
    | JMN // Tests, jumps if *not* 0
    | DJN // Decrement and jump if not 0
    | SPL // Split -- start a second process at another address
    | CMP // Compare two instructions, skip the next if they're equal
    | SEQ // As above
    | SNE // Skip if *not* equal
    | SLT // Compares two values, and skips next if first lower than second
    | LDP // Load from private storage space
    | STP // Save to private storage space
    | NOP // Do nothing
    ;;

type AddressingMode =
    | Direct     // Addr relative to current PC
    | Indirect   // B-field indirect: loc = (PC + val) + B-field of this instr
    | Immediate  // Absolute value

type LocationField = { AddrMode : AddressingMode; Value : int };;

type Command = { Instruction : Instr; Loc1 : LocationField; Loc2 : LocationField };;

let showLocation loc = 
    let symb = 
        match loc.AddrMode with
              Direct -> ""
            | Indirect -> "@"
            | Immediate -> "#"
    symb + loc.Value.ToString()

let showCommand cmd = 
    sprintf "%A %s %s" cmd.Instruction (showLocation cmd.Loc1) (showLocation cmd.Loc2)

let printProgram prog = 
    List.map(fun cmd -> printfn "%s" (showCommand cmd)) prog |> ignore

let showProgram prog = 
    Array.fold (fun acc instr -> acc + (showCommand instr) + "\n")
        "" prog