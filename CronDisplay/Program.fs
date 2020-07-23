open System
open ArgParser
open CronParser
open TimeConverter
open FParsec.CharParsers

type TZAbstract = TimeZoneInfo * Schedule
type CronAbstract = TZAbstract * BashCommand

type TZConcrete = TimeZoneInfo * DateTime
type CronConcrete = TZConcrete * BashCommand

let cronLineToCronAbstract defaultTZ lines = 
    let rec aux cronLines tz acc = 
        match cronLines with
        | [] -> acc
        | line::remainingLines -> 
            match line with
            | Comment(_) -> aux remainingLines tz acc
            | CronExpression(schedule, command) -> aux remainingLines tz (acc @ [CronAbstract(TZAbstract(tz, schedule), command)])
            | TimeZone(tz) -> aux remainingLines tz acc
    aux lines defaultTZ []

let abstractToConcrete ((tz, schedule): TZAbstract) =
    TZConcrete(tz, adjustDT DateTime.Now tz schedule)

let concreteToNewTZ newTZ ((oldTZ, oldDT): TZConcrete) =
    TZConcrete(newTZ, TimeZoneInfo.ConvertTime(oldDT, oldTZ, newTZ))

let cronAbstractToConcrete ((tzAbstract, command): CronAbstract) =
    CronConcrete(tzAbstract |> abstractToConcrete, command)

let cronConcreteToNewTZ newTZ ((tzConcrete, command): CronConcrete) =
    CronConcrete(tzConcrete |> concreteToNewTZ newTZ, command)

let cronConcreteToString (((tz, dt): TZConcrete, command): CronConcrete) =
    sprintf "Timezone: %A\nDateTime: %A\n Command: %A\n" tz dt command

let cronLinesToConcreteString cronLines defaultTZ targetTZ  =
    let cronConcreteToTargetTZ = cronConcreteToNewTZ targetTZ
    cronLineToCronAbstract defaultTZ cronLines
    |> List.map cronAbstractToConcrete
    |> List.map cronConcreteToTargetTZ
    |> List.map cronConcreteToString
    |> String.concat "\n"

let getResult (defaultTZ, targetTZ, content) =
    match parse content with
    | Success(cronLines, _, _) -> Ok (cronLinesToConcreteString cronLines defaultTZ targetTZ)
    | Failure(errorMsg, _, _) -> Error errorMsg

[<EntryPoint>]
let main argv =
    match parseArgs argv >>= validateArgs >>= getResult with
    | Ok(result) -> 
        printfn "%A" result
        0
    | Error(err) -> 
        printfn "%A" err
        -1
