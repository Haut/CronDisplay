module ArgParser

open Argu

type CliArguments =
    | [<AltCommandLine("-d")>] Default of string
    | [<AltCommandLine("-t")>] Target of string
    | [<MainCommand; ExactlyOnce; Last>] File of string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Default _ -> "default timezone of the cron"
            | Target _ -> "target timezone to convert to"
            | File _ -> "cron text file to parse"

let parseArgs argv =
    let parser = ArgumentParser.Create<CliArguments>()
    try
        let arguments = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        let defaultTZName = arguments.GetResult (Default, defaultValue = "UTC")
        let targetTZName = arguments.GetResult (Target, defaultValue = "UTC")
        let fileName = arguments.GetResult File
        Ok(defaultTZName, targetTZName, fileName)
    with | ex -> Error(parser.PrintUsage())

let readFileToString fileName = 
    try Ok(System.IO.File.ReadAllText fileName)
    with | ex -> Error(sprintf "Could not read file: %s" fileName)

let readTimeZone tzStr =
    try Ok(TimeZoneConverter.TZConvert.GetTimeZoneInfo(tzStr))
    with | ex -> Error(sprintf "Could not read time zone: %s" tzStr)

let validateArgs (defaultTZName, targetTZName, fileName) =
    match (readTimeZone defaultTZName, readTimeZone targetTZName, readFileToString fileName) with
    | Ok(defaultTZ), Ok(targetTZ), Ok(content) -> Ok(defaultTZ, targetTZ, content)
    | Error(e), _, _ -> Error(e)
    | _, Error(e), _ -> Error(e)
    | _, _, Error(e) -> Error(e)

//https://fsharpforfunandprofit.com/rop/
let (>>=) result f =
    match result with
    | Ok(value) -> f value
    | Error(e) -> Error(e)