module CronParser

open System
open FParsec

type Time = int32
type TimeField = 
    | Time of Time
    | TimeRange of Time * Time
    | Any

type BashCommand = String
type Schedule = TimeField list * TimeField list * TimeField list * TimeField list * TimeField list
type CronLine = 
    | Comment of String
    | CronExpression of Schedule * BashCommand
    | TimeZone of TimeZoneInfo

//https://stackoverflow.com/questions/9159554/parsing-numbers-in-fparsec
let mayThrow (p: Parser<'t,'u>) : Parser<'t,'u> =
    fun stream ->
        let state = stream.State        
        try 
            p stream
        with e ->
            stream.BacktrackTo(state)
            Reply(FatalError, messageError e.Message)

//TODO fix error handling
let parse content =
    let pComment = skipChar '#' >>. restOfLine true |>> Comment
    let pTZInfo (tzStr: String) = TimeZone(TimeZoneConverter.TZConvert.GetTimeZoneInfo(tzStr.Trim()))
    let pTimeInRange (rlow, rhigh) = function
        | num when rlow <= num && num <= rhigh -> Time(num)
        | _ -> failwithf "value must be in range [%d-%d]" rlow rhigh
    let pTimeRangeInRange (rlow, rhigh) = function
        | (num1, num2) when num1 <= num2 && rlow <= num1 && num1 <= rhigh && rlow <= num2 && num2 <= rhigh -> TimeRange(num1, num2)
        | (num1, num2) when num1 > num2 -> failwithf "[x-y] x must be less than y"
        | _ -> failwithf "values must be in range [%d-%d]" rlow rhigh

    let pTime range = pint32 |>> pTimeInRange range
    let pTimeRange range = pint32 .>>. (skipChar '-' >>. pint32) |>> pTimeRangeInRange range
    let pAny = skipChar '*' >>% Any
    let pTimeList range = sepBy1 (attempt (pTimeRange range) <|> attempt (pTime range) <|> attempt pAny) (skipChar ',')

    let pMinute = spaces >>. pTimeList (0, 59)
    let pHour = spaces >>. pTimeList (0, 23)
    let pDOM = spaces >>. pTimeList (1, 31)
    let pMonth = spaces >>. pTimeList (1, 12)
    let pDay = spaces >>. pTimeList (0, 6)

    let pSchedule = tuple5 pMinute pHour pDOM pMonth pDay |>> Schedule
    let pCommand = spaces >>. restOfLine true
    let pCronExpression = pSchedule .>>. pCommand |>> CronExpression
    let pTZLine = skipString "CRON_TZ=" >>. restOfLine true .>> spaces |>> pTZInfo

    let pCronLines = many (spaces >>. (mayThrow pCronExpression <|> attempt pTZLine <|> attempt pComment))
    run (pCronLines .>> spaces .>> eof) content
