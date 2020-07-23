module TimeConverter

open System
open CronParser

let findNextTime target prev = function
    | curr when curr < target && target <= curr -> curr
    | curr when curr < target && target <= prev -> prev
    | curr -> min prev curr

let findNextTimeCandidates target = function
    | Any -> target
    | Time(t) -> t
    | TimeRange(left, right) when left <= target && target <= right -> target
    | TimeRange(left, right) -> min left right

let rec getNextTime timeFields target =
    timeFields 
    |> List.map (findNextTimeCandidates target)
    |> List.reduce (findNextTime target)

let adjustSecond (curr: DateTime) =
    curr.AddSeconds(-(float)curr.Second)

let adjustMinute next (curr: DateTime) = 
    match (getNextTime next curr.Minute) - curr.Minute with
    | diff when diff >= 0 -> curr.AddMinutes((float)diff)
    | diff -> curr.AddMinutes((float)(diff + 60))

let adjustHour next (curr: DateTime) = 
    match (getNextTime next curr.Hour) - curr.Hour with
    | diff when diff >= 0 -> curr.AddHours((float)diff)
    | diff -> curr.AddHours((float)(diff + 24))

//TODO add adjustDOM
let adjustDOM next (curr: DateTime) = curr

let adjustMonth next (curr: DateTime) = 
    match (getNextTime next curr.Month) - curr.Month with
    | diff when diff >= 0 -> curr.AddMonths(diff)
    | diff -> curr.AddMonths(diff + 12)

let adjustDOW next (curr: DateTime) = 
    match (getNextTime next ((int)curr.DayOfWeek)) - (int)curr.DayOfWeek with
    | diff when diff >= 0 -> curr.AddDays((float)diff)
    | diff -> curr.AddDays((float)(diff + 7))

let adjustDT (localDT: DateTime) remoteTZ schedule =
    let min, hour, dom, month, dow = schedule
    System.TimeZoneInfo.ConvertTime(localDT, remoteTZ)
    |> adjustSecond
    |> adjustMinute min
    |> adjustHour hour
    |> adjustDOM dom
    |> adjustMonth month
    |> adjustDOW dow