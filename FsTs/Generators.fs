namespace FsTs

open System

module Generators =

    let rec dateRange (fromDate:DateTime) (toDate:DateTime) (by:TimeSpan) = 
        seq {
            if fromDate <= toDate then 
                yield fromDate
                yield! dateRange (fromDate.Add(by)) toDate by
        }