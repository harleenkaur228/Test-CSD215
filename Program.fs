let f = fun slry -> slry > 100000

let slrs = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

let HighInSlrs = slrs |> List.filter f

HighInSlrs |> List.iter (printfn "%d")





let slrs = [75000.0; 48000.0; 120000.0; 190000.0; 300113.0; 92000.0; 36000.0]

let Taxslry inc =
    if inc <= 49020.0 then inc * 0.15
    elif inc <= 98040.0 then inc * 0.205
    elif inc <= 151978.0 then inc * 0.26
    elif inc <= 216511.0 then inc * 0.29
    else inc * 0.33

let taxes = List.map Taxslry slrs

let Incaftertax =
    List.map2 (fun income tax -> income - tax) slrs taxes

printfn "Actual Slrs: %A" slrs
printfn "clculating Taxes: %A" taxes
printfn "Income after adding taxes: %A" Incaftertax





let slrs = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

let NewSalaries slrs =
    slrs
    |> List.filter (fun salary -> salary < 49020)
    |> List.map (fun salary -> salary + 20000)

printfn "Actual Salaries: %A" slrs
printfn "New Salaries: %A" (NewSalaries slrs)







let slrs = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

let fltrdslrs =
    slrs
    |> List.filter (fun slr -> slr >= 50000 && slr <= 100000)

let addingfltrdslrs =
    fltrdslrs
    |> List.fold (fun acc slr -> acc + slr) 0

printfn "Salaries filtered: %A" fltrdslrs
printfn "Adding fltrd Slrs: %d" addingfltrdslrs