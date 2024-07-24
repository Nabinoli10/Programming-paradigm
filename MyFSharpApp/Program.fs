let rec printSalaries salaries =
    match salaries with
    | [] -> ()
    | head :: tail ->
        printfn "%d" head
        printSalaries tail

let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

printfn "All Salaries:"
printSalaries salaries

let highIncomeSalaries = List.filter (fun salary -> salary > 100000) salaries

printfn "\nHigh-Income Salaries:"
printSalaries highIncomeSalaries

let calculateTax salary =
    match salary with
    | s when s <= 49020 -> float s * 0.15
    | s when s <= 98040 -> float s * 0.205
    | s when s <= 151978 -> float s * 0.26
    | s when s <= 216511 -> float s * 0.29
    | s -> float s * 0.33

let taxes = List.map calculateTax salaries

let rec printTaxes taxes =
    match taxes with
    | [] -> ()
    | head :: tail ->
        printfn "%.2f" head
        printTaxes tail

printfn "\nTaxes for Salaries:"
printTaxes taxes


let updatedSalaries = 
    salaries 
    |> List.filter (fun salary -> salary < 49020) 
    |> List.map (fun salary -> salary + 20000)

printfn "\nUpdated Salaries (less than $49,020 increased by $20,000):"
printSalaries updatedSalaries


let sumInRange =
    salaries
    |> List.filter (fun salary -> salary >= 50000 && salary <= 100000)
    |> List.fold (+) 0

printfn "\nSum of Salaries between $50,000 and $100,000: %d" sumInRange


let sumMultiplesOfThree (n: int) =
    let rec helper (current: int) (acc: int) =
        if current < 3 then
            acc
        else
            helper (current - 3) (acc + current)
    
    helper n 0

let result = sumMultiplesOfThree 33
printfn "The sum of all multiples of 3 up to 33 is %d" result
