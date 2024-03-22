// For more information see https://aka.ms/fsharp-console-apps
printfn "Sarabjeet Singh"
// List of salaries
let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

// Display list of salaries
printfn "List of salaries: %A" salaries

// Filter higher wages (> $100,000)
let highSalaries = salaries |> List.filter (fun salary -> salary > 100000)
printfn "Higher salaries: %A" highSalaries

// Tax calculation function
let calculateTax salary =
    match salary with
    | s when s <= 49020 -> float s * 0.15
    | s when s <= 98040 -> float 49020 * 0.15 + float (s - 49020) * 0.205
    | s when s <= 151978 -> float 49020 * 0.15 + float (98040 - 49020) * 0.205 + float (s - 98040) * 0.26
    | s when s <= 216511 -> float 49020 * 0.15 + float (98040 - 49020) * 0.205 + float (151978 - 98040) * 0.26 + float (s - 151978) * 0.29
    | _ -> float 49020 * 0.15 + float (98040 - 49020) * 0.205 + float (151978 - 98040) * 0.26 + float (216511 - 151978) * 0.29 + float (salary - 216511) * 0.33

let taxes = salaries |> List.map calculateTax
printfn "Taxes for all salaries: %A" taxes

// Adjust earnings less than $49,020 by adding $20,000
let adjustedSalaries = salaries |> List.map (fun salary -> if salary < 49020 then salary + 20000 else salary)
printfn "Adjusted salaries: %A" adjustedSalaries

// Filter salaries between $50,000 and $100,000 and sum them
let sum = salaries |> List.filter (fun salary -> salary >= 50000 && salary <= 100000) |> List.sum
printfn "Sum of salaries between $50,000 and $100,000: %d" sum

// Tail recursion to calculate the sum of all multiples of 3 up to a given number
let rec sumMultiplesOf3 n total =
    match n with
    | 0 -> total
    | _ -> sumMultiplesOf3 (n - 3) (total + n)

printfn "Sum of multiples of 3 up to 27: %d" (sumMultiplesOf3 27 0)







