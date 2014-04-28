// Дополнительные сведения о F# см. на http://fsharp.net

#load @"C:\Users\Zoth\Desktop\.F#\FSharpChart-0.2\FSharpChart.fsx"
#load @"C:\Users\Zoth\Desktop\.F#\FSharpChart-0.2\FSharpChartAutoDisplay.fsx"

open Samples.Charting

let M list n = 
    let move (a, b) = function
        | 0 -> (a + 1, b)
        | 1 -> (a, b + 1)
        | 2 -> (a - 1, b)
        | 3 -> (a, b - 1)
        | _ -> (a, b)
    match list with
    | [] -> [move (0, 0) n]
    | h::t -> (move h n)::list

let (^) a b = 
    let rec toBits i = 
        if (i > 0) then (i%2)::(toBits (i/2))
        else []
    let aList = toBits a
    let bList = toBits b
    let xorLists list1 list2 = 
        let rec makeOneLength list1 list2 = 
            if ((List.length list1) > (List.length list2)) then (makeOneLength list1 (List.append list2 [0]))
            else if ((List.length list1) < (List.length list2)) then (makeOneLength (List.append list1 [0]) list2)
            else (list1, list2)
        let lists = makeOneLength list1 list2
        List.map2 (fun i j -> i<>j) (fst lists) (snd lists)
    let resList = xorLists aList bList
    let fromBits list = 
        let listToSum = (List.mapi (fun index b -> 
            let doIt i = System.Math.Pow(2.0, (float)index)*i
            if (b) then doIt 1.0
            else doIt 0.0) list);
        List.fold (fun sum i -> 
            sum + i) 0.0 listToSum
    (int)(fromBits resList)

let rec LP list N k = 
    if (N > 1) then
        let l1 = LP list (N-1) (k^1)
        let l2 = M l1 k
        let l3 = LP l2 (N-1) k
        let l4 = M l3 (k^1)
        let l5 = LP l4 (N-1) k
        let l6 = M l5 (k^2)
        let l7 = LP l6 (N-1) (k^3)
        l7
    else list

let P N = 
    let l1 = [(0, 0)]
    let l2 = LP l1 (N-1) 1
    let l3 = M l2 0
    let l4 = LP l3 (N-1) 1
    let l5 = M l4 3
    let l6 = LP l5 (N-1) 3
    let l7 = M l6 2
    let l8 = LP l7 (N-1) 3
    let l9 = M l8 1
    FSharpChart.FastLine l9

let res = P 9
