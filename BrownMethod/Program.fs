open System

type Iter = { 
        K: int
        i: int
        j: int
        A: int list
        B: int list
        Ak: double
        Bk: double
        Vk: double
    }

let addList l1 l2 = 
    List.zip l1 l2
    |> List.map (fun (e1, e2) -> e1 + e2)

let braun mtx tr i =

    let rec braunInternal mtx tr strat iter =
        
        let startI = strat
        let k = iter.K + 1
    
        let b = addList iter.B ( mtx |> List.item startI )
        let bMin = List.min b
        let bMinAt = b |> List.findIndex ((=) bMin)
        let aK = double bMin / double k
    
        let a = addList iter.A ( tr |> List.item bMinAt )
        let aMax = List.max a
        let aMaxAt = a |> List.findIndex ((=) aMax)
        let bK = double aMax / double k
        
        let vK = (aK + bK) / 2.0
    
        let qqq = {
            K = iter.K + 1;
            i = startI;
            j = bMinAt
            A = a;
            B = b;
            Ak = aK;
            Bk = bK;
            Vk =  vK;
        }
    
        seq {
            yield qqq
            yield! braunInternal mtx tr aMaxAt qqq
        }

    let startIter = {
        K = 0;
        i = 0;
        j = 0;
        A = List.replicate ( List.length mtx ) 0;
        B = List.replicate ( List.length tr ) 0;
        Ak = 0.0;
        Bk = 0.0;
        Vk =  0.0;
    }

    braunInternal mtx tr i startIter

let printIters list =
    for element in list do
        printfn "%A" element
        printfn "%s" (new String(Array.replicate 30 '-'))

[<EntryPoint>]
let main args =
    
    let minMaxGeneric first second m =
        m
        |> List.zip [0..m.Length-1]
        |> List.map (fun (x, a) -> x, first a )
        |> second snd

    let minMax = minMaxGeneric List.max List.minBy
    let maxMin = minMaxGeneric List.min List.maxBy
    
    let rec transpose = function
    | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []
            
    let mtx = [
        [2; 0; 3]
        [1; 3; -3]
    ]

    let tr = transpose mtx

    let alpha = maxMin mtx
    let beta = minMax tr

    match alpha = beta with
    | true -> 
        printfn "Игра имеет решение в чистых стратегиях"
    | false -> 
        let result = braun mtx tr (fst alpha)

        let someIters = result |> Seq.take 10 |> Seq.toList
        printIters someIters

        ()

    Console.ReadKey() |> ignore

    0