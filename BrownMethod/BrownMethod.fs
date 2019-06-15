namespace Brown

module private Internal =
    let addList l1 l2 = 
        List.zip l1 l2
        |> List.map (fun (e1, e2) -> e1 + e2)

module BrownMethod =
    open Internal

    type Iter = { 
            K: int
            i: int
            j: int
            A: int list
            B: int list
            Ak: double
            Bk: double
            Vk: double
            BMin: int
            AMax: int
        }

    type BrownResult = {
        SedlT: bool
        A: int
        B: int

        P: double list
        Q: double list

        V: double

        Iters: Iter list
    }

    let braun mtx iterCount =
        let mtx = 
            mtx
            |> List.ofArray
            |> List.map List.ofArray

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
    
            let currentIter = {
                K = iter.K + 1;
                i = startI + 1;
                j = bMinAt + 1
                A = a;
                B = b;
                Ak = aK;
                Bk = bK;
                Vk =  vK;
                BMin = bMinAt;
                AMax = aMaxAt;
            }
    
            seq {
                yield currentIter
                yield! braunInternal mtx tr aMaxAt currentIter
            }

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

        let tr = transpose mtx
    
        let alpha = maxMin mtx
        let beta = minMax tr

        match snd alpha = snd beta with
        | true -> 
            {
                SedlT = true;
                A = snd alpha;
                B = snd beta;
                P = [];
                Q = [];
                Iters = [];
                V = 0.0;
            }
        | false -> 
            let startIter = {
                K = 0;
                i = 0;
                j = 0;
                A = List.replicate ( List.length mtx ) 0;
                B = List.replicate ( List.length tr ) 0;
                Ak = 0.0;
                Bk = 0.0;
                Vk =  0.0;
                BMin = 0;
                AMax = 0;
            }
        
            let iters = braunInternal mtx tr ( fst alpha ) startIter |> Seq.take iterCount |> Seq.toList

            let rows = mtx.Length
            let cols = tr.Length

            let getSolution dim mapFun =
                let starts = List.map mapFun iters
                [1..dim]
                |> List.map (fun x -> starts |> List.filter ((=) x) |> List.length )
                |> List.map (fun x -> double x / double iterCount)

            {
                SedlT = false;
                A = snd alpha;
                B = snd beta;
                P = getSolution rows (fun x -> x.i);
                Q = getSolution cols (fun x -> x.j);
                Iters = iters;
                V = List.last iters |> (fun x -> x.Vk)
            }

    