module Program = 

    open System    
    open System.Text.RegularExpressions
   

    [<Literal>]
    let CustomDelimPrefix = "//"
    [<Literal>]
    let CustomDelimSuffix = "\n"
    [<Literal>]
    let CustomMultiDelimPattern = "\[{1}[^\]\[]*\]{1}"


    let substitute_zerostr_for_empty (str : string) = 
        match String.IsNullOrWhiteSpace str with 
        | (true) -> "0"
        | _ -> str


    let get_single_char_delims (str : string) = 
        match str.StartsWith CustomDelimPrefix with 
            | (true) -> (seq [ str.Substring (2,1) ], 2)    // custom single char delim
            | _ -> (seq [ "," ; "\n" ], 0)                  // use default delims


    let get_multi_char_delims (str : string) =         
        let regex = new Regex(CustomMultiDelimPattern)
        let delims = regex.Matches(str) |> Seq.cast<Match> |> Seq.map (fun m -> m.Value.TrimStart '[') |> Seq.map (fun s -> s.TrimEnd ']')
        let delimLength = (CustomDelimPrefix.Length + (delims |> Seq.map (fun d -> 2 + d.Length) |> Seq.sum) + CustomDelimSuffix.Length) // 2x char for [] foreach delim
        (delims, delimLength)


    let get_numstr_parts (str : string) = 
        let delims, delimsLength = 
            match str.StartsWith "//[" with
            | (true) -> get_multi_char_delims str 
            | _ -> get_single_char_delims str

        let num_data = 
            match delimsLength > 0 with 
            | (true) -> str.Substring delimsLength
            | _ -> str

        num_data.Split (Seq.toArray delims, StringSplitOptions.RemoveEmptyEntries)


    let itoa (i : int) = i.ToString()
        
    let format_negatives_not_allowed_error_message less_than_zero_nums = 
        match Seq.length less_than_zero_nums > 0 with
        | (true) -> less_than_zero_nums |> Seq.map itoa |> Seq.fold (fun a b -> a + b + ",") "" |> (fun s -> s.TrimEnd ',')
        | _ -> ""


    let validate_nums int_nums =
        let numsToConsider = int_nums |> Seq.filter (fun n -> n < 1001) // gt 1000 ignored
        let numsLtZero = numsToConsider |> Seq.filter(fun n -> n < 0)         // lt zero is invalid
        match Seq.length numsLtZero > 0 with
        | (true) -> failwith ("negatives not allowed: " + format_negatives_not_allowed_error_message numsLtZero)
        | _ -> numsToConsider                 


    let calc input = 
        substitute_zerostr_for_empty input
        |> get_numstr_parts 
        |> Seq.map Int32.Parse
        |> validate_nums
        |> Seq.sum



    [<EntryPoint>]
    let main argv = 
        printfn "Hello world"
        System.Console.ReadKey() |> ignore
        0
