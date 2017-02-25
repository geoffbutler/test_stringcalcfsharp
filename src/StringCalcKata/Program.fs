module Program = 

    open System    
    open System.Text.RegularExpressions
   

    [<Literal>]
    let EmptyString = ""
    [<Literal>]
    let CustomDelimPrefix = "//"
    [<Literal>]
    let CustomDelimSuffix = "\n"
    [<Literal>]
    let CustomMultiDelimPattern = "\[{1}[^\]\[]*\]{1}"


    let flip f a b = f b a
    let itoa (i : int) = i.ToString()
    let trim_start (str : string, trim_chars) = str.TrimStart (trim_chars)
    let trim_end (str : string, trim_chars) = str.TrimEnd (trim_chars)

    let substitute_zerostr_for_empty (str : string) = 
        match String.IsNullOrWhiteSpace str with 
        | (true) -> "0"
        | _ -> str


    let single_char_delim_comma = ','
    let single_char_delim_newline = '\n'
    let custom_single_char_delim_length = 2

    let get_single_char_delims (str : string) = 
        match str.StartsWith CustomDelimPrefix with 
            | (true) -> (seq [ str.Substring (custom_single_char_delim_length, 1) ], custom_single_char_delim_length)   // custom single char delim
            | _ -> (seq [ String([| single_char_delim_comma |]) ; String([| single_char_delim_newline |]) ], 0)         // use default delims


    [<Literal>]
    let MultiCharDelimStart = '['
    [<Literal>]
    let MultiCharDelimEnd = ']'
    let trim_start_multi_char_delim (str : string) = trim_start (str, [| MultiCharDelimStart |])
    let trim_end_multi_char_delim (str : string) = trim_end (str, [| MultiCharDelimEnd |])
    let trim_multi_char_delims = trim_start_multi_char_delim >> trim_end_multi_char_delim

    let match_to_value_string (m : Match) = m.Value.ToString()
    let extract_match_values (matches : MatchCollection) =
        matches
        |> Seq.cast<Match>
        |> Seq.map match_to_value_string

    let get_multi_char_delims (str : string) =         
        let regex = new Regex(CustomMultiDelimPattern)
        let delims = regex.Matches(str) |> extract_match_values |> Seq.map trim_multi_char_delims
        let delimLength = (CustomDelimPrefix.Length + (delims |> Seq.map (fun d -> 2 + d.Length) |> Seq.sum) + CustomDelimSuffix.Length) // 2x char for [] foreach delim
        (delims, delimLength)


    let multi_char_delim_prefix = CustomDelimPrefix + String([| MultiCharDelimStart |])

    let get_numstr_parts (str : string) = 
        let delims, delimsLength = 
            match str.StartsWith multi_char_delim_prefix with
            | (true) -> get_multi_char_delims str 
            | _ -> get_single_char_delims str

        let num_data = 
            match delimsLength > 0 with 
            | (true) -> str.Substring delimsLength
            | _ -> str

        num_data.Split (Seq.toArray delims, StringSplitOptions.RemoveEmptyEntries)
    

    let error_message_value_delim = ','
    let trim_end_error_message (str: string) = trim_end (str, [| error_message_value_delim |])
    let error_message_value_concat = fun a b -> a + b + String([| error_message_value_delim |])

    let format_negatives_not_allowed_error_message less_than_zero_nums = 
        match Seq.length less_than_zero_nums > 0 with
        | (true) -> less_than_zero_nums |> Seq.map itoa |> Seq.fold error_message_value_concat EmptyString |> trim_end_error_message
        | _ -> EmptyString
            

    let validate_nums int_nums =
        let numsToConsider = int_nums |> Seq.filter (flip (<) 1001) // gt 1000 ignored
        let numsLtZero = numsToConsider |> Seq.filter (flip (<) 0)  // lt zero is invalid
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
