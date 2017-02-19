module Program = 

    open System    
    open System.Text.RegularExpressions

    let sub_zerostr_for_empty str = 
        if String.IsNullOrWhiteSpace str then "0" else str

    let get_single_char_delims(str : string) =                 
        if str.StartsWith("//") then ([| str.Substring(2, 1) |], 2) else ([| "," ; "\n" |], 0) // (delims, delimLength)

    let get_multi_char_delims(str : string) =         
        let regex = new Regex(@"\[{1}[^\]\[]*\]{1}");
        let delims = regex.Matches(str) |> Seq.cast<Match> |> Seq.map (fun m -> m.Value.TrimStart('[').TrimEnd(']')) |> Seq.toArray
        let delimLength = (2 + (delims |> Seq.map (fun d -> 2 + d.Length) |> Seq.sum) + 1) // 2x char for \\; 2x char for [] foreach delim; 1x char for \n
        (delims, delimLength)

    let get_num_data(str : string, delimsLength : int) = 
        if delimsLength > 0 then str.Substring(delimsLength) else str

    let get_numstr_parts(str : string) =         
        let delims, delimsLength = if str.StartsWith("//[") then get_multi_char_delims str else get_single_char_delims str        
        let num_data = get_num_data (str, delimsLength)        
        num_data.Split (delims, StringSplitOptions.RemoveEmptyEntries)

    let parse_nums num_strs = 
        num_strs |> Seq.map Int32.Parse |> Seq.toArray

    let find_less_than_zero_nums int_nums = 
        int_nums |> Seq.filter (fun n -> n < 0) |> Seq.toArray

    let format_negatives_not_allowed_error_message(less_than_zero_nums : int[]) = 
        if less_than_zero_nums.Length > 0 
        then less_than_zero_nums |> Seq.map (fun n -> n.ToString ()) |> Seq.fold (fun a b -> (a + b + ",")) "" |> (fun s -> s.TrimEnd(',')) 
        else ""

    let get_invalid_num_details nums_to_consider = 
        let less_than_zero_nums = find_less_than_zero_nums nums_to_consider
        let negative_num_message = format_negatives_not_allowed_error_message less_than_zero_nums
        (less_than_zero_nums, negative_num_message, nums_to_consider)

    let assert_no_negatives (bad_nums : int[], bad_nums_message, nums_to_consider) = 
        if bad_nums.Length > 0 then failwith ("negatives not allowed: " + bad_nums_message) else nums_to_consider

    let remove_numbers_gt_1000 int_nums = 
        int_nums |> Seq.filter (fun n -> n < 1001) |> Seq.toArray

    let validate_nums int_nums =                         
        (get_invalid_num_details <| remove_numbers_gt_1000 int_nums) |> assert_no_negatives

    let sum nums = 
        nums |> Seq.sum

    let calc input = 
        sub_zerostr_for_empty input
        |> get_numstr_parts 
        |> parse_nums
        |> validate_nums
        |> sum



    [<EntryPoint>]
    let main argv = 
        printfn "Hello world"
        System.Console.ReadKey() |> ignore
        0
