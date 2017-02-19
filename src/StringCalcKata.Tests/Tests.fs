namespace StringCalcKata.Tests

module ProgramShould = 
    open System
    open Program
    open Xunit

    [<Fact>]
    let return_zero_for_empty_string() = 
        let input = ""
        let expected = 0

        let result = Program.calc input

        Assert.StrictEqual(expected, result)

    [<Fact>]
    let return_a_single_number() = 
        let input = "1"
        let expected = 1

        let result = Program.calc input

        Assert.StrictEqual(expected, result)


    [<Fact>]
    let add_two_numbers() = 
        let input = "1,2"
        let expected = 3

        let result = Program.calc input

        Assert.StrictEqual(expected, result)


    [<Theory>]
    [<InlineData("1,1,1", 3)>]
    [<InlineData("1,2,3,4", 10)>]
    let add_any_number_of_numbers(input : string, expected : int) = 
        let result = Program.calc input

        Assert.StrictEqual(expected, result)


    [<Fact>]
    let handle_new_line_instead_of_comma_for_delim() = 
        let input = "\n1\n2\n3\n4\n5\n"
        let expected = 15

        let result = Program.calc input

        Assert.StrictEqual(expected, result)


    [<Fact>]
    let handle_single_char_custom_delim() = 
        let input = "//;\n1;2"
        let expected = 3

        let result = Program.calc input

        Assert.StrictEqual(expected, result)

           
    [<Theory>]
    [<InlineData("-1", "negatives not allowed: -1")>]
    [<InlineData("//;\n-1;-2", "negatives not allowed: -1,-2")>]
    let should_throw_when_supplied_negative_numbers((input : string), (expected : string)) =                
        try
            Program.calc input
        with
            | ex -> if expected.Equals(ex.Message) then 0 else reraise()


    [<Theory>]    
    [<InlineData("2,1001", 2)>]
    [<InlineData("//;\n1000;2;1001;2000", 1002)>]
    [<InlineData("1,1000", 1001)>]
    let should_ignore_numbers_greater_than_1000((input : string), (expected : int)) =                        
        let result = Program.calc input

        Assert.StrictEqual(expected, result)


    [<Theory>]        
    [<InlineData("1,999", 1000)>]
    [<InlineData("1,1000", 1001)>]
    [<InlineData("//;\n1000;2;3;", 1005)>]
    let should_not_ignore_numbers_less_than_equal_to_1000((input : string), (expected : int)) =                        
        let result = Program.calc input

        Assert.StrictEqual(expected, result)


    [<Fact>]
    let handle_multi_char_custom_delim() = 
        let input = "//[***]\n1***2***3"
        let expected = 6
        
        let result = Program.calc(input)

        Assert.StrictEqual(expected, result)


    [<Fact>]
    let handle_multiple_multi_char_custom_delim() = 
        let input = "//[***][%%%]\n1***2%%%3"
        let expected = 6

        let result = Program.calc(input)

        Assert.StrictEqual(expected, result)
