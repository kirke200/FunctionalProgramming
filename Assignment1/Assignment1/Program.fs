open System.Windows.Forms

// s164159
// Mathias Bo Jensen

//
type Member = {
    name: string;
    no : string;
    yb : int;
    ths : string list
    } 

type reg = Member list

// Register:

let reg = [
    {
        name = "Hans";
        no = "12341234";
        yb = 1980;
        ths = ["soccer" ; "beer"]
        } ;
    {
        name = "Simon";
        no = "87654321";
        yb = 1994;
        ths = ["food" ; "climbing"]
        } ;
    {
        name = "Robert";
        no = "21436587";
        yb = 1983;
        ths = ["jazz" ; "soccer"]
        } ;
    {
        name = "Frederik";
        no = "29921039";
        yb = 1995;
        ths = ["jazz" ; "handball"]
        } ;
    ]

// Creating a member:
let memberA = {name = "Sue" ; no = "12345678"; yb = 1984; ths = ["jazz" ; "soccer"]}

// Finding people interested in a certain arrangement
// person list --> 'a --> bool --> (string , string) list
let rec extractInterested r p =
    match r with
    | [] -> []
    | head::tail when p(head) -> (head.name,head.no)::extractInterested tail p
    | head::tail -> extractInterested tail p


// functions for checking if a member would be interested:
// Member --> bool
let p1 {name = n; no = _; yb = y; ths = t} 
    = y > 1982 && (List.contains "jazz" t && List.contains "soccer" t)

// skodløsning med dobbelt 'List.contains'?
let p2 {name = n; no = _; yb = y; ths = t}
    = y > 1982 && (List.contains "jazz" t || List.contains "soccer" t)

// Testing
p1(memberA)
p2(memberA)