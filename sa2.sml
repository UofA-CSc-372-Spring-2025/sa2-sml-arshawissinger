(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Arsha Wissinger *)
(* Time spent on SA2: 4 hours *)

(* Collaborators and references: ChatGPT *)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)
(* Returns true if list is empty, false otherwise *)
fun mynull [] = true
  | mynull _ = false

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true

(**** Problem B ****)
(* Takes a list of lower-case letters and returns true if the first character 
 * is a vowel (aeiou) and false if the character is not a vowel or empty *)
fun firstVowel [] = false
  | firstVowel(x::_) = 
    case x of
        #"a" => true
      | #"e" => true
      | #"i" => true
      | #"o" => true
      | #"u" => true
      | _ => false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true

(**** Problem C ****)
(* Define reverse : 'a list -> 'a list using foldl *)
fun reverse list = 
  List.foldl(fn(x, acc) => x :: acc) [] list

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]

(**** Problem D ****)
(* Returns the smallest element of a nonempty list of integers. *)
fun minlist [] = raise Match
  | minlist (x::xs) =
      List.foldl(fn(x, acc) => Int.min(x, acc)) x xs

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0

(**** Problem E ****)
(* Takes a pair of lists (of equal length) and returns the equivalent
 * list of pairs. If the lengths don't match, raise the exception Mismatch *)
exception Mismatch

fun zip ([],[]) = []
  | zip(x::xs, y::ys) = (x,y) :: zip (xs, ys)
  | zip(_,_) = raise Mismatch

(**** Problem F ****)
(* Takes a list of lists of 'a and produces a single list of 'a containing 
 * all the elements in the correct order. *)
fun concat [] = []
  | concat (x::xs) = x @ concat xs;

(**** Problem G ****)
(* Takes a single character and returns true if the character is a digit 
 * ('0' to '9') and false otherwise. *)
fun isDigit x =
  case x of
      #"0" => true
    | #"1" => true
    | #"2" => true
    | #"3" => true
    | #"4" => true
    | #"5" => true
    | #"6" => true
    | #"7" => true
    | #"8" => true
    | #"9" => true
    | _ => false;

val () =
    Unit.checkExpectWith Bool.toString "isDigit #"1" should be true"
    (fn () => isDigit #"1") 
    true;

(**** Problem H ****)
(* Takes a single character and returns true if the character is an 
 * alphabetical letter ('a' to 'z' or 'A' to 'Z') and false otherwise. *)
fun isAlpha x = 
  let
    val ascii = Char.ord x
  in
    (ascii >= Char.ord #"a" andalso ascii <= Char.ord #"z") orelse
    (ascii >= Char.ord #"A" andalso ascii <= Char.ord #"Z")
  end;

(**** Problem I ****)
(* Takes a tuple of four values and returns a string in the following format
 * <circle cx="..." cy="..." r="..." fill="..." /> *)
fun svgCircle (cx, cy, r, fill) =
  "<circle cx=\"" ^ Int.toString cx ^
  "\" cy=\"" ^ Int.toString cy ^
  "\" r=\"" ^ Int.toString r ^
  "\" fill=\"" ^ fill ^
  "\" />"

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";

(**** Problem J ****)
(* Takes a predicate function and a list, and splits the list into two lists:
 * The first list contains elements that satisfy the predicate.
 * The second list contains elements that do not satisfy the predicate. *)
fun partition p [] = ([], [])
  | partition p (x :: xs) =
      let
        val (true_list, false_list) = partition p xs
      in
        if p x then
          (x :: true_list, false_list)
        else
          (true_list, x :: false_list)
      end;

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);

(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)
