fun mult [ ] = 		1
    | mult (x ::L) = 	x * (mult L);

fun Mult [ ] = 	1
  | Mult (r :: R) = 	(mult r) * (Mult R);


(* multx : int list * int -> int 			*)
(* REQUIRES: true				*)
(* ENSURES: multx(L, a) … evaluates to the product of integers in L and a	*)

fun multx ([ ], a) = a
  | multx (x :: L, a) = multx (L, x * a);

(* Multx: int list list * int -> int *)
(* REQUIRES: True               *)
(* ENSURES: Multx(R, a) evaluates to the product of integers in R times a*)

fun Multx( [ ], a) =  a
  | Multx(r::R, a) =   multx(r,a)*Multx(R,a);

(* double : int -> int *)
(* REQUIRES: n >= 0 *)
(* ENSURES: double n evaluates to 2 * n.*)

fun double (0 : int) : int = 0
    | double n = 2 + double (n - 1)

(* 编写函数square*)

(* square : int -> int *)
(* REQUIRES: n >= 0 *)
(* ENSURES: square n returns n * n *)

fun square (0 : int) = 	0
 | square n =  n + n - 1 + square(n-1);

(*****End*****)
["hello",3];
 (*** Begin ***)
 (* zip : string list * int list -> (string * int) list *)
 fun zip ([],[])= []
  | zip ([],L2) = []
  | zip (L1,[]) = []
  | zip (s1 :: L1,x2 :: L2) = (s1:string , x2:int) :: zip(L1,L2);
 
 
 (* unzip : (string * int) list -> string list * int list *)
  fun unzip ([]:(string * int) list )= ([]:string list,[]:int list)
  | unzip (x :: xs) = (#1 x :: (#1 (unzip(xs))),#2 x :: (#2 (unzip(xs))));
  
 (*** End ***)

(****   Begin      ****)
fun oddP (0) = false
| oddP (1) = true
| oddP (x) = oddP(x-2);
(****   End      ****)

(*****Begin*****)
fun divisibleByThree(0) = true
|divisibleByThree(1) = false
|divisibleByThree(2) = false
|divisibleByThree(x) = divisibleByThree(x-3);

(*****End*****)

(*** Begin ***)
fun interleave([],R):int list = R
| interleave(L,[]) = L
| interleave(x::L,y::R) = x::y::interleave(L,R);
 
(*****End*****)

val str = "Hello, World!";
print(str);

fun printString(str: string) =
    print str;

fun all (your, base) =
    case your of
        0 => base
      | _ => "are belong to us" :: all(your - 1, base);

fun main () =
    let
        val result = all(3, "Hello, ")
    in
        printString(result);
    end;

val _ = main ();

fun all (your, base) =
    case your of
             0 => base
          | _ => "are belong to us" :: all(your - 1, base)

fun funny (f, []) = 0
| funny (f, x::xs) = f(x, funny(f, xs))

fun printInt (a:int) =
    print(Int.toString(a)^" ");

fun getInt () =
    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);
    
fun printIntList ( [] ) = ()
  | printIntList ( x::xs ) = 
    let
	val tmp = printInt(x)
    in
	printIntList(xs)
    end;

fun getIntList ( 0 ) = []
  | getIntList ( N:int) = getInt()::getIntList(N-1);

fun split [ ]  = ([ ], [ ]) 
    | split [x] = ([ ], [x])
    | split (x::y::L) =
	let val (A, B) =split L
	in (x::A, y::B) 	
	end;

datatype tree = Empty | Br of tree * int * tree; 

fun trav(Br(t1,a,t2)) = trav(t1)@(a::trav(t2))
    |trav empty = [];

(*BEGIN*)
fun listToTree ([] : int list) : tree = Empty
  | listToTree (x::l) = 
  let val (A, B) =split l
  in Br(listToTree A, x, listToTree B)
  end;
fun revT (Empty : tree) : tree = Empty
    | revT (Br(A, x, B)) = Br(revT B, x, revT A)
(*END*)

val L = getIntList(7);
printIntList (trav(revT(listToTree L)));



fun hd (x::_) = x;

fun partition ([], less, greaterEq) = less @ greaterEq
  | partition (x::xs, less, greaterEq) =
    if x < hd greaterEq
    then partition(xs, less, x::greaterEq)
    else partition(xs, x::less, greaterEq)

fun quickSort [] = []
  | quickSort [x] = [x]
  | quickSort (x::xs) =partition(xs, [], [x])

  fun partition(a: int, xs: int list) : int list * int list =
    let
        fun aux([], less, greater) = (less, greater)
          | aux(x::rest, less, greater) =
            if x < a
            then aux(rest, less @ [x], greater)
            elseif x > a
            then aux(rest, less, greater @ [x])
            else 
            then aux(rest, less, greater)
    in
        aux(xs, [], [])
    end;


