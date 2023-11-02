datatype 'a tree = Empty| Node of 'a tree*'a*'a tree;
datatype 'a options = NONE | SOME of 'a;

fun treeFilter f Empty = Empty
  | treeFilter f (Node(L,x,R)) = if f x then Node(treeFilter f L,SOME x,treeFilter f R)
                                                                              else Node(treeFilter f L,NONE,treeFilter f R);


(*test code*)
fun printInt (a:int) =
    print(Int.toString(a)^" ");
fun printIntList ( [] ) = ()
  | printIntList ( x::xs ) = 
    let
	val tmp = printInt(x)
    in
	printIntList(xs)
    end;
fun split [ ]  = ([ ], [ ]) 
    | split [x] = ([ ], [x])
    | split (x::y::L) =
	let val (A, B) =split L
	in (x::A, y::B) 	
	end;
fun listToTree ([] : 'a list) : 'a tree = Empty
  | listToTree (x::l) = 
  let val (A, B) =split l
  in Node(listToTree A, x, listToTree B)
  end;
fun trav(Node(t1,a,t2)) = trav(t1)@(a::trav(t2))
    |trav empty = [];
fun gttwo(x: int): bool =
  if x > 2 then true
  else false;

val L = [7,6,5,4,3,2,1];
printIntList (trav(listToTree L));

