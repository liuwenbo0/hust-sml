fun printBool (a:bool) = 
    print(Bool.toString(a)^" "); 

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
fun trav1(Br(t1,a,t2)) = (a::trav1(t1))@trav1(t2)
    |trav1 empty = [];
(*BEGIN*)
fun listToTree ([] : int list) : tree = Empty
  | listToTree (x::l) = 
  let val (A, B) =split l
  in Br(listToTree A, x, listToTree B)
  end;
fun revT (Empty : tree) : tree = Empty
    | revT (Br(A, x, B)) = Br(revT B, x, revT A)
fun binarySearch (Empty : tree, _ : int) : bool = false
    | binarySearch (Br(A, x, B), h) = 
    case Int.compare(x, h) of 
    EQUAL => true
    |LESS => binarySearch(B, h)
    |GREATER => binarySearch(A, h);
(*END*)

fun treecompare (Empty, Empty) = EQUAL
  | treecompare (Empty, _) = LESS
  | treecompare (_, Empty) = GREATER
  | treecompare (Br(L1, x1, R1), Br(L2, x2, R2)) =Int.compare(x1, x2);

fun swapDown (Empty) = Empty
  | swapDown (Br(Empty, x, Empty)) = Br(Empty, x, Empty)
  | swapDown (Br(Empty, x, Br(L, y, R))) =
      if x > y then Br(Empty, y, swapDown (Br(L, x, R)))
      else Br(Empty, x, Br(L, y, R))
  | swapDown (Br(Br(L, y, R), x, Empty)) =
      if x > y then Br(swapDown (Br(L, x, R)), y, Empty)
      else Br(Br(L, y, R), x, Empty)
  | swapDown (Br(Br(L1, x, R1), y, Br(L2, z, R2))) =
      if (x <= y) andalso (y <= z) then Br(swapDown (Br(L1, y, R1)), x, Br(L2, z, R2)) (*x<=y<=z*)
      else if (y <= x) andalso (x <= z) then Br((Br(L1, x, R1)), y, Br(L2, z, R2)) (*y<=x<=z*)
      else if (y <= z) andalso (z <= x) then Br((Br(L1, x, R1)), y, Br(L2, z, R2)) (*y<=z<=x*)
      else if (x <= z) andalso (z <= y) then Br(swapDown (Br(L1, y, R1)), x, (Br(L2, z, R2))) (*x<=z<=y*)
      else if (x <= y) andalso (z <= x)then Br(Br(L1, x, R1), z, swapDown (Br(L2, y, R2))) (*z<=x<=y*)
      else Br(Br(L1, x, R1), z, swapDown (Br(L2, y, R2))); (*z<=y<=x*)

fun heapify (Empty) = Empty
  | heapify (Br(left, x, right)) =
      let
        val leftHeap = heapify left
        val rightHeap = heapify right
      in
        swapDown (Br(leftHeap, x, rightHeap))
      end;

val L = [7,6,5,4,3,2,1];
val h = 2;
printIntList (trav(listToTree L));
printIntList (trav1(listToTree L));
(*printIntList (trav(revT(listToTree L)));
printBool(binarySearch((listToTree L), h));*)
printIntList (trav(heapify(listToTree L)));
printIntList (trav1(heapify(listToTree L)));



