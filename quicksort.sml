fun printInt (a:int) =
    print(Int.toString(a)^" ");

fun printIntList ( [] ) = ()
  | printIntList ( x::xs ) = 
    let
	val tmp = printInt(x)
    in
	printIntList(xs)
    end;
fun hd (x::_) = x;

fun partition ([], less, greaterEq) = less @ greaterEq
  | partition (x::xs, less, greaterEq) =
    if x < hd greaterEq
    then partition(xs, less, x::greaterEq)
    else partition(xs, x::less, greaterEq)

fun splitList (x, []) = ([], []) 
    | splitList (x, y::ys) = 
        let 
            val (left, right) = splitList (x, ys) 
        in 
            if y < x then (y::left, right) 
            else (left, y::right) 
        end
fun quickSort [] = []
  | quickSort [x] = [x]
  | quickSort (x::xs) =
    let
        val (left,right) =splitList(x, partition(xs, [], [x]))
    in
        quickSort(left)  @ quickSort(right)
    end

val L=[3,6,4,8,9,1,2];
printIntList(quickSort(L));