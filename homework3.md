# 作业

## T1

这两个函数在功能和性能方面有以下异同点：

功能：

- take函数：该函数接受一个列表和一个整数i作为参数，返回列表中**前i个元素**组成的新列表。如果i大于列表长度，则返回整个列表。
- rtake函数：该函数接受一个列表、一个整数i和一个已取出的元素列表作为参数，返回列表中**前i个元素逆序和初始taken元素**组成的新列表。如果i大于列表长度，则返回整个列表的逆序。

性能：

- take函数：该函数使用递归方式实现，每次递归都会将列表剩余元素的头部元素添加到结果列表中，直到达到指定的元素数量。递归的时间复杂度为O(n)，其中n是列表的长度。
- rtake函数：该函数也使用递归方式实现，但是每次递归都会将列表剩余元素的头部元素添加到中间结果列表的开头，以实现逆序。递归的时间复杂度也为O(n)，其中n是列表的长度。

异同点：

- take函数和rtake函数的功能有所不同，take函数返回列表的前i个元素，而rtake函数返回列表的前i个元素的逆序和初始taken的组合。
- take函数和rtake函数在处理方式上有所不同，take函数取元素放到结果列表的头部，而rtake函数取元素放到最终结果列表的尾部。
- take函数和rtake函数的性能相似，都具有线性的时间复杂度，取决于列表的长度。

## T2

```txt
nextperm [3,2,4,1] 
=> next [3] [2, 4, 1] 
=>3 > 2
=> swap [3] 
=> 2::3::[4,1] 
=> [2,3,4,1]
```

## T3

下面是quickSort和partition函数的实现代码：

```sml
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
```
