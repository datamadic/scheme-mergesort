#### Merge Sort
The Merge Sort algorithm recursively breaks a list in half until it has sublists of length one (or zero), then rebuilds the list from these sublists merging them together in order. 

The pseudocode is basically this:
````
function mergesort (list)
    if list length <= 1
        return list

    merge (
        mergesort (first half of the list) 
        mergesort (second half of the list)
    )

````
Where the `merge` function builds a new merged list by walking both lists from their beginnings taking the lesser item from either list as the next item in the merged list. The algorithm is something like this:
````
merged = []
list1 = [1, 2]
list2 = [0, 3]

//compare
if list1[0] <= list2[0] 
    add list1[0] to merged 
else 
    add list2[0] to merged

merged now [0]
list1 now [1, 2]
list2 now [3]

//compare 

merged now [0, 1]
list1 now [2]
list2 now [3]

...
````

#### My Implementation of Merge Sort in Scheme.

When starting to translate Merge Sort into Scheme I found that starting from the inside out was much easier than starting from the outer functions and working inward. At the heart of Merge Sort is a merge function, at the heart of that is a compare so lets start there. 

The structure of Scheme statements may seem jarring at first, but after a little while with it I have come to find it quite elegant. It can be boiled down to: 
````
(operator operand [operand...])
````
Thats it. All operators are in prefix notation. Operands can be data (strings, numbers, structures, etc) or other operators. This pattern can be nested to arbitrary depths. Looping is achieved via recursion.

So to get a less than comparison, instead of saying:
````
var1 < var2
````
you write:
````
(< var1 var2)
````
To compare var1 against 1 less than var2 you would write:
````
(< var1 (- var2 1))
````

In our case wee need to check if the first item of the first list is less than or equal to the first item of the second list. In Scheme, the operator `car` gets you the first item of a list. So the phrase would look like:
````
(<= (car list1) (car list2))
````

If this is the case, we want to add list1's first item to the merged list and add that to the result of recursively calling `merge` against the rest of list1 and all of list2. The premise is the same for the case where list2 has the smaller first item. In Scheme the `cons` operator constructs a list and the `cdr` operator returns you a list without it's first item (its `car`). The `cdr` of a list of length one is an empty list. The phrase looks like this:
````
 (cons (car list1) (merge (cdr list1) list2))
````

Because it is a recursive call, we need to check for the end condition of a list being null. In this case we just return the other list. Checking for an empty list is done via the `null?` operator and looks like this:
````
(null? list1)
````


There are now four possible conditions that need to be checked for (list 1 or 2 is null, the first item in list 1 or 2 being larger), and four different possible return values. The control structure I chose was `cond` which takes a list of true/false predicates paired with outcomes and executes the first true predicate and returns the evaluation of its paired expression. As all operators it is wrapped in parens and looks like this:
````
((lambda ()                                                 
   (cond                                                    
    ((< 2 1) "2 < 1")                                       
    ((< 1 2) "1 < 2")
    ((< 1 2) "never returned"))))
````
The above function will return "1 < 2".

The `lambda` operator denotes that this is expression is a function. The `()` after indicates that this function takes no arguments. In this case it is an anonymous function that by virtue of living in parens is immediately executed. The `lambda` operator can be used in conjunction with `define` to explicitly indicate that what is being defined is function.

Lets define our `merge` function where this logic will live. Whether you are defining a function or a piece of data the operator is the same and is called `define`. 
````
(define merge 
  (lambda (l1 l2)
    (cond 
     ((null? l1) l2)
     ((null? l2) l1)
     ((<= (car l1) (car l2))
    (cons (car l1) (merge (cdr l1) l2)))
     ((cons (car l2) (merge (cdr l2) l1))))))
```` 

You will notice that there are no return statements. The return is implicit in the `cond` operator.


Now having a `merge` function, lets define the `mergesort` function.
````
(define mergesort 
  (lambda (lst)
    (if (<= (length lst) 1)
  lst
  (let ((left (car (split lst)))
        (right (cdr (split lst))))
    (merge (mergesort left) (mergesort right))))))
````

The `mergesort` function takes one argument, the list to be sorted. There are two paths through this function. If the list passed in has length zero or one, we just return it. Notice that the `if` operator takes a predicate and one or two expressions. If the predicate is true, `if` returns the first expression, else there is an implied `else` statement and `if` will return the second expression. 

The other path through the `mergesort` function uses the `let` operator to define local variables to hold the left and right half of the split list. `mergesort` will then have `merge` reassemble the result of a recursive call on these sublists. 
 
