|| -*- mode: text; -*-

||| Lists: sequence of elements that must be of the same type
|| - Lists are also tuples (?)
numbers = [0,1,2,3]
week_days = ["Mon","Tue","Wed","Thur","Fri"]

||| Tuple: sequence of elements that can be potentially of mixed types
|| - Tuples cannot be subscripted
this_is_a_tuple = ("Nicolai", "Singh", 123, True)

||| List operations
list_length = #[0,1,2,3]    || Prefixing # returns the length of a list (returns 4)
first_element = [0,1,2,3]!1 || Returns 1st element (which is 1)

days = week_days ++ ["Sat","Sun"]       || Adding/Appending to lists
numbers2 = 0:[1,2,3]                    || Another type of append: prefixes an element in front of a list
even_numbers = [1,2,3,4,5] -- [1,3,5]   || Subtracting/Removing from lists

one_to_ten = [1..10]    || Shorthand for making an arithmetic series
foo = [1,3..50]         || [1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49]

||| Guards in equations
gcd a b = gcd (a-b) b, if a>b
        = gcd a (b-a), if a<b
        = a,           otherwise


|| Introduce local definitions on the right hand side of a definition
quadsolve a b c = error "complex roots", if delta<0
                = [-b/(2*a)],            if delta=0
                = [-b/(2*a) + radix/(2*a),
                   -b/(2*a) - radix/(2*a)], if delta>0
                  where
                  delta = b*b - 4*a*c
                  radix = sqrt delta
