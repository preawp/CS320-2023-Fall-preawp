# Assign4-5:
#
# HX-2023-10-06: 30 points
# Please translate the following code from OCaml
# into Python:
#
# let
# string_fset_at
# (cs: string)(i0: int)(c0: char) =
# string_tabulate
# (string_length(cs))
# (
# fun i ->
# if i <> i0 then string_get_at(cs)(i) else c0)
# ;;
# (* ****** ****** *)
#
# let
# alphabet =
# string_tabulate(26)(fun i -> chr(ord('a') + i));;
#
# (* ****** ****** *)
#
# let
# list_of_buddies
# (word: string): string list =
# let n0 =
# string_length(word) in
# list_make_fwork
# (
# fun work ->
# int1_foreach(n0)
# (
# fun i0 ->
# let c0 =
# string_get_at(word)(i0) in
# string_foreach(alphabet)
# (fun c1 -> if c1 <> c0 then work(string_fset_at(word)(i0)(c1)))))
# ;; 
# (* ****** ****** *)
#
#
################################################

def string_fset_at(cs, i0, c0):
    def tabulate(length, func):
        return [func(i) for i in range(length)]

    def get_at(cs, i):
        return cs[i]

    def set_at(cs, i, c):
        cs = list(cs)
        cs[i] = c
        return ''.join(cs)

    return tabulate(len(cs), lambda i: get_at(cs, i) if i != i0 else c0)

# ****** ******

def list_of_buddies(word):
    def foreach(iterable, func):
        for item in iterable:
            func(item)

    def length(cs):
        return len(cs)

    def get_at(cs, i):
        return cs[i]

    def make_fwork(func):
        result = []
        foreach(range(length(word)), lambda i0: result.append(
            set_at(word, i0, c1) for c1 in alphabet if get_at(word, i0) != c1))
        return result

    alphabet = ''.join([chr(ord('a') + i) for i in range(26)])
    return make_fwork(string_fset_at)

# Test the code
word = "word"
buddies = list_of_buddies(word)
for buddy in buddies:
    print(buddy)
