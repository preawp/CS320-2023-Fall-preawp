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
    return "".join(c0 if i == i0 else cs[i] for i in range(len(cs)))

alphabet = "".join(chr(ord('a') + i) for i in range(26))

def list_of_buddies(word):
    n0 = len(word)

    def make_work(work):
        for i0 in range(n0):
            c0 = word[i0]
            for c1 in alphabet:
                if c1 != c0:
                    work(string_fset_at(word, i0, c1))

    buddies = []
    make_work(buddies.append)
    return buddies
