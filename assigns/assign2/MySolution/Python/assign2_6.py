#
# Assign2-6: 20 points
# Please translate the following code
# into Python and then implement all the
# functions called in the code to make the
# translation (written in Python) works correctly
#
# let
# string_merge
# (cs1: string)(cs2: string): string =
# let n1 = string_length(cs1)
# and n2 = string_length(cs2) in
# let rec
# foreach(i1: int)(i2:int)(work) =
# if
# i1 < n1
# then (
# if
# i2 < n2
# then
#   let c1 = string_get_at(cs1)(i1)
#   and c2 = string_get_at(cs2)(i2) in
#   if c1 <= c2
#   then (work(c1); foreach(i1+1)(i2+0)(work))
#   else (work(c2); foreach(i1+0)(i2+1)(work))
# else
#   int1_foreach(n1-i1)
#     (fun i -> work(string_get_at(cs1)(i1+i)))
# ) else (
#   int1_foreach(n2-i2)
#     (fun i -> work(string_get_at(cs2)(i2+i)))
# )
# in
#   string_make_fwork(foreach(0)(0))
# ;;
#


# This function merges two input strings, cs1 and cs2, into a single string
def string_merge(cs1, cs2):
    n1 = len(cs1)
    n2 = len(cs2)

    def foreach(i1, i2, work):
        if i1 < n1:
            if i2 < n2:
                if  cs1[i1] <=  cs2[i2]:
                    work( cs1[i1])
                    foreach(i1 + 1, i2 + 0, work)
                else:
                    work( cs2[i2])
                    foreach(i1 + 0, i2 + 1, work)
            else:
                int1_foreach(n1 - i1, lambda i: work(cs1[i1 + i]))
        else:
            int1_foreach(n2 - i2, lambda i: work(cs2[i2 + i]))
            
            
    def string_make_fwork(fwork):
        result = []

        def work(c):
            result.append(c)

        fwork(work)
        return ''.join(result)

    return string_make_fwork(lambda work: foreach(0, 0, work))

def int1_foreach(c, work):
    for i in range(c):
        work(i)


