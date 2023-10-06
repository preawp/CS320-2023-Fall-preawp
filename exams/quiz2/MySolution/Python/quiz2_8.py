# (*
# Q2-8: 20 points

# Recall the 'foreach' function and the 'get_at' function.
# For instance, list_foreach(list)(work) applies 'work' to
# each element in the given list 'list'; list_get_at(list)(i)
# returns the element at position 'i' in 'list' if 'i' is a
# valid index; otherwise the Subscript exception is raised.

# Please implement in *Python* a function 'foreach_to_get_at'
# that turns a 'foreach' function into a 'get_at' function.

# (*
# Following is the type for 'foreach_to_get_at' in ocaml:
# fun foreach_to_get_at
#   (foreach: ('list, 'x0) foreach): ('list -> int -> 'x0) = ...
# *)

#
# *)
class SubscriptException(Exception):
    pass

def foreach_to_get_at(foreach): # your implementation below
    def get_at(list, index):
        if  (index < 0) or (index >= len(list)):
            raise SubscriptException("Subscript Exception")
        else :
            cur_index = 0
            for x in foreach(list):
                if cur_index == index:
                    return x
                cur_index += 1
    return get_at

