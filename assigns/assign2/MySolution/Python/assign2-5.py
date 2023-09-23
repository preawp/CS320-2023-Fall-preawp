
# Assign2: Higher-order programming in Python
# *
########################################################################
#
# Assign2-5: 20 points
# Please implement in Python a function
# of the name fnlist_make_fwork that corresponds
# to the function list_make_fwork in the library
# MyOCaml.ml
#

#transforms the work done by fwork into a list
def fnlist_make_fwork(fwork):
    res = [] 
    
    def work(x0):
        res.append(x0)
        
    fwork(work)
    return res