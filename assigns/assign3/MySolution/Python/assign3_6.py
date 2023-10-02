# Assign3-6: 30 points
# Please translate the datatype mylist (given in Assign2) into
# type classes (by following the example of fnlist in MyPython.py).
# Then please translate mylist_foreach and mylist_rforeach into Python
#
########################################################################

#translate the datatype mylist

class mylist:
    pass

class mylist_nil(mylist):
    def __init__(self):
        pass

class mylist_cons(mylist):
    def __init__(self, head, tail):
        self.head = head
        self.tail = tail

    def mylist_cons(self):
        return self.head, self.tail

class mylist_snoc(mylist):
    def __init__(self, init, last):
        self.init = init
        self.last = last

    def mylist_snoc(self):
        return self.init, self.last

class mylist_reverse(mylist):
    def __init__(self, lst):
        self.lst = lst

    def mylist_reverse(self):
        return self.lst

class mylist_append2(mylist):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def mylist_append2(self):
        return self.left, self.right
 
 #translate mylist_foreach and mylist_rforeach 
    
def mylist_foreach(xs, work):
    if isinstance(xs, mylist_nil):
        pass  
    elif isinstance(xs, mylist_cons):
        x1, xs = xs.mylist_cons()
        work(x1)  
        mylist_foreach(xs, work) 
    elif isinstance(xs, mylist_snoc):
        xs, x1 = xs.mylist_snoc()
        mylist_foreach(xs, work) 
        work(x1)  
    elif isinstance(xs, mylist_reverse):
        xs = xs.mylist_reverse()
        mylist_rforeach(xs, work) 
    elif isinstance(xs, mylist_append2):
        xs1, xs2 = xs.mylist_append2()
        mylist_foreach(xs1, work) 
        mylist_foreach(xs2, work) 

def mylist_rforeach(xs, work):
    if isinstance(xs, mylist_nil):
        pass  
    elif isinstance(xs, mylist_cons):
        x1, xs = xs.mylist_cons()
        mylist_rforeach(xs, work) 
        work(x1)  
    elif isinstance(xs, mylist_snoc):
        xs, x1 = xs.mylist_snoc()
        work(x1)  
        mylist_rforeach(xs, work)  
    elif isinstance(xs, mylist_reverse):
        xs = xs.mylist_reverse()
        mylist_foreach(xs, work)  
    elif isinstance(xs, mylist_append2):
        xs1, xs2 = xs.mylist_append2()
        mylist_rforeach(xs2, work)  
        mylist_rforeach(xs1, work) 
