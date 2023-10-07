# Assign4-6:
#
# HX-2023-10-06: 30 points (bonus)
#
# (*
# //
# Please implement the following function
# that enumerates all the pairs (i, j) of natural
# numbers satisfying $i <= j$; a pair (i1, j1) must
# be enumerated ahead of another pair (i2, j2) if the
# following condition holds:
#   i1*i1*i1 + j1*j1*j1 < i2*i2*i2 + j2*j2*j2
# //
# let
# theNatPairs_cubesum(): (int * int) stream = fn () =>
# //
# *)
#
# def theNatPairs_cubesum(): # please give your implementation
#
################################################
def theNatPairs_cubesum():
    i = 0
    j = 0
    while True:
        if i ** 3 + j ** 3 <= (i + 1) ** 3 + j ** 3:
            yield (i, j)
            i += 1
        else:
            j += 1
            i = 0

# Test the code
n = 10  # You can specify the number of pairs to generate
pairs = []
generator = theNatPairs_cubesum()
for _ in range(n):
    pair = next(generator)
    pairs.append(pair)

for i, j in pairs:
    print(f"({i}, {j})")
