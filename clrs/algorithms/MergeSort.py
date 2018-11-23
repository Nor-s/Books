# 2.3-2
def merge(ls, p, q, r):
    """ Merge = p~q + q~r.

    This is O(n).
    """
    i = 0
    j = 0
    L = ls[p:q+1]
    R = ls[q+1:r+1]
    k = p
    print("L",L, "R", R)

    while i!= q - p + 1 and j != r - q:
        if L[i] < R[j]:
            ls[k] = L[i]
            i += 1
        else:
            ls[k] = R[j]
            j += 1
        k += 1

    if i == q - p + 1:
        while k <= r:
            ls[k] = R[j]
            j += 1
            k += 1
    if j  == r - q:
        while k <= r:
            ls[k] = L[i]
            i += 1
            k += 1

def mergeSort(ls, p, r):
    if p < r:
        q = (p+r)//2
        mergeSort(ls, p, q)
        mergeSort(ls, q+1, r)
        merge(ls, p, q, r)

a = list(map(int, input("Enter elements: ").split()))

mergeSort(a, 0, len(a)-1)
print("sort: ",a)