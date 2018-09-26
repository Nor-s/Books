def insertionSort(ls):
    j = 1
    while j < len(ls):
        key = ls[j]
        i = j -1
        while i >= 0 and ls[i] > key:
            ls[i+1] = ls[i]
            i = i-1
        ls[i+1] = key
        j = 1 + j

#2.3-4
def insertRec(ls, p, r):
    if r > 2:
        insertRec(ls, p, r-1)
    key = ls[r-1]
    i = r - 2
    while i >= 0 and ls[i] > key:
        ls[i + 1] = ls[i]
        i -= 1
    ls[i+1] = key


a = list(map(int, input("elements: ").split()))
insertRec(a, 0, len(a))
print("sort: ", a)