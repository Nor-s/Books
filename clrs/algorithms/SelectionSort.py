def selec(ls):
    for i in range(len(ls)):
        min = i
        for j in range(i, len(ls)):
            if ls[i] > ls[j]:
                min = j;
        ls[i], ls[min] = ls[min], ls[i]

a = list(map(int, input("Enter element: ").split()))
selec(a)
print("sort: ",a)
