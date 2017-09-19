def SortList(List, cIndex = 0, iIndex = 0, index = 0, minElement = 0):


    '''
    Objective        : To sort a list using recursion approach.
    Input Parameters :
                list : Holding the unsorted elements in a list.
              cIndex : Holding the element of the list to be checked.
              iIndex : Holding the initial element of the list.
               index : Holding the index of the current element.
          minElement : Index holding the minimum element of previous call.

    '''

    #Approach          : Using recursion.


    assert index <= len(List)

    

    
    if iIndex == len(List):
        return

    elif index < len(List):

        if List[minElement] > List[index]:
            minElement = index

        SortList(List, cIndex, iIndex, index+1, minElement)

    elif (index == len(List)):

         temp = List[minElement]
         List[minElement] = List[cIndex]
         List[cIndex] = temp

         SortList(List, cIndex+1, iIndex+1, cIndex+1, iIndex+1)
    return List
List = [int(x) for x in input("Enter elements in the list : ").split()]
print("Sorted list is: ",SortList(List))
