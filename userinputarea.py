#THIS IS THE NEW CODE






import sys
sys.path.append('/home/administrator/Desktop')
import area
                
def main():
    '''
    Objective : To find area of Rectangle or Triangle
    User inputs :
        For Area Of Rectangle
            length  : length of rectangle
            breadth : breadth of rectangle
        For Area of Triangle
            base   : base of triangle
            height : height of triangle
    '''
    #approach :  Compute Area of Rectangle and Area of Triangle using areaRectangle and areaTriangle function respectively
        
    areaType = input('Which area you want to find (Rectangle/Triangle) : ')
    if areaType == 'Rectangle':
        print('\t\t============================= ')
        print('\t\t Computing Area Of Rectangle ')
        print('\t\t=============================\n ')
        length  = int(input('Enter the length of Rectangle  : '))
        breadth = int(input('Enter the breadth of Rectangle : '))
        print('\nThe Area of Rectangle is : ',area.areaRectangle(length,breadth))
    
    elif areaType == 'Triangle':
        print('\t\t============================= ')
        print('\t\t Computing Area of Triangle ')
        print('\t\t=============================\n ')
        base   = int(input('Enter the base of Triangle   : '))
        height = int(input('Enter the height of Triangle : '))
        print('\nThe Area of Triangle  is : ',area.areaTriangle(base,height))

    else :     
        print('Wrong Input')
        
if __name__=='__main__':
    main()
