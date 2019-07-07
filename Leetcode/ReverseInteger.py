class Solution(object):
    def reverse(self, x):
        """
        :type x: int
        :rtype: int
        """
        rev=0
        if(x<-2**31 and x>2**31-1):
            return 0
        elif x>0:
            while(x!=0):
                rem = x%10
                rev=rev*10+rem
                x=x/10
            if(rev>2**31-1):
                return 0
            return rev
        elif(x<0):
            x*=-1
            while(x!=0):
                rem=x%10
                rev = rev*10+rem
                x=x/10   
            if(rev*(-1)>-2**31):
                return -1*rev
            return 0
        elif(x==0):
            return rev
