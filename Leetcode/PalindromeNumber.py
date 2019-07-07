class Solution(object):
    def isPalindrome(self, x):
        """
        :type x: int
        :rtype: bool
        """
        rev=0
        num = x
        if(x>0):
            while(x!=0):
                rem = x%10
                rev=rev*10+rem
                x /= 10
            if(rev==num):
                return 1
            else:
                return 0
        if(x<0 or (x%10==0 and x!=0)):
            return 0
        if(x==0):
            return 1
        
