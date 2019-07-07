class Solution(object):
    def romanToInt(self, s):
        """
        :type s: str
        :rtype: int
        """
        output=0
        dictionary = {
            'I': 1,
            'V': 5,
            'X': 10,
            'L': 50,
            'C': 100,
            'D': 500,
            'M': 1000
        }
        for i in range (len(s)):
            if(i>0 and dictionary[s[i]] > dictionary[s[i-1]]):
                output+=dictionary[s[i]]-2*dictionary[s[i-1]]
            
            else:
                output=output+dictionary[s[i]]
            
                
        return output
