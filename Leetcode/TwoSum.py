class Solution(object):
    def twoSum(self, nums, target):
        """
        :type nums: List[int]
        :type target: int
        :rtype: List[int]
        """
        if(len(nums)==1):
            if(target-nums[1]==nums[1]):
                return(0,0)
        pos=0
        while(pos<len(nums)):
            if(target-nums[pos] in nums ):
                return (pos,nums.index(target-nums[pos])) 
            elif(target-nums[pos]==nums[pos] or target-nums[pos]not in nums):
                pos+=1
        
                   
            
