class Solution:
    mytable={}
    def rob(self, nums) -> int:
        if (self.mytable.get(len(nums),0)):
            return self.mytable[len(nums)]
        print(nums)
        #print(self.mytable)
        if len(nums) ==1:
            return nums[0]
        elif len(nums) ==2:
            return max(nums[0], nums[1])
        rob1 = nums[0]+self.rob(nums[2:])
        rob2 = self.rob(nums[1:])
        value = max(rob1, rob2)
        self.mytable[len(nums)]=value
        return value
        
solution = Solution()
x = [2, 7, 9, 3, 1]
x = [56,67,243,64,86,25,65,6,23,3,45,44,73,145,45,234,56,23,146,5,43,67]
y=solution.rob(x)
print(y)
