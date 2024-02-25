class Solution(object):
    def findMedianSortedArrays(self, nums1, nums2):
        print('nums1: ',nums1)
        print('nums2: ',nums2)
        if (len(nums1)>len(nums2)):
            s1 = nums2
            s2 = nums1
        else:
            s1 = nums1
            s2 = nums2
        if (len(s1)==0):
            if (len(s2)%2 ==0):
                return (s2[len(s2)//2-1]+s2[len(s2)//2])/2
            else:
                return s2[len(s2)//2]
        elif (len(s1)==1):
            if (len(s2)%2 ==0):
                if s1[0]<s2[len(s2)//2-1]:
                    return s2[len(s2)//2-1]
                elif s1[0]<s2[len(s2)//2]:
                    return s1[0]
                else:
                    return s2[len(s2)//2]
            else:
                if len(s2)==1: 
                    return (s1[0]+s2[0])/2
                elif s1[0]<s2[len(s2)//2-1]:
                    return (s2[len(s2)//2-1]+s2[len(s2)//2])/2
                elif s1[0]>s2[len(s2)//2+1]:
                    return (s2[len(s2)//2+1]+s2[len(s2)//2])/2
                else:
                    return (s1[0]+s2[len(s2)//2])/2
        else:
            mid1=s1[len(s1)//2]
            mid2=s2[len(s2)//2]
            if mid1<mid2:
                deleteL = len(s1)//2
                return self.findMedianSortedArrays(s1[len(s1)//2:], s2[:len(s2)-deleteL])
            else:
                deleteL = len(s1)-len(s1)//2
                return self.findMedianSortedArrays(s1[:len(s1)//2], s2[deleteL:])

solution=Solution()
x1=[1,3,4,7,89,244]
x2=[2,4,7,8,9,10,11,23,43,78,99,123,178,389]
x1=[1,2]
x2=[-1,3] # not solved
y=solution.findMedianSortedArrays(x1,x2)
print(y)
