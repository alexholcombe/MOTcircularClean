import numpy as np

#For a texture, the first dimension is vertical I guess, not the one I want to vary along, instead it should be identical

# Define the original array
arr = np.array([[[1,2,3],[7,8,9]]])

# Duplicate the array along the first dimension
arr_dup = np.repeat(arr, 2, axis=0)

# Print the duplicated array
print(arr_dup.shape)
print(arr_dup)

# Define the original array
arr = np.array([[[1,2,3],[7,8,9],[10,20,30],[70,80,90]]])

# Duplicate the array along the first dimension
arr_dup = np.repeat(arr, 4, axis=0)

# Print the duplicated array
print(arr_dup.shape)
print(arr_dup)

#############################################

# Create your initial array
ar = np.array([[1,2,3],[7,8,9],[10,20,30],[70,80,90]])  #triplets are different colors, for each of two tex
print(ar.shape) #(4,3)

# Add a new dimension
arr_ex = np.expand_dims(ar, axis=0)
print(arr_ex.shape)  #(1,1,4,3)  #Why did it create two dimensinos rather than one?

# Repeat the values in the new dimension
repeats = len(ar)
arr_rep = np.repeat(arr_ex, repeats, axis=0)
print(arr_rep.shape)
#

#########################
bgColor = [[-1,-1,-1],[-1,-1,-1],[-1,-1,-1]]
gratingTexPix = 4
myTexThis = np.zeros([gratingTexPix,3]) + bgColor[0]
print(myTexThis)

arr_ex = np.expand_dims(myTexThis, axis=0)

# Duplicate the array along the first dimension
repeatsWanted = len(myTexThis)
arr_dup = np.repeat(arr_ex, repeatsWanted, axis=0)

# Print the duplicated array
print(arr_dup.shape)
print(arr_dup)

#
# Create your initial array
#ar = np.array([[1,2,3],[7,8,9]])  #triplets are different colors, for each of two tex
#
# Add a new dimension
#arr_ex = np.expand_dims(arr, axis=0)
#print(arr_ex.shape)
#np.array([[[1,2,3],[7,8,9]]])
#
# Repeat the values in the new dimension
#arr_rep = np.repeat(arr_expanded, 1, axis=0)
#print(arr_rep.shape)
#
#
