import pandas as pd
import matplotlib.pyplot as plt

# Initialize a DataFrame with x and y values
df = pd.DataFrame({
    'x': [1,1, 2, 3, 4, 5],
    'y': [2,3, 3, 5, 7, 11]
})

grouped_df = df.groupby(['x']).agg(
    meanY=('y', 'mean'),
    n=('y', 'count')
)
grouped_df = grouped_df.reset_index()

# Plot it using matplotlib
plt.plot(grouped_df['x'], grouped_df['meanY'], marker='o')
plt.xlabel('x')
plt.ylabel('y')


xs = df[['x']]
print('xs=',xs,'type=',type(xs))
#Below is the problem, maxX ends up being a string, because 
# that's the highest value in how it treats a named list, which includes a string for the column name
#maxX = max(xs)
maxX = df['x'].max()
print('maxX=',maxX, 'type=',type(maxX))
plt.plot([0, maxX], [5,5], 'k--')  # horizontal dashed line

plt.show()