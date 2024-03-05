import pandas as pd
import pylab, os
# Replace 'file_path.csv' with the path to your CSV file.
df = pd.read_csv('some_data.tsv',delimiter='\t')

# Print the DataFrame to check if the data has been loaded correctly.
print(df)

grouped_df = df.groupby(['speedThisTrial', 'numTargets', 'numObjectsInRing']).agg(
    pctCorrect=('correctForFeedback', 'mean'),
    n=('correctForFeedback', 'count')
)

#grouped_df = df.groupby(['speedThisTrial', 'numTargets', 'numObjectsInRing'])['correctForFeedback'].mean()

grouped_df = grouped_df.reset_index()

print(grouped_df)

# plot curve
pylab.subplot(122)
pylab.xlabel("speed (rps)")
pylab.ylabel("Percent correct")

# plot points
pointSizes = pylab.array(grouped_df['n']) * 5  # 5 pixels per trial at each point
points = pylab.scatter(grouped_df['speedThisTrial'], grouped_df['pctCorrect'], s=pointSizes,
    edgecolors=(0, 0, 0), facecolor=(1, 1, 1), linewidths=1,
    zorder=10,  # make sure the points plot on top of the line
    )

pylab.ylim([0, 1])
pylab.xlim([0, None])
# save a vector-graphics format for future
dataFolder='.'
outputFile = os.path.join(dataFolder, 'last.pdf')
pylab.savefig(outputFile)
print('saved figure to: ' + outputFile)
pylab.show()

# pylab.plot(smoothInt, smoothResp, 'k-')
# pylab.plot([thresh, thresh], [0, threshVal], 'k--')  # vertical dashed line
# pylab.plot([0, thresh], [threshVal, threshVal], 'k--')  # horizontal dashed line
# pylab.title('threshold (%.2f) = %0.3f' %(threshVal, thresh))


core.quit()
