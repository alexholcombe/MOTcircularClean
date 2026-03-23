I found where pylink installed with:

find /Applications -name "pylink*" 2>/dev/null

It was installed, among other places, at 
/Applications/EyeLink/SampleExperiments/Python/3.10/pylink

# Claude said then try Option 2: Symlink/copy to site-packages
cp -r /Applications/EyeLink/SampleExperiments/Python/3.10/pylink /opt/miniconda3/envs/psychopy310/lib/python3.10/site-packages/
