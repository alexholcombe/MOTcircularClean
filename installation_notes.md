# Install pylink

I found where pylink installed with:

find /Applications -name "pylink*" 2>/dev/null

It was installed, among other places, at 
/Applications/EyeLink/SampleExperiments/Python/3.10/pylink

In the context of running psychopy using a miniconda environment called psychopy310, Claude said then try Symlink/copy to site-packages

cp -r /Applications/EyeLink/SampleExperiments/Python/3.10/pylink /opt/miniconda3/envs/psychopy310/lib/python3.10/site-packages/

That seems to have worked! Because when I did the following, it imported

> conda activate psychopy310
> import pylink