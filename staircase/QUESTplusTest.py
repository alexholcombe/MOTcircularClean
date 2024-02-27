#Code from https://github.com/psychopy/psychopy/issues/4305
import numpy as np
from questplus import QuestPlus
from questplus.psychometric_function import weibull

n_trials = 30

# create an idealized participant (weibull function)
def get_response(intensity):
    prop_corr = weibull(intensity=intensity, threshold=0.13,
                        slope=6.35, lower_asymptote=0.5, lapse_rate=0.02,
                        scale='linear').item()
    dice_roll = np.random.random()
    return int(dice_roll <= prop_corr)

# set up intensity domain and parameter space
min_coh, n_dots = 0.02, 200
coherences = np.logspace(np.log10(min_coh), np.log10(1.), num=100)
coh_dots = np.unique(np.round(coherences * n_dots))
coherences = coh_dots / n_dots

thresh = np.logspace(np.log10(0.01), np.log10(1.5), num=50)
slopes = np.logspace(np.log10(0.15), np.log10(20.), num=50)
lapses = np.arange(0., 0.06, 0.01)

stim_domain = dict(intensity=coherences)
outcome_domain = dict(response=[1, 0])
param_domain = dict(threshold=thresh, slope=slopes, lapse_rate=lapses,
                    lower_asymptote=0.5)

# create questplus "staircase"
staircase = QuestPlus(
    stim_domain=stim_domain, func='weibull', stim_scale='linear',
    param_domain=param_domain, outcome_domain=outcome_domain,
    stim_selection_method='min_entropy', param_estimation_method='mean')

# try to run it for 30 trials
for idx in range(n_trials):
    print(f'Trial {idx + 1:02d}')

    intensity = staircase.next_stim
    resp = get_response(intensity['intensity'])

    print('intensity:', intensity)
    print('response:', resp)

    staircase.update(stim=intensity, outcome=dict(response=resp))
    print('estimated params:', staircase.param_estimate)