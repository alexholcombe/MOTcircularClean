network theory of speed and t.f. limit?
==============

In the Oscillator Model (details below), it takes time to shift all the oscillators' activity to the new location, causing a *speed limit*. Normally it's ok if some oscillators are lingering on the old location. But if a distractor invades that space before those old locations are desynchronized (*tf limit*), well the oscillators have a tendency to synchronise with any stimulus that meets the current featural attention settings, so some will get stuck on it and tracking has a higher chance of failing.

In the spiking model (details below), the pointer field is updated at a particular frequency, like 10 Hz (blinking spotlight). So if the target leaves the pointer field and is replaced by a distractor before the next sampling, then model ends up tracking the distractor. The effect of load: more targets means dividing the updating accordingly.

# Borisyuk et al. (2008, Neural Network World) models technical notes

## Oscillator model

![KazanovichBorisyuk2006](KazanovichBorisyuk2006.png
 "KazanovichBorisyuk2006")
 
The peripheral oscillators, retinotopic.

Central oscillator (CO) has feedforward and feedback connections to the peripheral oscillators (PO).

PO can be silent, active, or resonant. When resonant, included in focus of attention.

Phase-locking: 

### Neural basis

- It is assumed that POs represent cortical columns and are constituted
  of locally interacting populations of excitatory and inhibitory
  neurons of the cortex. The CO represents the central executive of the
  attention system that is implemented by a distributed network in the
  prefrontal cortex and the septo-hippocampal system. 

## Spiking (Hodgkin-Huxley) element model

![BorisyukNetwork](BorisyukEtAl2008Fig4.png
 "")

- Cognitive control module represents attention pointer fields, larger than each object
  - As the target moves, the location of the pointer field will be updated to catch up with the target. The updating process is assumed to be discrete (for example 10Hz). 
  - The new location of the pointer field is calculated by averaging the coordinates of active pixels inside the pointer field.
- Attention formation module implements partial synchronization of "attended" neurons
  - a single “central neuron” (CN) inhibits distracters and synchronises the spikes of attended peripheral neurons. CN receives excitatory synaptic inputs from all peripheral neurons. Each PN receives strong inhibitory synaptic currents from the CN, and weak local excitatory synaptic currents from 8 neighbouring neurons.
  - neurons corresponding to the attended object have higher natural frequencies. They will be synchronised by the system. 

# Questions

- How to show load effect is temporally specific and not all performance falling due to less attention?
