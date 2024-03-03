On circular versus radial
==============

Circular speed is the only component of motion for which direct support for an effect has been provided. The pseudorandom trajectories typically used in the MOT literature do have a circular component, and they also have a radial component. To many, the notion of decomposing motion into circular and radial components will be unfamiliar. In school we learn to represent a vector, such as a direction of motion, using the Cartesian axes of x (horizontal) and y (vertical). An arbitrary motion direction is then decomposed into its horizontal and vertical components. But the choice of horizontal and vertical is arbitrary. One could equally use circular (the tangent to the circle around the display center) and the radial axis, which is perpendicular to circular.
With the present state of object tracking knowledge, it is sensible to consider the circular and radial components of an object’s motion. We know that tracking has a circular speed limit, but know nothing about the effect of speed in the radial direction. 

We face two difficulties in studying the effect of speed in the radial direction.
* The distance available to array the objects, while avoiding spatial crowding, is less.
Spatial crowding has a greater extent in the radial direction
* Unlike for the circular direction, moving an object radially does not eventually bring it back to where it started. For a target that has reached the end of a radial extent, if it is to move radially, it can only move in one direction. Because the participant will likely know that, it makes the tracking task easier.

## Experiment: Modulate radius of a single ring, e.g. sinusoidally

- Modulation of radius as function of time. Radius calls function to determine its value at each time.
- Spatial modulation of radius, yielding a radial frequency pattern. Something added to radius as function of angle.

### Theory
* Under pure spatial modulation of radius, distractor traverses same locations as target. At any one time, radius might help distinguish target from distractor. Also if know trajectory, can predict future radius which helps for re-acquisition of target.
* Under pure temporal modulation of radius, can't use radius to distinguish target from distractor. Depending on modulation rate:
  * the following distractor(s) will either occupy the same region as the target or not
* What should be done with simultaneous but separable spatial and temporal modulation of radius?
* What should be done with inseparable spatial and temporal modulation of radius? 
Need to look at pictures to work this out, see 

Should also be a function of time? But if the idea is to know/determine individual objects' trajectories, then reversals will mess it up.
Is it about individual object paths or not?

### Decomposing motion into radial and circular components

- schematic_radialCircularSpeed.key contains a schematic.

## Radial motion with exchange positions with distractor 

- [ ] Have multiple radii and occasionally have corresponding objects
      in different radii exchange position
    - [ ] When they are going the same direction, that's easy. They can
          simply behave as their own double-star trajectory to exchange
          positions, added to their common circular motion. And can
          have as many as one wants doing this.
        - [ ] Perhaps should set up for enforcing a
              minimum-common-direction interval, so that can have at
              least one exchange event before they start going in
              opposite directions again.
        - [ ] What do I do when the circles are slightly out of phase?
              Define an oval trajectory with the positions of the two
              objects as end points. Have to be adjacent to each other,
              with relative phase less than half the same-trajectory
              spacing?  How eccentric should the oval be? Ideally they
              would come no closer to each other or to anyone else than
              would objects in the same trajectory.
        - [ ] For two trajectories going the same direction, can pick
              up to a large number to exchange positions. Indeed
              probably would be best to have at least 2 do it,
              otherwise too conspicuous.
    - [ ] When they are going opposite directions, it seems very
          difficult.
        - [ ] They both would have to slow down and reverse direction.
              I suppose one could leave its trajectory, gradually slow
              down and reverse direction, and then the other would jump
              out ahed or behind it so it can slide into its place. 
              But that would mean a big change in speed, so not so
              great for testing speed limits.









