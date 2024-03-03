Neurons-per-unit-time theory
==============

- The speed limit is caused by a minimum time required to deselect neurons corresponding to old locations and select neurons corresponding to new locations.

## Speed limit = `2pi*x*e`, where `x` is the limit in rps and `e` is the eccentricity
- explains why the rps speed limit is approximately the same for higher eccentricity. The # of neurons one needs to select/deselect per degree of visual angle decreases by `2*pi` for every unit increase in deg of visual angle. In other words, neurons at 5 deg ecc are spaced 2pi further apart than are neurons at 4 deg ecc. 
- makes a prediction for what will happen when we move a circular trajectory from being centred on fixation into the periphery. When we move something from a mean eccentricity of 2.5 to a mean eccentricity of 6.7, that's an increase of 4.2 deg. So we'd have to increase the distance traveled by 8.4pi to have the same level of performance.
- By simply shifting the whole trajectory to be more eccentric, we instead didn't increase the distance traveled at all. That means that the speed limit should now be much higher. I guess it should be 8.4pi times higher.
- Speed limit in deg/sec (e) = `4*pi*e`   which is always equal to 2 rps
- E.g. if test at e=6.7, speed limit = `4*pi*6.7deg/sec`
 - If that traj has a radius of 2.5, then its path length is 5*pi.
 - So in rps, that's `(4*pi*6.7 deg/sec) / (5*pi deg/rev) ` = 5.36 rps
 - That's a prediction that's easy to falsify!
 - But a radius of 2.5 at e=6.7 could be crowded, which could be blamed for the bad speed limit.

## Crowding. Design experiment to avoid shift of trajectory into periphery introducing crowding. 
- Figure 6b of Gurnsey, Roddy & Chanab 2011 shows critical separation as a function of eccentricity and how it varies. As they say in their discussion on p. 14, 0.5 is plenty. So to be safe, let's not allow spacing to go below 0.55 of eccentricity.
- r will be `0.55*e`   Therefore, e = r/0.55
- Speed limit =`4*pi*e` 
- Substitute in r/0.55 for e
- `4*pi*r/0.55` deg per second
- In rps, `4*pi*r/0.55 / (2*pi*r)` = `2/0.55` = 3.64 rps
- In other words, shifting any trajectory as peripheral as you can before running into crowding should inflate the speed limit to 3.64 rps.
- This assumes that the average eccentricity of the displaced circle is equal to the eccentricity of its center. That's true that for a circle fully away from fixation. But I may want to have it cross the vertical and horizontal meridian as does the one centred on fixation, see next section.

## Crossing hemifields. 
If the displaced circle trajectory does not cross between hemifields, that may give it an unfair advantage (if indeed there is a cost to crossing hemifields).

We can address this by maybe moving it only partly off-center.

To predict the speed limit according to the neurons per unit time theory, we must then determine the eccentricity of each point on the circle, or at least the average eccentricity (because everything is linear, assuming all points are at the circle's average eccentricity will be equivalent).

 What is the mean eccentricity for a circle whose interior includes fixation? Its center would be equivalent to the mean eccentricity only if eccentricities on foveal side were treated as negative. But they are instead positive eccentricities.
  
- By symmetry, all that matters is the radius `r` of the circle and the distance of its center from fixation, `d` . The sum of its eccentricities is
 ![integral](https://github.com/alexholcombe/MOTcircular/blob/master/analysis/integral.png "staircase plot") and dividing that by the circumference would give the average eccentricity. The equation can be fed into WolframAlpha with [this URL](http://www.wolframalpha.com/input/?i=+integrate+%28+++%28d%2B+r+cos+%CE%B8+%29%5E2+%2B+%28r+sin+%CE%B8%29%5E2++++%29%5E0.5+d%CE%B8+from+%CE%B8%3D0+to+2pi) but it won't solve it in standard computation time and you have to pay to find out if it can ever solve it.
 - Someone asked [essentially the same question](http://math.stackexchange.com/questions/98231/is-there-a-simple-formula-for-this-simple-question-about-a-circle) on Mathematics StackExchange and only the limiting cases were solved.
 - my function calcMeanEccentricityOfCircle in theory.R approximates it in R. Then plot the average eccentricity as a function of d and r. Because near the fovea, Bouma's law again would err on side of caution. Increases with r and d. D has to be less than r to ensure that crosses the vertical meridian.
 - calcMeanEccentricityOfCircle(6, 5, .001) / 6 #1.18   An 18% boost in speed limit predicted.

## Ended up using radius=6, 
centered on eccentricity= 5 (*near* condition) and =10 (*far* condition)

See [theoryCalc.R](theoryCalc.R) for full calculations!

calcMeanEccentricityOfCircle(6, 5, .001) / 6 #1.18   An 18% boost in speed limit predicted.
 
## Kind of related to Azadi, Holcombe, Edelman saccade hypometria

Why the hypometria is greater for the larger linear speeds (as opposed to angular speeds) at larger eccentricities

This depends on the eccentricity scaling factor of the superior colliculus. If the eccentricity scaling is such that every 1-deg increase in eccentricity results in more than a 2*pi increase in SC territory stimulated by a particular deg unit area of retina, even for equivalent rps at different eccentricities. In other words, more locations will be simultaneously active for the higher linear speeds  of larger eccentricities. In that case, the idea that having more locations simultaneously active screws up the mutual inhibition could explain the greater hypometria. We talked about what the eccentricity scaling factor was before for SC but I don’t remember what your answer was.






