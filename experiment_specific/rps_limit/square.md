Circular speed the limiting factor?
==============

## Experiment: Diamond vs. circle

- Adding modulations to the trajectory has little effect, in other words rotational speed is the determinant.  
- First, blast the annoying speed limit people with a square trajectory. This finally helps answer those interested in linear speed limit
  - Square as function of theta goes faster away from the circle. Need to have slower speed, the more the direction deviates from the circle.
  - The angle could be reinterpreted for square as distance that needs to travel along the trajectory, however for it to equal the same speed it would have to get up to more than 360. So, reinterpretation can scale to 360 but then speed needs to be proportionately higher for the square trajectory if want to compare on rps basis.
- Compare square trajectory to circular at two eccentricities. Got some data for me with 1 target, but then remembered I'd like to use two targets whenever on center to avoid being blur-limited.

### Diamonds calculations
- visualisation of square and circle https://twitter.com/rabihalameddine/status/634491712149291009
-  What is the average eccentricity of a diamond? Think of it as a square, which is fine thanks to rotational symmetry. Actually this doesn't come up currently in the Methods.
- I am using constant speed in dva, not constant rps. Therefore worked out from theta how far along trajectory should be (theta/360) and calculated length of the four sides to calculate which side I should be on and how far along it.
- Seems it circumscribed the circle because used a unit square and then multiplied by radius. So the perimeter instead of being 2*pi*2.5 = 15.7 deg, was 2*2.5*4= 20 deg.
  - Therefore the average speed in dva per second was 20/15.7 = 1.27 or 27% faster. So its speed limit should be 27% slower if the limit was set by dva per second.
- Seems should compare null hypothesis of same speed limit to alternative hypothesis of circle being 27% faster. Of course we already know that it's not a dva per second speed limit because it's not affected by eccentricity, so I'm not sure it's worth testing. What we should be thinking about instead is whether there is a radial speed limit. And for that I need to calculate the radial speed at each moment.
  - Consider just one quadrant. By symmetry same holds for others.
  - At only one point, the point tangent to the circle, does it have zero radial speed. At other points, break down the vector into tangent component and radial component.

### Results
- Critical issue is speed limit of smaller-radius one when not concentric. Smaller one is always ringToQuery=0. Speed limit for me is lower by about 0.4rps.
- 18 August have 11 participants in total. One (anonymised as "NS") thrown out for fixation failure in >40% of trials. Two more, including me, thrown out for not having eyetracking data.
- To-do. Get quickpsy plot of data to work even when psychometric function is flat.


