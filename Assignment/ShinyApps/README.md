# Understanding Difference-in-Differences Problems

The following questions aim to explore a set of interactive applications to improve understanding on the problems with using TWFE estimators. 

## Bacon Decomposition

Open up the application at https://mixtape.shinyapps.io/did_bacon/. Feel free to play around, but the following questions will guide you through the key insights. Note that the data is generated satisfying parallel trends and that each group have the same number of units.

1. Click "Homogeneous Effects". Notice that each treatment group experiences the same effect size. Run the Bacon decomposition. 

- Are any of the 4 2x2s biased? 
- Does the estimated TWFE approximately equal the true treatment effect? If not, why not?

2. Click "Heterogeneity in Levels". Notice that each treatment group experiences a different treatment effect size, but it remains constant over time. Run the Bacon decomposition. 

- Are any of the 4 2x2s biased? 
- Does the estimated TWFE approximately equal the true treatment effect? If not, why not?

3. Click "Heterogeneity in Levels with Slopes". Notice that each treatment group experiences a different treatment effect size, and the treatment effect grows over time. Run the Bacon decomposition. Note that now you see a dotted line. This is the counterfactual trend line (i.e. if treatment effects were just a level shift)

- Are any of the 4 2x2s biased? Why?
- Increase the treatment effect slope for the early treated units. Does the problem 2x2 bias get better or worse?


4. Play around and try to figure out how to make the sign of the estimated treatment effect flip. You may have to go extreme.



## Static TWFE 

Open up the application at https://mixtape.shinyapps.io/did_twfe/. Feel free to play around, but the following questions will guide you through the key insights. Note that the data is generated satisfying parallel trends and that each group have the same number of units.


1. Click "Homogeneous Effects". Notice that each treatment group experiences the same effect size. Run the TWFE Estimators.

- Is the TWFE estimate biased?
- Does the estimated TWFE approximately equal the true treatment effect? If not, why not?
- How do the "robust" estimators perform?

2. Click "Heterogeneity in Levels". Notice that each treatment group experiences a different treatment effect size, but it remains constant over time. Run the TWFE Estimators.

- Is the TWFE estimate biased?
- Does the estimated TWFE approximately equal the true treatment effect? If not, why not?
- How do the "robust" estimators perform?

3. Click "Heterogeneity in Levels with Slopes". Notice that each treatment group experiences a different treatment effect size, and the treatment effect grows over time. Run the TWFE Estimators.

- Is the TWFE estimate biased?
- Increase the treatment effect slope for the early treated units. Does the bias get better or worse?
- How do the "robust" estimators perform?

4. Play around and try to figure out how to make the sign of the estimated treatment effect flip. You may have to go extreme.


## Event-study Estimators

Open up the application at https://mixtape.shinyapps.io/did_eventstudy/.

1. Click "Homogeneous Effects". Notice that each treatment group experiences the same effect size, and it remains constant over time. Run the Event Study Estimators.

- Is the event-study biased? Is the binned event-study biased?

2. Click "Homogeneous paths". Notice that each treatment group experiences a different treatment effect size, but it grows over time. Run the Event Study Estimators.

- Is the event-study biased? Is the binned event-study biased? 
- Is your answer to the above different than the static TWFE estimator? 

3. Click "Heterogeneity in Levels". Notice that each treatment group experiences a different treatment effect size, and the treatment effect grows over time. Run the Event Study Estimators.

- Is the event-study biased? Is the binned event-study biased?

- Increase the treatment effect slopes. What happens to the bias in the event study? Do you notice a pattern to what happens to the pre-trends estimate?

