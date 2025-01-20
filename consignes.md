1. Form a group of two persons max (you can work on your own as well).

2. Use the enclosed dataset on public transport in France (same as the data used in the papers I presented).

3. **Estimate a cost function in order to test empirically the effect of an incentive contract on the cost of the operators.** As the choice of contract is endogenous, you will probably find an effect that is counterintuitive. You can use the enclosed papers to get ideas to deal with endogeneity. The first one ("Piechucka") is based on a reduced form and uses an easier approach. You can try to estimate a translog cost function in this case, which is an interesting exercise. The second one is based on the structural model I presented and is more complicated to implement. Feel free to choose the method you prefer.

4. The list of variables is described below + there is a series a questions/answers that are useful to understand how the dataset is constructed. You can also refer to the two papers enclosed.

Let me know if you have any question.

----------

NUM: Firm id

TRANS - AGIR - CONNEX - KEOLIS : industrial group who owns the operator

INCENT: =1 if incentive contract (fixed-price), 0 otherwise.

RIGHT: =1 if right wing municipality

PUBLIC: =1 if public firm, 0 otherwise.

NCITIES: # of cities in the urban network.

POPU: Population of the urban network.

LINES: # of lines.

LENGHT: total lenght of network.

PARC: # of vehicles.

EMPLOY: # of employees

DRIVERS: # of drivers

PKO: # of seats kilometers (this is your supply variable)

LABOR: Labor costs.

COSTS: Total costs.

GASI: Gas Index

-----

Questions/Answers:

Firstly, I presume we can use the Gas Index as the Material Price. Yet, it may be insufficient to recover the overhead costs simply by Total costs - Labor costs as we do not know how much fuels firms really used.

Yes -- Indeed, you cannot recover overhead costs as total cost include many different items.


Should we then ignore overhead costs?


Yes


If so, it may not be possible to follow the authors' approach to use a restricted transcendental log cost function and impose a normalisation for homogeneity of degree 1 in input prices.


You can estimate  a translog, but you will not be able to add the input cost share equations (which is not a problem). Yes you must impose the homo of degree 1 in all the input prices you include in your equation.


May we use the Cobb-Douglas cost function from the lectures instead, with the contract choice variable being endogenised?


You should try the translog.

Secondly, to recover the wage rate, should we use LABOR/EMPLOY or LABOR/(EMPLOY+DRIVERS) as it does not seem like the number of employees already include drivers, and if so, is it reasonable if we sum them up like this, which would then assume them to have same wages? Or should we control the percentage of drivers ?


Use EMPLOY as they include drivers.


Finally, should we also include in the cost function the variable VERSE which I presume to be the soft capital costs as it's not included in the list of variables you indicate?


No, this is a subsidy which is useless for your homework.

Actually I only want city fixed effects and I want to include a YEAR variable in the regressions to account for the trend