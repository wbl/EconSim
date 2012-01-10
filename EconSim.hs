import Data.List
import Numeric.GSL.Minimization
--Econ Simulator, version 0.0.1
--by Watson Ladd
--We will use a simple model in which all agents have a Cobb Douglas utility
--function and a simple budget constraint, then find the price vector at which
--all given commodities are consumed.
data Agent = Agent Double [Double]
budget:: Agent->Double
budget (Agent budget exps)=budget

exponents :: Agent->[Double]
exponents (Agent budget exps)=exps
---Solution from wikipedia
--Given a single agent, they optimize their  consumption
--subject to the price constraints.
demand :: Agent->[Double]->[Double]
demand (Agent budget exps) prices = map(\x->budget*x)(zipWith (/) exps prices)

allFunc :: (Agent->[Double]->[Double])->[Agent]->[Double]->[[Double]]
allFunc func populace prices = map (\x-> func  x prices) populace
---Sum up the demands
sumMerge :: [[Double]]->[Double]
sumMerge = map (\x-> (foldr (+) 0 x)) . transpose
-- Total demand at a set of prices for the commodities
totFunc :: (Agent->[Double]->[Double])->[Agent]->[Double]->[Double]
totFunc func people prices = sumMerge  (allFunc func  people prices)

totDemand :: [Agent]->[Double]->[Double]
totDemand = totFunc demand
--Now we come to the tricky part. See, if I was clever I would have
--been writing functions to get the gradient of the utility functions
--and ultimately of what we need to minimize, the waste.
wasteVec :: [Agent]->[Double]->[Double]->[Double]
wasteVec agents resources prices = (zipWith (-) (totDemand agents prices)
                                   resources)
waste :: [Agent]->[Double]->[Double]->Double
waste a r p= foldr (+) 0 (map (\x->x*x) (wasteVec a r p))
