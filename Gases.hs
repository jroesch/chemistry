module Chemistry.Gases where

import Data.List (sortBy)

boltzmann = 1.3806488e10-23

kineticEnergyOf t = (3/2)*boltzmann*t

rankEnergy ts = 
    let pairs = zip ts (map kineticEnergyOf ts)
        comp e1 e2 = compare (snd e1) (snd e2)
      in sortBy comp pairs


