{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Stoichiometry where

import Control.Applicative
import Control.Lens
import Data.Aeson

import qualified Data.Map as M
import Data.Vector (toList)
import Control.Monad

slope (x, y) (x', y') = (x - x')/(y - y')

data Element = Element
             { _group  :: String
             , _name :: String
             , _number :: Int
             , _symbol :: String
             , _molarMass :: Double
             , _electrons :: [Int]
             } deriving (Eq, Show)

-- Use TH to generate them all
newtype PeriodicTable = PT { getMap :: M.Map String Element }

instance FromJSON PeriodicTable where
    parseJSON (Object v) = do
      table <- v .: "table"
      lanthanides <- v .: "lanthanoids"
      actinides <- v .: "actinoids"
      elems <- case table of
        Array v -> return $ map parseJSON $ toList v
        _       -> mzero       
      return $ PT $ M.fromList $ zip (map show [1..]) $ elems

instance FromJSON Element where
  parseJSON (Object v) = undefined

makeLenses ''Element

-- sigfig :: Int -> Double -> Double
sigfig n x = (take n mantissa) ++ exp
    where str = show x
          (mantissa, _:exp) = span (/= 'e') str
          

avogadros :: Double
avogadros = 6.0221413e+23

-- periodicTable :: PeriodicTable
-- composition :: 
--

composition xs = map (/min) xs
  where min = minimum xs

