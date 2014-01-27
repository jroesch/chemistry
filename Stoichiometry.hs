{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Stoichiometry where

import Control.Applicative
import Control.Lens
import Data.Aeson

data Element = Element
             { _group  :: String
             , _name :: String
             , _number :: Int
             , _symbol :: String
             , _molarMass :: Double
             , _electrons :: [Int]
             } deriving (Eq, Show)

instance FromJSON Element where
  parseJSON (Object v) = Person <$>

makeLenses ''Element

-- Use TH to generate them all
newtype PeriodicTable = PT { getMap :: Map String Element }

periodicTable :: PeriodicTable
composition :: 
