{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Tests (tests) where

import Distribution.TestSuite (Test)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck hiding (property)
import Data.String
import Control.Applicative

import Network.NineP.Server

tests :: IO [Test]
tests = return
  [ getPropertyTest PropertyTest 
    { name = "URLs are parsed properly"
    , tags = []
    , property = bindAddrShowFromStringId
    }
  ]

bindAddrShowFromStringId :: BindAddr -> Bool
bindAddrShowFromStringId x = x == fromString (show x)

newtype FilePathString = FilePathString { getFilePathString :: String }
newtype HostString     = HostString     { getHostString     :: String }

genLower = chooseEnum ('a', 'z')

genUpper = chooseEnum ('A', 'Z')

genNumber = chooseEnum ('0', '9')

genHostSymbols = elements ['/', ':', '.', '@']

genPathSymbols = elements ['/', '.']

genBoringChar = frequency [(5,genLower), (5,genUpper), (2, genNumber)]

genHostChar = frequency [(5,genBoringChar), (1,genHostSymbols)]

genPathChar = frequency [(5,genBoringChar), (1,genPathSymbols)]

instance Arbitrary FilePathString where
  arbitrary = FilePathString <$> listOf genPathChar

instance Arbitrary HostString where
  arbitrary = HostString <$> listOf genHostChar

instance Arbitrary BindAddr where
  arbitrary = do
    con <- arbitrary
    if con
       then UnixDomain . getFilePathString <$> arbitrary
       else liftA2 Tcp (getHostString <$> arbitrary) arbitrary
