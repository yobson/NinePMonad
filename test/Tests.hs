{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}
{-# LANGUAGE TypeApplications #-}

module Tests (tests) where

import Distribution.TestSuite (Test)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck hiding (property)
import Data.String
import Control.Applicative
import Data.Word

import Network.NineP.Server

tests :: IO [Test]
tests = return
  [ getPropertyTest PropertyTest 
    { name = "URLs are parsed properly 1"
    , tags = []
    , property = bindAddrShowFromStringId
    }
  , getPropertyTest PropertyTest
    { name = "URLs are parsed properly 2"
    , tags = []
    , property = bindAddrFromStringShowId
    }
  ]

bindAddrShowFromStringId :: BindAddr -> Bool
bindAddrShowFromStringId x = x == fromString (show x)

bindAddrFromStringShowId :: Plan9String -> Bool
bindAddrFromStringShowId (Plan9String x) = x == show (fromString @BindAddr x)

newtype FilePathString = FilePathString { getFilePathString :: String }
newtype HostString     = HostString     { getHostString     :: String }
newtype Plan9String    = Plan9String    { getPlan9String    :: String }
  deriving Show

genLower = chooseEnum ('a', 'z')

genUpper = chooseEnum ('A', 'Z')

genNumber = chooseEnum ('0', '9')

genHostSymbols = elements ['/', ':', '.', '@']

genPathSymbols = elements ['/', '.', '~']

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


instance Arbitrary Plan9String where
  arbitrary = do
    con <- arbitrary
    if con
       then Plan9String <$> fmap ("unix!" <>) (getFilePathString <$> arbitrary)
       else do
         port <- arbitrary @Word16
         if port == 564
            then Plan9String <$> fmap ("tcp!" <>) (getHostString <$> arbitrary)
            else Plan9String <$> fmap (\host -> "tcp!" <> host <> show port) (getHostString <$> arbitrary)
         
