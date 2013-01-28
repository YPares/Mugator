{-# LANGUAGE ImplicitParams, OverloadedStrings #-}

module Main where

import qualified Network.HTTP.Conduit    as H
import System.FilePath ((</>))
import System.Environment (getEnv)

import Mugator.WebClient.Types
import Mugator.WebClient.GooglePlus


metalComm = UserId "102004979259719098381"

main :: IO ()
main = do
  homeDir <- getEnv "HOME"
  ak <- readFile (homeDir </> ".gplus.apikey")
  H.withManager $ \m ->
    let ?manager = m
        ?apiKey = ak in getActivity metalComm

