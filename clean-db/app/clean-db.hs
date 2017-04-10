module Main where

import Control.Monad.IO.Class

import Database.Persist.TH

import Database.Esqueleto

import qualified Data.Text as Txt

import Template

tables "migrateAll" [persistLowerCase|
Author
	name Txt.Text
	deriving Show
|]

main :: IO ()
main = runDB migrateAll $ do
	alice <- insert $ Author "Alice"
	selectAll @Author >>= liftIO . print
	deleteAll @Author
