{-# LANGUAGE ImplicitParams, OverloadedStrings, DeriveFunctor,
RecordWildCards, DeriveFoldable, DeriveTraversable #-}

module Mugator.WebClient.GooglePlus where

-- * See the Google+ API description, available at: 
-- [developers.google.com](https://developers.google.com/+/api/latest/activities/list)

import qualified Network.HTTP.Conduit    as H
import qualified Network.HTTP.Types      as H
import qualified Data.Conduit            as C
import qualified Data.Conduit.List       as C
import qualified Data.Conduit.Binary     as C
import Data.Conduit (($$), ($$+), ($$++), ($$+-), ($=), (=$), (>+>))
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Aeson              as J
import qualified Data.Aeson.Types        as J
import qualified Data.HashMap.Strict     as M
import qualified Data.Vector             as V
import qualified Data.Text               as T
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Monoid
import Data.String
import System.FilePath ((</>))
import System.Environment (getEnv)

import qualified Data.Foldable    as D
import qualified Data.Traversable as D


newtype UserId = UserId String deriving (Show, Eq)

type URL = String

type MimeType = String

data Medium a = Audio MimeType a | Video MimeType a | Link URL
  deriving (Show, Eq, Functor, D.Foldable, D.Traversable)

data Picture a = Picture MimeType !Int !Int a
  deriving (Show, Eq, Functor, D.Foldable, D.Traversable)

type UserName = String
type UserProfile = URL
data User a = User { userId      :: UserId
                   , userName    :: UserName
                   , userProfile :: UserProfile
                   , userAvatar  :: Maybe a }
  deriving (Show, Eq, Functor, D.Foldable, D.Traversable)

data Entry a = Entry { entryPostURL   :: URL
                     , entryTitle     :: String
                     , entryCategory  :: String
                    -- , entryScore     :: !Int
                     , entryMedium    :: Medium a
                    -- , entryThumbnail :: Maybe (Picture a)
                     , entryPoster    :: User a }
  deriving (Show, Eq, Functor, D.Foldable, D.Traversable)
-- ^ TODO: add date: UTCTime is an instance of FromJSON


http' req = H.http req ?manager
setQuery req query = req { H.queryString = H.renderSimpleQuery True
                             (("key", ?apiKey):query) }

asEntryURL x = x :: Entry URL

getActivity (UserId uid) = do
  req <- H.parseUrl $ "https://www.googleapis.com/plus/v1/people/" <> uid
                      <> "/activities/public"
  H.Response {H.responseBody=res, H.responseStatus=stat} <- http' $ setQuery req
      [("fields", "id,items(access(description),actor(displayName,id,image,url),object(attachments(displayName,embed,image,objectType,url)),url),nextPageToken")]
  liftIO $ do putStr "Google+ "
              print stat
  res $$+- ((C.sinkParser J.json' >>= breakActivity) >+>
            C.map (fromString . show . asEntryURL) >+> C.sinkFile "plop")
  return ()

lk k hm = MaybeT $ return $ M.lookup (k :: T.Text) hm
tryHead v = guard (not $ V.null v) >> (return $ V.unsafeHead v)
continueOnFailure act = act `mplus` return ()
fromJValue jvalue = case J.fromJSON jvalue of
                      J.Success a -> return a
                      J.Error _   -> mzero
lkj k hm = lk k hm >>= fromJValue

breakActivity activity = runMaybeT $ do
  items <- fromJValue activity >>= lkj "items"
  V.forM_ items $ \item -> continueOnFailure $ do
    entryPostURL  <- lkj "url" item
    entryCategory <- lkj "access" item >>= lkj "description"
    attachment    <- lkj "object" item >>= lkj "attachments" >>= tryHead
    objectType    <- lkj "objectType" attachment
    guard (objectType == ("video"::String))  -- For now
    embed         <- lkj "embed" attachment
    entryMedium   <- Video <$> lkj "type" embed <*> lkj "url" embed
    entryTitle    <- lkj "displayName" attachment
    entryPoster   <- do
      actor       <- lkj "actor" item
      userId      <- UserId <$> lkj "id" actor
      userName    <- lkj "displayName" actor
      userProfile <- lkj "url" actor
      userAvatar  <- lkj "image" actor >>= lkj "url"
      return User{..}
    lift $ C.yield Entry{..}
    

metalComm = UserId "102004979259719098381"

main :: IO ()
main = do 
  homeDir <- getEnv "HOME"
  ak <- fromString <$> readFile (homeDir </> ".gplus.apikey")
  H.withManager $ \m ->
    let ?manager = m
        ?apiKey = ak in getActivity metalComm

