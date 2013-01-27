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

import qualified Data.Foldable    as D
import qualified Data.Traversable as D


newtype UserId = UserId T.Text deriving (Show, Eq)

type URL = T.Text

type MimeType = T.Text

data Medium a = Audio MimeType a | Video MimeType a | Link URL
  deriving (Show, Eq, Functor, D.Foldable, D.Traversable)

data Picture a = Picture MimeType !Int !Int a
  deriving (Show, Eq, Functor, D.Foldable, D.Traversable)

type UserName = T.Text
type UserProfile = URL
data User a = User { userId      :: UserId
                   , userName    :: UserName
                   , userProfile :: UserProfile
                   , userAvatar  :: Maybe a }
  deriving (Show, Eq, Functor, D.Foldable, D.Traversable)

data Entry a = Entry { entryPostURL   :: URL
                     , entryTitle     :: T.Text
                     , entryCategory  :: T.Text
                    -- , entryScore     :: !Int
                     , entryMedium    :: Medium a
                    -- , entryThumbnail :: Maybe (Picture a)
                     , entryPoster    :: User a }
  deriving (Show, Eq, Functor, D.Foldable, D.Traversable)
-- ^ TODO: add date: UTCTime is an instance of FromJSON


http' req = H.http req ?manager
setQuery req query = req { H.queryString = H.renderSimpleQuery True
                             (("key", fromString ?apiKey):query) }

getActivity (UserId uid) = do
  req <- H.parseUrl $ "https://www.googleapis.com/plus/v1/people/" <> T.unpack uid
                      <> "/activities/public"
  H.Response {H.responseBody=res, H.responseStatus=stat} <- http' $ setQuery req
      [("fields", "id,items(access(description),actor(displayName,id,image,url),object(attachments(displayName,embed,image,objectType,url)),url),nextPageToken")]
  liftIO $ do putStr "From Google+: "
              print stat
  res $$+- ((C.sinkParser J.json' >>= breakActivity) >+>
            C.map entryToBS >+> C.sinkFile "entries.txt")
  return ()
  where entryToBS entry = fromString . (<> "\n") . show $ (entry :: Entry URL)

tryHead v = guard (not $ V.null v) >> (return $ V.unsafeHead v)
continueOnFailure act = act `mplus` return ()
fromJValue jvalue = case J.fromJSON jvalue of
                      J.Success a -> return a
                      J.Error _   -> mzero
lk k hm = MaybeT (return $ M.lookup (k :: T.Text) hm) >>= fromJValue

breakActivity jActivity = runMaybeT $ do
  activity      <- fromJValue jActivity
  nextPageToken <- lk "nextPageToken" activity
  items         <- lk "items" activity
  V.forM_ items $ \item -> continueOnFailure $ do
    entryPostURL  <- lk "url" item
    entryCategory <- lk "access" item >>= lk "description"
    attachment    <- lk "object" item >>= lk "attachments" >>= tryHead
    objectType    <- lk "objectType" attachment
    guard (objectType == ("video"::T.Text))  -- For now
    embed         <- lk "embed" attachment
    entryMedium   <- Video <$> lk "type" embed <*> lk "url" embed
    entryTitle    <- lk "displayName" attachment
    entryPoster   <- do
      actor       <- lk "actor" item
      userId      <- UserId <$> lk "id" actor
      userName    <- lk "displayName" actor
      userProfile <- lk "url" actor
      userAvatar  <- lk "image" actor >>= lk "url"
      return User{..}
    lift $ C.yield Entry{..}
  return (nextPageToken :: T.Text)

