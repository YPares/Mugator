{-# LANGUAGE ImplicitParams, OverloadedStrings, DeriveFunctor,
RecordWildCards, DeriveFoldable, DeriveTraversable #-}

module Mugator.WebClient.Types where

import qualified Data.Text               as T
import qualified Data.Foldable    as D
import qualified Data.Traversable as D


newtype UserId = UserId T.Text deriving (Show, Eq)

type URL = T.Text

type MimeType = T.Text

data Medium a = Audio !MimeType a | Video !MimeType a | Link !URL
  deriving (Show, Eq, Functor, D.Foldable, D.Traversable)

data Picture a = Picture MimeType !Int !Int a
  deriving (Show, Eq, Functor, D.Foldable, D.Traversable)

type UserName = T.Text
type UserProfile = URL
data User a = User { userId      :: !UserId
                   , userName    :: !UserName
                   , userProfile :: !UserProfile
                   , userAvatar  :: Maybe a }
  deriving (Show, Eq, Functor, D.Foldable, D.Traversable)

data Entry a = Entry { entryPostURL   :: !URL
                     , entryTitle     :: !T.Text
                     , entryCategory  :: !T.Text
                    -- , entryScore     :: !Int
                     , entryMedium    :: Medium a
                    -- , entryThumbnail :: Maybe (Picture a)
                     , entryPoster    :: User a }
  deriving (Show, Eq, Functor, D.Foldable, D.Traversable)
-- ^ TODO: add date: UTCTime is an instance of FromJSON

