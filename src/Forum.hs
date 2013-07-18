{-# LANGUAGE DeriveDataTypeable #-}

import Data.Generics
import Data.Generics.Zipper
import Data.Generics.Validation

import Data.Functor.Compose

import System.Process
import System.Exit

data Forum = Forum
  { admins :: [User]
  , groups :: [Group]
  , topics :: [Topic]
  } deriving (Data, Typeable, Show)

data User = User
  { name :: String
  , nickname :: String
  , postcount :: Int
  , profilepicture :: File
  } deriving (Data, Typeable, Show)

data Group = Group 
  { moderator :: User
  , users :: [User]
  } deriving (Data, Typeable, Show)

data Topic = ComplexTopic
  { title :: String
  , subTopics :: [Topic]
  } 
  | SimpleTopic
  { title :: String
  , posts :: [Post]
  } deriving (Data, Typeable, Show)

data Post = Post
  { authorName :: String
  , text :: String
  , date :: String
  , attachments :: [File]
  , reply :: Maybe Post
  } deriving (Data, Typeable, Show)
  
data File = File
  { filename :: String
  , content :: String
  } deriving (Data, Typeable, Show)

antiVirusCommand = "clamavscan"
antiVirusArguments = ["--infected", "-"]

data VirusPosition = VirusPosition
  { file :: File
  , position :: Zipper Forum
  , message :: String
  }

collectViruses :: Collect IO (Maybe (File, Zipper Forum)) (IO [VirusPosition])
collectViruses s = 
  case getCompose s of
    Nothing -> return []
    Just state -> do
      (result, results) <- getCompose state
      case result of
        Nothing -> results
        Just (file, position) -> do
          (exitCode, avMsg, avErr) <- readProcessWithExitCode antiVirusCommand antiVirusArguments (content file)
          case exitCode of
            ExitSuccess -> results
            _ -> fmap ((:) (VirusPosition file position (avMsg ++ avErr))) results

findViruses = zeverything collectViruses (preorder fileQuery)
  where
    fileQuery :: GenericQ (Maybe File)
    fileQuery = mkQ Nothing Just


