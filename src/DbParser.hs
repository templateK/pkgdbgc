{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP                 #-}


module DbParser
  ( (.>)
  , getPkgInfos
  , dirProducer
  , strictReadUTF8File
  , parseContents
  ) where


import           Prelude
import qualified System.Info
import qualified Config

import           Data.List
import           Data.Maybe
import           Distribution.InstalledPackageInfo
import           Distribution.ModuleName

import           System.Directory
import           System.Environment             ( getEnv )
import           System.FilePath
import           System.IO
import qualified Control.Exception             as Exc
import           GHC.Paths
import qualified Control.Exception             as Exception
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Pipes
import qualified Pipes.Prelude                 as P
import           Control.Monad
import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE


(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)
infixl 9 .>


getPkgInfos :: FilePath -> IO (Either String [InstalledPackageInfo])
getPkgInfos pkgDir = fmap sequence
  $ P.toListM
  $ dirProducer pkgDir >-> strictReadUTF8File >-> parseContents


dirProducer :: FilePath -> Producer FilePath IO ()
dirProducer pkgDir = do
  pkgDirExists <- lift $ doesDirectoryExist pkgDir
  if pkgDirExists then do
    files <- lift $ getDirectoryContents pkgDir
    let filePaths = [ pkgDir </> file | file <- files, ".conf" `isSuffixOf` file ]
    each filePaths
  else
    error $  "pkgDir does not exists: " ++ pkgDir


strictReadUTF8File :: Pipe String String IO ()
strictReadUTF8File = do
  filePath <- await
  -- lift $ print $ "(INFO) file: " ++ filePath ++ " endoing: " ++ show encoding
  content  <- lift $ B.readFile filePath
  yield . T.unpack . TE.decodeUtf8 $ content
  strictReadUTF8File


parseContents :: Pipe String (Either String InstalledPackageInfo) IO ()
parseContents = do
  content <- await
  yield $ case parseInstalledPackageInfo content of
            ParseFailed err  -> Left  $ "conf parser error: " ++ show err
            ParseOk _warns r -> Right r
  parseContents
