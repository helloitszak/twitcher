{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Prelude
import qualified Twitcher.Twitch as TW
import qualified Twitcher.Table as Table
import qualified Data.Text as T
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.List (intersperse)
import Text.Read (readEither)
import Data.Either.Combinators (mapLeft)
import Control.Exception.Base (displayException)
import System.IO
import qualified Data.Text.IO as TIO
import System.Process as Proc
import System.Info (os)
import Options.Applicative
import Data.Semigroup ((<>))

formatStreamRow :: TW.Stream -> [Table.Cell]
formatStreamRow stream = [Table.right displayName,
                          Table.center "-",
                          Table.left (T.take 50 streamTitle),
                          Table.center "-",
                          Table.left game,
                          Table.center "-",
                          Table.right viewers]
  where channel = TW.channel stream
        viewers = T.pack $ (show $ TW.viewers stream) ++ " viewers"
        game = TW.game (channel :: TW.Channel)
        displayName = TW.displayName channel
        streamTitle = TW.status channel

countCell :: Int -> Table.Cell
countCell i = Table.left $ T.pack $ (show i ++ ".")

makeTable :: [TW.Stream] -> Table.TableSet
makeTable streams = Table.TableSet cells
  where cells = mapCount mapper streams
        mapper s i = [countCell i] ++ formatStreamRow s

mapCount :: (a -> Int -> b) -> [a] -> [b]
mapCount f l = zipWith f l [1..]

consolePicker :: [TW.Stream] -> IO TW.Stream
consolePicker streams = do
  let table = makeTable streams
  TIO.putStrLn $ Table.toText " " table
  streamId <- valuePicker 1 (length streams)
  return $ streams !! (streamId - 1)

eitherRange :: Int -> Int -> Int -> Either String Int
eitherRange min max val
  | val >= min && val <= max = Right val
  | otherwise = Left "Value out of range"

valuePicker :: Int -> Int -> IO Int
valuePicker min max = do
  putStr $ "Pick (" ++ show min ++ " - " ++ show max ++ "): "
  line <- getChar
  putStrLn ""
  let value = readEither [line] :: Either String Int
  let result = mapLeft (const "Bad Input") value >>= eitherRange min max
  either (\e -> putStrLn e >> valuePicker min max) return result

firstJust :: [Maybe a] -> Maybe a
firstJust (m@(Just x):xs) = m
firstJust (Nothing:xs) = firstJust xs
firstJust [] = Nothing

clientId :: Text
clientId = "3tr1i5e8hx71iz8y29vy8udbiaerz43"

token :: Text
token = "3oocbgvfgff7y0x6m5x1bpvyk0b5kn"


openerForOs :: String -> Maybe (String, [String])
openerForOs url
  | os == "darwin" = Just $ ("open", [url])
  | os == "linux" = Just $ ("xdg-open", [url])
  | otherwise = Nothing

openerForProgram :: String -> String -> (String, [String])
openerForProgram url p = (p, [url])

data Options = Options { program :: Maybe String }


optionsParser :: Parser Options
optionsParser = Options <$>
                (optional $ strOption
                 (long "program" <>
                  short 'p' <>
                  help "Launch this instead of your OS default"))

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  
  options <- execParser $ info (optionsParser <**> helper) (fullDesc)
  streams <- TW.getFollowed clientId token

  case streams of
    Left ex ->
      do
        putStrLn "Something really bad happened: "
        putStrLn $ displayException ex
    Right response ->
      do
        stream <- consolePicker (TW.streams response)
        let url = TW.url (TW.channel stream)

        let urlStr = (T.unpack url)
        let opener = firstJust [
              openerForProgram urlStr <$> program options,
              openerForOs urlStr]
              
        maybe
          (putStrLn "OS Not Supported")
          (uncurry callProcess)
          opener
          
        return ()
       
