{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import GHC.Generics

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (pack)
import Network
import System.Exit
import System.IO
import Text.Printf

-- JSON Config
data Config = Config
    { server :: String
    , port :: Int
    , channel :: String
    , nick :: String
    , nsPass :: String
    , modes :: String
    } deriving (Show, Generic)

instance FromJSON Config

-- State Record
data MainState = MainState
    { msHandle :: Handle
    , msConfig :: Config
    , msCResp :: [String]
    , msNResp :: [String]
    }
    
-- Empty String List
eSL :: [String]
eSL = []

-- Config FilePath
confFile :: FilePath
confFile = "config.json"

-- Function to read the JSON config file
readConf :: IO B.ByteString
readConf = B.readFile confFile

-- Function to send messages to the server
hWrite :: Handle -> String -> IO ()
hWrite h s = do
    hPutStrLn h s
    putStrLn $ ">:" <> s

-- Function to read messages from the server
hRead :: Handle -> IO String
hRead h = do
    s <- hGetLine h
    putStrLn $ "<:" <> s
    return s

-- Function called by main
startUp :: IO ()
startUp = do
    -- Decode the JSON File
    j <- (eitherDecode <$> readConf) :: IO (Either String Config)
    case j of 
        -- Case Decoding the File Failed
        Left err -> putStrLn err
        -- Case it worked
        Right js -> connectIRC js

-- Function to connect to IRC
connectIRC :: Config -> IO ()
connectIRC js = do
    h <- connectTo (server js) (PortNumber (fromIntegral (port js)))
    hSetBuffering h LineBuffering
    hWrite h $ "NICK " <> nick js
    hWrite h $ "USER " <> nick js <> " 0 * :" <> nick js
    firstPing h
    identify h js
    hWrite h $ "JOIN " <> channel js
    hWrite h $ "WHO " <> channel js
    mainLoop (MainState {msHandle = h, msConfig = js, msCResp = eSL, msNResp = eSL})

-- Function which waits for the first ping
firstPing :: Handle -> IO ()
firstPing h = do
    s <- hRead h
    if | "PING" `isPrefixOf` s -> hWrite h $ "PONG " <> words s !! 1
       | otherwise -> firstPing h

-- Function which identifies with nickserv
identify :: Handle -> Config -> IO ()
identify h js = do
    s <- hRead h
    if | "NickServ" `isInfixOf` s -> hWrite h ("NS IDENTIFY " <> nsPass js)
       | otherwise -> identify h js

-- The Main Loop that the program always returns to.
mainLoop :: MainState  -> IO ()
mainLoop ms@MainState{..} = do
    -- Fetch a line from the server
    s <- hRead msHandle
    -- If the line is shorter than 2 words, fetch another, otherwise handle it.
    if length (words s) < 2
    then mainLoop ms
    else handleInput ms s

handleInput :: MainState -> String -> IO ()
handleInput ms@MainState{..} s = do
    -- Set cmd to equal the second word, which is the IRC command
    let cmd = words s !! 1
    -- Set n to equal the nick who sent the command
    let n = tail $ takeWhile (/= '!') (words s !! 0)
    let w = words s
    v <- if (length w) > 4
        then isVersionResp ms s
        else (return False)
    if | "PING" `isPrefixOf` s -> handlePing ms s
       | n == nick msConfig -> mainLoop ms
       | cmd == "JOIN" -> handleJoin ms s
       | cmd == "352" -> handleWho ms s
       -- When the server finishes sending WHO command data, switch CResp to
       -- the latest who list.
       | cmd == "315" -> mainLoop (ms { msCResp = msNResp, msNResp = eSL })
       | length (words s) < 4 -> mainLoop ms
       | v -> sendNotice ms s
       -- Command didn't match what we were looking for
       | otherwise -> mainLoop ms

-- Function which checks to see if the message is a version response
isVersionResp :: MainState -> String -> IO Bool
isVersionResp ms@MainState{..} s = do
    let w = words s
    let c = w !! 1
    let n = w !! 2
    let m = w !! 3
    if c == "NOTICE" && n == nick msConfig && m == ":\001VERSION"
        then return True
        else return False

-- Function to handle a 352 response
handleWho :: MainState -> String -> IO ()
handleWho ms@MainState{..} s = do
    let n = words s !! 7
    let m = words s !! 8
    if any (\x -> x `elem` m) (modes msConfig)
        then mainLoop (ms { msNResp = n:msNResp })
        else mainLoop ms

-- Function to handle a PING
handlePing :: MainState -> String -> IO ()
handlePing ms@MainState{..} s = do
    hWrite msHandle $ "PONG " <> words s !! 1
    -- Send a WHO when we get pinged to keep the nick list up to date
    hWrite msHandle $ "WHO " <> channel msConfig
    mainLoop ms

-- Function to handle when a client joins the channel
handleJoin :: MainState -> String -> IO ()
handleJoin ms@MainState{..} s = do
    let n = tail $ takeWhile (/= '!') (words s !! 0)
    -- Send the CTCP VERSION
    hWrite msHandle $ "PRIVMSG " <> n <> " :\001VERSION\001"
    mainLoop ms

-- Notice people with channel modes about a CTCP response
sendNotice :: MainState -> String -> IO ()
sendNotice ms@MainState{..} s = do
    let n = tail $ takeWhile (/= '!') (words s !! 0)
    let (_, c) = splitAt 4 $ words s
    -- Send a message to everyone in channel with modes
    mapM_ (\x -> hWrite msHandle ("NOTICE " <> x <> " :" <> n <> " CTCP Version responded: " <> (unwords c))) msCResp
    mainLoop ms