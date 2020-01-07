{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Time
import Data.Typeable
import System.Environment

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as CL
import Network.HTTP.Conduit

import Text.HTML.TagSoup

data BookInfo = BookInfo {
    biDay :: Day,
    biName :: ByteString
}

data EmptyInfoException = EmptyInfoException
    deriving (Typeable, Show)

instance Exception EmptyInfoException

today :: IO Day
today = do
    time <- getCurrentTime
    zone <- getCurrentTimeZone
    return $ localDay $ utcToLocalTime zone time

strToDay :: String -> Maybe Day
strToDay = parseTimeM True defaultTimeLocale "%Y/%m/%d"

parseArgs :: [String] -> Maybe Day
parseArgs (a:as) = strToDay a
parseArgs [] = Nothing

url :: Int -> String
url n = "http://wawabook.com.tw/comic/0101.php?pagea=" ++ show n

parsePage :: ByteString -> [BookInfo]
parsePage p =
    let tbl = dropWhile (~/= tagTable) $ parseTags p
        rows = init $ partitions (~== tagTd) tbl
        fields = map (tail . map ((!! 1)) . sections (== TagOpen "td" [])) rows
        s2d = fromJust . strToDay . B.unpack . B.takeWhile (/= '(') . fromTagText
    in map (\[d, _, _, n, _, _] -> BookInfo (s2d d) (fromTagText n)) fields
  where
    tagTable = TagOpen "table" [("id", "comid0")] :: Tag ByteString
    tagTd = TagOpen "td" [("background", "/images/dot3.gif")] :: Tag ByteString

getBooks :: (MonadThrow m, MonadResource m) => ConduitT () BookInfo m ()
getBooks = do
    manager <- liftIO $ newManager tlsManagerSettings
    let fetch n = do
            liftIO $ threadDelay 500000
            req <- parseUrlThrow (url n)
            http req manager >>= ($$+- sinkBody) . sealConduitT . responseBody
        sinkBody = CL.fold B.append B.empty
        fetchInfo n = do
            p <- fetch n
            let info = parsePage p
            if null info
                then throwM EmptyInfoException
                else return info
    CL.sourceList [1..] .| CL.concatMapM fetchInfo

printBooks :: Day -> IO ()
printBooks day = runResourceT $ runConduit $ getBooks .| printCurDay
  where
    printCurDay = do
        next <- await
        case next of
            Just b -> case compare (biDay b) day of
                        LT -> return ()
                        EQ -> liftIO (printBookInfo b >> putChar '\n') >> printCurDay
                        GT -> printCurDay
            _ -> return ()
    printBookInfo (BookInfo d n) = putStr (show d) >> putChar '\t' >> B.putStr n >> putChar '\n'

main = do
    args <- getArgs
    case parseArgs args of
        Just day -> printBooks day
        Nothing -> today >>= printBooks
