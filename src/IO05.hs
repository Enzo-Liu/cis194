module IO05
  (

  )
where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Word

import Control.Applicative
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import Data.Bits

import Parser

-- Exercise 1 -----------------------------------------

zipToBS :: (Word8->Word8->Word8) -> ByteString -> ByteString -> ByteString
zipToBS f a b = BS.pack $ BS.zipWith f a b

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret f1 f2 = do
  fs1 <- BS.readFile f1
  fs2 <- BS.readFile f2
  return $ BS.pack .filter (/= 0)$ BS.zipWith xor fs1 fs2


-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
  fs <- BS.readFile $ path++".enc"
  BS.writeFile path $ zipToBS xor (BS.cycle key) fs


-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile p = decode <$> BS.readFile p

-- Exercise 4 -----------------------------------------
isIn :: Transaction -> Set TId -> Bool
isIn t s = Set.member (tid t) s

exclude :: [TId] -> [Transaction] -> [Transaction]
exclude td ts = filter (`isIn` s) ts
  where s = Set.fromList td

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs vf tf = do
  vd <- parseFile vf
  td <- parseFile tf
  return $ liftA2 exclude vd td

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow [] = Map.empty
getFlow ((Transaction from to amount _):xs) = Map.insertWith (+) from (-amount) $ Map.insertWith (+) to amount $ getFlow xs

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal mp = head . Map.keys $ Map.filter (== m) mp
    where m = maximum $ Map.elems mp

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs _ _= []

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON f = BS.writeFile f . encode

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "./resources/dog-original.jpg"
                        "./resources/dog.jpg"
                        "./resources/transactions.json"
                        "./resources/victims.json"
                        "./resources/new-ids.json"
                        "./resources/new-transactions.json"
  putStrLn crim
