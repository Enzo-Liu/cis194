module IO05
  (

  )
where
import           Data.ByteString.Lazy (ByteString)
import           Data.List
import           Data.Map.Strict      (Map)
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Word
import           System.Environment   (getArgs)

import           Control.Applicative
import           Data.Bits
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict      as Map

import           Parser

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
isIn t = Set.member (tid t)

exclude :: [TId] -> [Transaction] -> [Transaction]
exclude td = filter (`isIn` s)
  where s = Set.fromList td

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs vf tf = do
  vd <- parseFile vf
  td <- parseFile tf
  return $ liftA2 exclude vd td

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow [] = Map.empty
getFlow (Transaction from to amount _:xs) = Map.insertWith (+) from (-amount) $ Map.insertWith (+) to amount $ getFlow xs

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal mp = head . Map.keys $ Map.filter (== m) mp
    where m = maximum $ Map.elems mp

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs mp = go payer payee
  where (payerMap,payeeMap) = Map.partition (<0) $ Map.filter (/=0) mp
        order = sortBy (\(_,v1) (_,v2)-> compare v2 v1) . Map.toList
          :: Map String Integer -> [(String,Integer)]
        payer =  order payerMap
        payee =  order payeeMap
        go _ [] _ = []
        go [] _ _ = []
        go _ _ [] = []
        go ((pr,m):xs) ((pe,n):ys) (t:ts) = let s = m + n in case compare s 0 of
          GT -> Transaction pe pr (-m) t : go xs ((pe,s):ys) ts
          EQ -> Transaction pe pr n t : go xs ys ts
          LT -> Transaction pe pr n t : go ((pr,s):xs) ys ts



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
