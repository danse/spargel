import Exmargination( Margin(..), time, description, value )
import qualified Data.Text as T
import System.IO( openFile, IOMode( ReadMode ), hSetEncoding, latin1, hGetContents )
import Data.List( drop )
import qualified Data.Time.Format as F
import System.Locale( defaultTimeLocale )
import Data.Time.Clock( UTCTime )
import Data.Aeson( encode )
import Data.ByteString.Lazy.Char8( pack, unpack )

isQuote = ('"' ==)

removeQuotes :: T.Text -> T.Text
removeQuotes = T.dropAround isQuote

parseLine line  = map removeQuotes $ T.splitOn (T.pack ";") line

parseFile s =  map parseLine $ T.lines $ T.pack s

convert :: String -> [Margin]
convert = (map (toMargin . toLine)) . (drop 1) . parseFile

parseFloat :: T.Text -> Float
parseFloat t = read (T.unpack (T.replace (T.pack ",") (T.pack ".") t))

parseDate :: T.Text -> UTCTime
-- i will use it with newer versions of the library
-- parseDate = (F.parseTimeOrError True defaultTimeLocale "%g.%m.%d") . T.pack
-- the following is deprecated
parseDate = (F.readTime defaultTimeLocale "%g.%m.%d") . T.unpack

data Line = Line {
 auftragskonto :: T.Text,
 buchungstag :: UTCTime,
 valutadatum :: T.Text,
 buchungstext :: T.Text,
 verwendungszweck :: T.Text,
 beguenstigter :: T.Text,
 kontonummer :: T.Text,
 blz :: T.Text,
 betrag :: Float,
 waehrung :: T.Text,
 info :: T.Text
 } deriving (Show)

toLine :: [T.Text] -> Line
toLine [auf, bu1, val, bu2, ver, beg, kon, blz_, bet, wae, inf] = Line {
 auftragskonto = auf,
 buchungstag = parseDate bu1,
 valutadatum = val,
 buchungstext = bu2,
 verwendungszweck = ver,
 beguenstigter = beg,
 kontonummer = kon,
 blz = blz_,
 betrag = parseFloat bet,
 waehrung = wae,
 info = inf
}

toMargin :: Line -> Margin
toMargin line = Margin {
 time = buchungstag line,
 description = "",
 value = betrag line
}

main = do
  handle <- openFile "20150711-1062314251-umsatz.CSV" ReadMode
  hSetEncoding handle latin1
  contents <- hGetContents handle
  writeFile "spargel-margin-data.json" (unpack (encode (convert contents)))
