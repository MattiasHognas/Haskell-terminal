
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Graphics.Vty as V
import Control.Concurrent
import Control.Monad (forever, when, void)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Simple
import GHC.Generics
import Data.Aeson
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (transpose, elemIndex)
import System.Process (callCommand)
import qualified Data.HashMap.Strict as HM
import System.Directory (getModificationTime)
import Data.Time.Clock (UTCTime)

type Name = ()
type Cell = (String, Maybe String)
type Row = [Cell]

data TablePlacement = Horizontal | Vertical deriving (Show, Read, Eq)

data TableSource
  = StaticSource [[Cell]]
  | RestSource
      { url :: String
      , fields :: FieldMapping
      , refreshSeconds :: Int
      }
  deriving Show

data FieldMapping = FieldMapping
  { titleField :: String
  , bodyField  :: String
  , idField    :: String
  } deriving (Show, Generic)

data TableConfig = TableConfig
  { title         :: Maybe String
  , columnHeaders :: Maybe [String]
  , placement     :: TablePlacement
  , columnWeights :: [Int]
  , columnHeights :: [Int]
  , maxWidth      :: Maybe Int
  , source        :: TableSource
  } deriving Show

instance FromJSON FieldMapping where
  parseJSON = withObject "FieldMapping" $  ->
    FieldMapping <$> v .: "title" <*> v .: "body" <*> v .: "id"

instance FromJSON TableSource where
  parseJSON = withObject "TableSource" $  -> do
    typ <- v .: "type"
    case (typ :: String) of
      "static" -> do
        rawRows <- v .: "rows"
        let parseCell [txt, mUrl] = (txt, mUrl)
        return $ StaticSource (map (map parseCell) rawRows)
      "rest" -> RestSource
        <$> v .: "url"
        <*> v .: "fields"
        <*> v .:? "refreshSeconds" .!= 5
      _ -> fail "Unknown source type"

instance FromJSON TableConfig where
  parseJSON = withObject "TableConfig" $  ->
    TableConfig <$> v .:? "title"
                <*> v .:? "columnHeaders"
                <*> v .:  "placement"
                <*> v .:  "columnWeights"
                <*> v .:  "columnHeights"
                <*> v .:? "maxWidth"
                <*> v .:  "source"

-- Drawing logic using maxWidth

drawUI :: St -> [Widget Name]
drawUI st = [center $ layoutTables st (tables st) (tableRowsData st) 0]

layoutTables :: St -> [TableConfig] -> [[Row]] -> Int -> Widget Name
layoutTables _ [] _ _ = emptyWidget
layoutTables st (t:ts) (r:rs) idx =
  let w = drawTable st idx t r
      rest = layoutTables st ts rs (idx + 1)
  in case placement t of
       Horizontal -> hBox [w, padLeft (Pad 2) rest]
       Vertical   -> vBox [w, padTop (Pad 1) rest]
layoutTables _ _ _ _ = emptyWidget

drawTable :: St -> Int -> TableConfig -> [Row] -> Widget Name
drawTable st idx cfg rows = Widget Fixed Fixed $ do
  ctx <- getContext
  let avail = availWidth ctx
      usable = maybe avail (min avail) (maxWidth cfg)
      colWs = distributeWidths usable (columnWeights cfg)
      heights = columnHeights cfg
      selRow = if activeTableIndex st == idx then rowPositions st !! idx else -1
      selCol = if activeTableIndex st == idx then colPositions st !! idx else -1

      headerWidgets = case columnHeaders cfg of
        Just hs -> [drawHeaderRow colWs hs, drawBorder colWs]
        Nothing -> []

      tableLines = concatMap (drawRow colWs heights selRow selCol) (zip [0..] rows)
      allLines = headerWidgets ++ tableLines

      titled w = case title cfg of
        Just t -> vBox [withAttr "title" (str t), padTop (Pad 1) w]
        Nothing -> w

  render $ border $ titled $ vBox allLines

drawHeaderRow :: [Int] -> [String] -> Widget Name
drawHeaderRow colWs hs =
  hBox $ zipWith (\w h -> padRight (Pad (w - length h)) (str h) <+> str " |") colWs (take (length colWs) hs ++ repeat "")

drawRow :: [Int] -> [Int] -> Int -> Int -> (Int, Row) -> [Widget Name]
drawRow widths heights selRow selCol (i, row) =
  let wrapped = zipWith3 (\w h (txt, _) -> wrapOrTruncate w h txt) widths heights row
      padded = padCells (maximum heights) wrapped
      linesPerRow = transpose padded
  in map (drawLine i row widths selRow selCol) linesPerRow ++ [drawBorder widths]

drawLine :: Int -> Row -> [Int] -> Int -> Int -> [String] -> Widget Name
drawLine i row widths selRow selCol line =
  hBox $ zipWith4 (drawCell i row selRow selCol) [0..] line widths (repeat 1)

drawCell :: Int -> Row -> Int -> Int -> Int -> String -> Int -> Int -> Widget Name
drawCell i row selRow selCol j txt w _ _ =
  let isSel = i == selRow && j == selCol
      attr = if isSel then withAttr selectedAttr else id
  in attr $ str " " <+> padRight (Pad (w - length txt)) (str txt) <+> str " |"

drawBorder :: [Int] -> Widget Name
drawBorder widths = str "+" <+> hBox (map (\w -> str (replicate (w + 3) '-')) widths) <+> str "+"

wrapOrTruncate :: Int -> Int -> String -> [String]
wrapOrTruncate w h txt =
  let chunks = map T.unpack $ T.chunksOf w (T.pack txt)
  in if length chunks <= h
     then chunks
     else take (h - 1) chunks ++ [take (w - 3) (chunks !! (h - 1)) ++ "..."]

padCells :: Int -> [[String]] -> [[String]]
padCells h = map (\xs -> xs ++ replicate (h - length xs) "")

distributeWidths :: Int -> [Int] -> [Int]
distributeWidths total weights =
  let sumW = sum weights
  in map (\w -> max 10 $ w * total `div` sumW) weights

-- Event handling

handleEvent :: St -> BrickEvent Name AppEvent -> EventM Name (Next St)
handleEvent st (AppEvent (UpdateTable i rs)) =
  continue st { tableRowsData = updateAt i (const rs) (tableRowsData st) }

handleEvent st (AppEvent (ReloadConfig cfgs)) = do
  rows <- mapM (\cfg -> case source cfg of
    StaticSource rs -> return rs
    _ -> return []) cfgs
  continue st
    { tables = cfgs
    , tableRowsData = rows
    , rowPositions = replicate (length cfgs) 0
    , colPositions = replicate (length cfgs) 0
    , activeTableIndex = 0
    }

handleEvent st (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt st
handleEvent st _ = continue st

app :: App St AppEvent Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const $ attrMap V.defAttr
      [ ("selected", fg V.yellow)
      , ("title", V.withStyle V.defAttr V.bold)
      ]
  }

-- Helpers

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs = take i xs ++ [f (xs !! i)] ++ drop (i + 1) xs

-- Config reloading

watchConfig :: FilePath -> BChan AppEvent -> IO ()
watchConfig path chan = do
  lastMod <- getModificationTime path
  forever $ do
    threadDelay 2_000_000
    newMod <- getModificationTime path
    when (newMod > lastMod) $ do
      res <- B.readFile path
      case eitherDecode res of
        Right cfg -> writeBChan chan (ReloadConfig cfg)
        Left err -> putStrLn $ "JSON parse error: " ++ err

startRestThreads :: [(Int, TableSource)] -> BChan AppEvent -> IO ()
startRestThreads sources chan = mapM_ forkSource sources
  where
    forkSource (i, RestSource url fields refresh) =
      void $ forkIO $ forever $ do
        rows <- fetchRestRows url fields
        writeBChan chan (UpdateTable i rows)
        threadDelay (refresh * 1_000_000)
    forkSource _ = return ()

-- Main

main :: IO ()
main = do
  let cfgFile = "tables.json"
  raw <- B.readFile cfgFile
  case eitherDecode raw of
    Left err -> putStrLn ("Failed to load config: " ++ err)
    Right tableCfgs -> do
      chan <- newBChan 10
      void $ forkIO $ watchConfig cfgFile chan
      startRestThreads (zip [0..] (map source tableCfgs)) chan
      rows <- mapM (\cfg -> case source cfg of
        StaticSource rs -> return rs
        _ -> return []) tableCfgs
      let st = St 0 (replicate (length tableCfgs) 0) (replicate (length tableCfgs) 0) tableCfgs rows
      void $ defaultMain app st
