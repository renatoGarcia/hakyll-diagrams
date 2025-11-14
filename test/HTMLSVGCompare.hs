{-# LANGUAGE OverloadedStrings #-}

module HTMLSVGCompare (
  shouldBeSimilar,
) where

import Data.List (sortOn)
import Data.Text (Text, strip)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as TR
import Test.Hspec
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree


data Norm = Norm
  { nName :: Text
  , nAttrs :: [(Text, Text)]
  , nChildren :: [Norm]
  , nText :: Text
  }
  deriving (Eq, Show)


-- | Expect two HTML snippets to be "structurally equal"
shouldBeSimilar :: Text -> FilePath -> Expectation
shouldBeSimilar actual expectedPath = do
  expected <- T.readFile expectedPath
  let normA = normalizeTree <$> parseTree (strip actual)
      normE = normalizeTree <$> parseTree (strip expected)
  normA `shouldBe` normE


normalizeTree :: TagTree Text -> Norm
normalizeTree node = case node of
  TagBranch name attrs children
    | name == "svg" ->
        Norm
          { nName = name
          , nAttrs = normalizeAttrs attrs
          , -- We don't care about the actual drawing, that
            -- is responsibility of the diagrams library
            nChildren = []
          , nText = ""
          }
    | otherwise ->
        Norm
          { nName = name
          , nAttrs = normalizeAttrs attrs
          , nChildren = normalizeTree <$> children
          , nText = ""
          }
  TagLeaf (TagText txt) ->
    Norm
      { nName = "text"
      , nAttrs = []
      , nChildren = []
      , nText = normalizeText txt
      }
  TagLeaf (TagOpen name _)
    | name `elem` ["?xml", "!DOCTYPE"] ->
        Norm
          { nName = name
          , nAttrs = []
          , nChildren = []
          , nText = ""
          }
  other -> error $ "normalizeTree: expected element but got: " ++ show other
  where
    normalizeAttrs attrs = sortOn fst [(k, normalizeText v) | (k, v) <- attrs]
    normalizeText t = T.unwords $ normalizeNumeric <$> T.words t


parseNumber :: T.Text -> Maybe Double
parseNumber t =
  case TR.double t of
    Right (n, rest) | T.null rest -> Just n
    _ -> Nothing


epsilon :: Double
epsilon = 0.001


normalizeNumeric :: T.Text -> T.Text
normalizeNumeric t =
  case parseNumber t of
    Just n ->
      let bucket = (fromIntegral :: Int -> Double) (round (n / epsilon)) * epsilon
       in T.pack (show bucket)
    Nothing -> T.strip t
