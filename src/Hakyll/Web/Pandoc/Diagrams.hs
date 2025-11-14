{-# LANGUAGE OverloadedStrings #-}

{- |
License     : BSD-3-Clause
Maintainer  : Renato Garcia
-}
module Hakyll.Web.Pandoc.Diagrams (
  drawDiagrams,
  drawDiagramsWith,
  Options (..),
  defaultOptions,
) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base16 as B16
import Data.Default (Default, def)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text, unpack)
import qualified Data.Text as T (drop, isPrefixOf, length, null, pack, take, unpack, unwords)
import qualified Data.Text.Encoding as TE
import qualified Diagrams.Backend.SVG as SVG
import Diagrams.Core (renderDia)
import Diagrams.Prelude (Any, QDiagram, SizeSpec, V2, mkSizeSpec2D)
import Graphics.Svg.Core (makeAttribute)
import Hakyll (Compiler, destinationDirectory, makeDirectories)
import Hakyll.Core.Compiler (unsafeCompiler)
import Hakyll.Core.Compiler.Internal (CompilerRead (..), compilerAsk)
import Language.Haskell.Interpreter (OptionVal ((:=)))
import qualified Language.Haskell.Interpreter as Hint
import Text.Pandoc.Definition (Block (..), Caption (Caption), Inline (..), Pandoc)
import Text.Pandoc.Walk (walkM)
import Text.Read (readMaybe)


-- | Configure the interpreter environment used when rendering the diagrams.
data Options = Options
  { globalModules :: [(Hint.ModuleName, Maybe String)]
  -- ^ Global modules to import ("module name", "namespace qualifier")
  , localModules :: [(Hint.ModuleName, Maybe String)]
  -- ^ Local modules to import ("module name", "namespace qualifier")
  , searchPaths :: [FilePath]
  -- ^ The paths where to search for local modules
  , languageExtensions :: [Hint.Extension]
  -- ^ Language extensions in use by the interpreter
  }
  deriving (Show)


instance Default Options where
  def = defaultOptions


{- | The default value will import "Prelude", "Diagrams.Prelude", and
"Diagrams.Backend.SVG" global modules. All other fields will be emtpty.
-}
defaultOptions :: Options
defaultOptions =
  Options
    { globalModules =
        [ ("Prelude", Nothing)
        , ("Diagrams.Prelude", Nothing)
        , ("Diagrams.Backend.SVG", Nothing)
        ]
    , localModules = []
    , searchPaths = []
    , languageExtensions = []
    }


setUpInterpreter :: Options -> Text -> Hint.Interpreter (QDiagram SVG.SVG V2 Double Any)
setUpInterpreter opts code = do
  Hint.set
    [ Hint.searchPath := searchPaths opts
    , Hint.languageExtensions := languageExtensions opts
    ]
  Hint.loadModules $ fst <$> localModules opts
  Hint.setImportsQ $ globalModules opts ++ localModules opts
  Hint.interpret (T.unpack code) (Hint.as :: QDiagram SVG.SVG V2 Double Any)


buildDiagram :: Options -> Text -> IO (QDiagram SVG.SVG V2 Double Any)
buildDiagram opts code = do
  result <- Hint.runInterpreter $ setUpInterpreter opts code
  case result of
    Right diagram -> pure diagram
    Left (Hint.WontCompile errs) ->
      fail $
        foldl (\str err -> str ++ "\n" ++ Hint.errMsg err) "" errs
    Left err -> fail $ show err


hashCodePrefix :: Text -> Text
hashCodePrefix code =
  let hash = T.take 8 . TE.decodeUtf8 . B16.encode . SHA1.hash . TE.encodeUtf8 $ code
   in "dia_" <> hash <> "_"


genInlineSvg :: Options -> Text -> SizeSpec V2 Double -> Text -> [Text] -> [(Text, Text)] -> IO Text
genInlineSvg opts code imageSize elementId classes attributes =
  T.pack . show . renderDia SVG.SVG svgOptions <$> buildDiagram opts code
  where
    attrs =
      attributes
        ++ [("id", elementId) | not . T.null $ elementId]
        ++ [("class", T.unwords classes) | not . null $ classes]

    svgOptions =
      SVG.SVGOptions
        imageSize
        Nothing
        (hashCodePrefix code)
        [makeAttribute k v | (k, v) <- attrs]
        False


genImageFile ::
  -- | Interpreter configuration
  Options ->
  -- | Destination directory
  FilePath ->
  -- | Output path relative to destination
  FilePath ->
  -- | Haskell code to interpret
  Text ->
  -- | Output image size
  SizeSpec V2 Double ->
  [(Text, Text)] ->
  IO ()
genImageFile opts destDir relPath code imageSize attributes
  | null relPath = fail "The `relPath` attribute of a diagram can not be an empty string."
  | otherwise = do
      makeDirectories imagePath
      SVG.renderSVG' imagePath svgOptions =<< buildDiagram opts code
  where
    imagePath = destDir ++ "/" ++ relPath
    svgOptions =
      SVG.SVGOptions
        imageSize
        Nothing
        (hashCodePrefix code)
        [makeAttribute k v | (k, v) <- attributes]
        True


imageBlock :: Text -> [Text] -> [(Text, Text)] -> [Inline] -> Text -> Text -> Inline
imageBlock elementId classes attributes alt path title = Image (elementId, classes, attributes) alt (path, title)


figureBlock :: Text -> [Text] -> [(Text, Text)] -> Text -> Block -> Block
figureBlock elementId classes attributes caption img =
  -- ShortCaption is ignored by Pandoc when generating HTML
  Figure (elementId, classes, attributes) (Caption Nothing [Plain [Str caption]]) [img]


-- | Compiles the Diagrams code and transforms a code block with a .diagram class in a figure block
transformBlock :: Options -> FilePath -> Block -> IO Block
transformBlock opts destDir (CodeBlock (elementId, classes, keyVals) code)
  | Just relpath <- lookup "img:src" keyVals
  , Just caption <- lookup "figcaption" keyVals = do
      genImageFile opts destDir (unpack relpath) code (mkSizeSpec2D svgWidth svgHeight) (tagAttributes "svg")
      pure $
        figureBlock elementId classes (tagAttributes "figure") caption $
          Plain [imageBlock "" [] (tagAttributes "img") altText relpath imgTitle]
  | Just relpath <- lookup "img:src" keyVals = do
      genImageFile opts destDir (unpack relpath) code (mkSizeSpec2D svgWidth svgHeight) (tagAttributes "svg")
      pure $ Plain [imageBlock elementId classes (tagAttributes "img") altText relpath imgTitle]
  | Just caption <- lookup "figcaption" keyVals = do
      svgText <- genInlineSvg opts code (mkSizeSpec2D svgWidth svgHeight) "" [] (tagAttributes "svg")
      pure $ figureBlock elementId classes (tagAttributes "figure") caption $ RawBlock "html" svgText
  | otherwise = do
      svgText <- genInlineSvg opts code (mkSizeSpec2D svgWidth svgHeight) elementId classes (tagAttributes "svg")
      pure $ RawBlock "html" svgText
  where
    tagAttributes :: Text -> [(Text, Text)]
    tagAttributes tag =
      [ (T.drop (T.length prefix) k, v)
      | (k, v) <- keyVals
      , T.isPrefixOf prefix k
      , k `notElem` specialAttributes
      ]
      where
        prefix = tag <> ":"
        specialAttributes = ["img:src", "img:alt", "img:title", "svg:width", "svg:height"]

    altText = maybeToList (Str <$> lookup "img:alt" keyVals)

    imgTitle = fromMaybe "" (lookup "img:title" keyVals)

    svgWidth =
      fromMaybe (error "Failed to parse `svg:width` attribute value.")
        . readMaybe
        . unpack
        <$> lookup "svg:width" keyVals
    svgHeight =
      fromMaybe (error "Failed to parse `svg:height` attribute value.")
        . readMaybe
        . unpack
        <$> lookup "svg:height" keyVals
transformBlock _ _ block = pure block


{- | Render the code inside all 'Text.Pandoc.Definition.CodeBlock' with a @.diagram@ class
to either a 'Text.Pandoc.Definition.RawBlock' or an 'Text.Pandoc.Definition.Image'
(depending on if we have received an attribute with a path to an external file), where
these two can or cannot be within a parent 'Text.Pandoc.Definition.Figure' (depending on
if we have received an attribute with a figure caption).
-}
drawDiagramsWith :: Options -> Pandoc -> Compiler Pandoc
drawDiagramsWith opts = walkM visitor
  where
    visitor :: Block -> Compiler Block
    visitor (CodeBlock (ident, classes, attrs) code)
      | "diagram" `elem` classes = do
          destDir <- destinationDirectory . compilerConfig <$> compilerAsk
          unsafeCompiler $
            transformBlock opts destDir $
              CodeBlock (ident, filter (/= "diagram") classes, attrs) code
    visitor b = pure b


-- | Call 'drawDiagramsWith' with the 'defaultOptions'.
drawDiagrams :: Pandoc -> Compiler Pandoc
drawDiagrams = drawDiagramsWith (def :: Options)
