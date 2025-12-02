{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import HTMLSVGCompare
import Hakyll (Compiler, Identifier, defaultHakyllReaderOptions, defaultHakyllWriterOptions, itemBody, pandocCompilerWithTransformM)
import Hakyll.Web.Pandoc.Diagrams
import Test.Hspec
import Text.Pandoc.Definition (Pandoc)
import Util


main :: IO ()
main = hspec $ after_ cleanTestEnv $ do
  describe "The four output variations" $ do
    describe "<img> linking an external SVG file" $ do
      it "evaluates code that has no attributes" $ do
        html <- compileMarkdownWith drawDiagrams "data/external_bare.md"
        svg <- T.readFile "_testsite/external_bare.svg"
        html `shouldBeSimilar` "test/data/external_bare.html"
        svg `shouldBeSimilar` "test/data/no_attributes.svg"
      it "evaluates code with attributes" $ do
        html <- compileMarkdownWith drawDiagrams "data/external.md"
        svg <- T.readFile "_testsite/external.svg"
        html `shouldBeSimilar` "test/data/external.html"
        svg `shouldBeSimilar` "test/data/with_attributes.svg"

    describe "<img> linking an external SVG file within a <figure>" $ do
      it "evaluates code that has no attributes" $ do
        html <- compileMarkdownWith drawDiagrams "data/external_figure_bare.md"
        svg <- T.readFile "_testsite/external_figure_bare.svg"
        html `shouldBeSimilar` "test/data/external_figure_bare.html"
        svg `shouldBeSimilar` "test/data/no_attributes.svg"
      it "evaluates code with attributes" $ do
        html <- compileMarkdownWith drawDiagrams "data/external_figure.md"
        svg <- T.readFile "_testsite/external_figure.svg"
        html `shouldBeSimilar` "test/data/external_figure.html"
        svg `shouldBeSimilar` "test/data/with_attributes.svg"

    describe "<svg> code inline" $ do
      it "evaluates code that has no attributes" $ do
        html <- compileMarkdownWith drawDiagrams "data/inline_bare.md"
        html `shouldBeSimilar` "test/data/inline_bare.html"
      it "evaluates code with attributes" $ do
        html <- compileMarkdownWith drawDiagrams "data/inline.md"
        html `shouldBeSimilar` "test/data/inline.html"

    describe "<svg> code inline within a <figure>" $ do
      it "evaluates code that has no attributes" $ do
        html <- compileMarkdownWith drawDiagrams "data/inline_figure_bare.md"
        html `shouldBeSimilar` "test/data/inline_figure_bare.html"
      it "evaluates code with attributes" $ do
        html <- compileMarkdownWith drawDiagrams "data/inline_figure.md"
        html `shouldBeSimilar` "test/data/inline_figure.html"

    describe "Changing the options" $ do
      it "import Data.Maybe global Module to use fromJust" $ do
        let opts =
              defaultOptions
                { globalModules =
                    [ ("Prelude", Nothing)
                    , ("Diagrams.Prelude", Nothing)
                    , ("Diagrams.Backend.SVG", Nothing)
                    , ("Data.Maybe", Nothing)
                    ]
                }
        html <- compileMarkdownWith (drawDiagramsWith opts) "data/global_import.md"
        html `shouldBeSimilar` "test/data/global_import.html"

      it "import an invalid global Module" $ do
        let opts =
              defaultOptions
                { globalModules =
                    [ ("Prelude", Nothing)
                    , ("Diagrams.Prelude", Nothing)
                    , ("Diagrams.Backend.SVG", Nothing)
                    , ("foo", Nothing)
                    ]
                }
        let html = compileMarkdownWith (drawDiagramsWith opts) "data/global_import.md"
        html `shouldThrow` anyIOException

      it "set a language extension" $ do
        let opts =
              defaultOptions
                { languageExtensions = ["NoMonomorphismRestriction"]
                }
        html <- compileMarkdownWith (drawDiagramsWith opts) "data/inline.md"
        html `shouldBeSimilar` "test/data/inline.html"

      it "set an invalid language extension" $ do
        let opts =
              defaultOptions
                { languageExtensions = ["foo"]
                }
        let html = compileMarkdownWith (drawDiagramsWith opts) "data/inline.md"
        html `shouldThrow` anyIOException


compileMarkdownWith :: (Pandoc -> Compiler Pandoc) -> Identifier -> IO T.Text
compileMarkdownWith compiler path = do
  store <- newTestStore
  provider <- newTestProvider store
  item <- testCompilerDone store provider path $ pandocCompWithTransform' compiler
  pure . T.pack . itemBody $ item
  where
    pandocCompWithTransform' = pandocCompilerWithTransformM defaultHakyllReaderOptions defaultHakyllWriterOptions
