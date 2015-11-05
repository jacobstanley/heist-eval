{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Blaze.ByteString.Builder

import           Control.Applicative
import           Control.Lens (set)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe

import qualified Data.ByteString as BS
--import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable

import           Heist
import qualified Heist.Interpreted as I
import qualified Heist.Interpreted.Internal as I
import           Heist.Internal.Types.HeistState (_spliceDefault)

import qualified Text.XmlHtml as X

data Error =
    InitError [String]
  | RenderError
  deriving (Show)

data RobVal =
    String Text
  | List   [RobVal]
  | Object (Map Text RobVal)
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  x <- runEitherT simple
  case x of
    Left err -> print err
    Right () -> pure ()

simple :: EitherT Error IO ()
simple = do
  heistState <- bimapEitherT InitError id (EitherT (initHeist heistConfig))

  let env = Map.fromList
            [ ("foos", List $ [ Object $ Map.fromList
                                [ ("quux",    String "this is foo1 quux")
                                , ("snaffle", String "this is foo1 snaffle") ]
                              , Object $ Map.fromList
                                [ ("quux",    String "this is foo2 quux")
                                , ("snaffle", String "this is foo2 snaffle") ]
                              ])

            , ("bars", List $ [ String "this is bar1 quux"
                              , String "this is bar2 snaffle"
                              ])
            ]

  let heistState' = heistState { _spliceDefault = robSplice env }

  (builder, _) <- fromMaybeE RenderError =<< I.renderTemplate heistState' "home"

  liftIO (toByteStringIO BS.putStr builder)

heistConfig :: (Functor m, MonadIO m) => HeistConfig m
heistConfig =
    set hcInterpretedSplices defaultInterpretedSplices
  $ set hcLoadTimeSplices    defaultInterpretedSplices
  $ set hcTemplateLocations  [loadTemplates "templates"]
  $ emptyHeistConfig

robSplice :: (Functor m, MonadIO m) => Map Text RobVal -> X.Node -> Maybe (HeistT m m [X.Node])
robSplice env node = eachSplice env node <|> varSplice env node

eachSplice :: (Functor m, MonadIO m) => Map Text RobVal -> X.Node -> Maybe (HeistT m m [X.Node])
eachSplice env node = do
  each <- X.getAttribute "each" node
  vars <- robList =<< Map.lookup each env
  return $ do
    X.Element tag attrs _ <- getParamNode
    fmap concat . for vars $ \var -> do
      let env' = fromMaybe (Map.singleton "value" var) (robObject var) `Map.union` env
      liftIO (print env')
      localHS (\s -> s { _spliceDefault = robSplice env' }) $ do
        nodes <- I.runChildren
        let attrs' = filter (\(k,_) -> k /= "each") attrs
        return [X.Element tag attrs' nodes]

varSplice :: (Functor m, MonadIO m) => Map Text RobVal -> X.Node -> Maybe (HeistT m m [X.Node])
varSplice env node = do
  name <- X.tagName node
  str  <- robString =<< Map.lookup name env
  return $ do
    return [X.TextNode str]

robList :: RobVal -> Maybe [RobVal]
robList (List xs) = Just xs
robList _         = Nothing

robString :: RobVal -> Maybe Text
robString (String x) = Just x
robString _          = Nothing

robObject :: RobVal -> Maybe (Map Text RobVal)
robObject (Object x) = Just x
robObject _          = Nothing

fromMaybeE :: Monad m => x -> Maybe a -> EitherT x m a
fromMaybeE x = hoistEither . maybe (Left x) Right
