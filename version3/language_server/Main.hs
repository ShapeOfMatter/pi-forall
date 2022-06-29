{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE LambdaCase #-}

module Main (
  main
) where

import Control.Arrow (left)
import Control.Lens.Getter (view)
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Lens (uri)
import Language.LSP.VFS (virtualFileText, VirtualFile)
import Control.Monad.IO.Class
import qualified Data.Text as T
import Text.ParserCombinators.Parsec.Error ( errorPos, ParseError )

import Environment (emptyEnv, runTcMonad)
import Parser (parseExpr, Term)
import PrettyPrint (disp, render)
import TypeCheck (inferType)

handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SInitialized $ const (do
      pure ())
  , notificationHandler SWorkspaceDidChangeConfiguration $ const (do
      pure ())
  , requestHandler STextDocumentHover $ \req responder -> (do
      let RequestMessage _ _ _ (HoverParams doc pos _workDone) = req
          Position _l _c' = pos
          u = toNormalizedUri $ view uri doc
      vAst <- parseVirtual <$> getVirtualFile u
      message <- case vAst of
        Left err -> pure err
        Right ast -> do
          res <- liftIO $ runTcMonad emptyEnv $ inferType ast
          case res of
            Left typeErr -> return $ render $ disp typeErr
            Right ty -> return $ render $ disp ty
      let rsp = Hover ms (Just range)
          ms = HoverContents $ markedUpContent "lsp-pi4all-server" $ T.pack message
          range = Range pos pos
      responder (Right $ Just rsp))
  ]
  where parseVirtual :: Maybe VirtualFile -> Either String Term
        parseVirtual Nothing = Left "Couldn't find a file buffer?"
        parseVirtual (Just v) = left renderParseError $ parseExpr $ T.unpack . virtualFileText $ v
        renderParseError parseError = unlines [render $ disp $ errorPos parseError, show parseError]

syncOptions :: TextDocumentSyncOptions
syncOptions = TextDocumentSyncOptions
  { _openClose         = Just True
  , _change            = Just TdSyncIncremental
  , _willSave          = Just False
  , _willSaveWaitUntil = Just False
  , _save              = Just $ InR $ SaveOptions $ Just False
  }

lspOptions :: Options
lspOptions = defaultOptions
  { textDocumentSync = Just syncOptions
  , executeCommandCommands = Just ["lsp-pi4all-command"]
  }

main :: IO Int
main = do
  runServer $ ServerDefinition
    { defaultConfig = ()
    , onConfigurationChange = const $ pure $ Right ()
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options = lspOptions
    }

