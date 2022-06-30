{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE LambdaCase #-}

module Main (
  main
) where

import Control.Arrow (left)
import Control.Lens.Getter (view, (^.))
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Lens (params, textDocument, uri)
import Language.LSP.VFS (virtualFileText, VirtualFile)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.Text as T
import Text.ParserCombinators.Parsec.Error ( errorPos, ParseError )

import Environment (emptyEnv, runTcMonad, TcMonad)
import Parser (parseExpr, Term)
import PrettyPrint (disp, render)
import TypeCheck (inferType)

type TyperM conf = ExceptT String (LspT conf TcMonad)

parse :: VersionedTextDocumentIdentifier -> TyperM conf ()
parse v = undefined

serverName :: T.Text
serverName = "lsp-pi4all-server"

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: Maybe FilePath -> NormalizedUri -> Maybe Int -> LspM conf ()
sendDiagnostics filePath fileUri version = do
  let diags = [Diagnostic
                (Range (Position 0 1) (Position 0 5))
                (Just DsError)  -- severity
                Nothing  -- code
                (Just serverName) -- source
                "Example diagnostic message"
                Nothing -- tags
                (Just (List []))
              ]
  publishDiagnostics 100 fileUri version (partitionBySource diags)

handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SInitialized $ const (do
      pure ())
  , notificationHandler SWorkspaceDidChangeConfiguration $ const (do
      pure ())
  , notificationHandler STextDocumentDidOpen $ \msg -> (do
      let doc  = msg ^. params . textDocument . uri
          fileName =  uriToFilePath doc
      -- logger <& ("Processing DidOpenTextDocument for: " <> T.pack (show fileName)) `WithSeverity` Info
      sendDiagnostics fileName (toNormalizedUri doc) (Just 0))
  , notificationHandler STextDocumentDidChange $ \msg -> (do
      let doc  = msg ^. params . textDocument . uri
          fileName = uriToFilePath doc
      -- logger <& ("Processing DidOpenTextDocument for: " <> T.pack (show fileName)) `WithSeverity` Info
      sendDiagnostics fileName (toNormalizedUri doc) (Just 0))
  , requestHandler STextDocumentHover $ \req responder -> (do
      let RequestMessage _ _ _ (HoverParams doc pos _workDone) = req
          Position _l _c' = pos
          _u = toNormalizedUri $ view uri doc
      {-vAst <- parseVirtual <$> getVirtualFile u
      message <- case vAst of
        Left err -> pure err
        Right ast -> do
          res <- liftIO $ runTcMonad emptyEnv $ inferType ast
          case res of
            Left typeErr -> return $ render $ disp typeErr
            Right ty -> return $ render $ disp ty-}
      let rsp = Hover ms (Just range)
          ms = HoverContents $ markedUpContent serverName $ T.pack "hello"
          range = Range pos pos
      responder (Right $ Just rsp))
  ]
  where _parseVirtual :: Maybe VirtualFile -> Either String Term
        _parseVirtual Nothing = Left "Couldn't find a file buffer?"
        _parseVirtual (Just v) = left _renderParseError $ parseExpr $ T.unpack . virtualFileText $ v
        _renderParseError parseError = unlines [render $ disp $ errorPos parseError, show parseError]

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

