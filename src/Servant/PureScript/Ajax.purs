{--
  This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}

module Servant.PureScript.Ajax where

import Prelude

import Affjax (Request, Response, request)
import Affjax.ResponseFormat as Response
import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Data.Argonaut.Decode.Generic.Rep (class DecodeRep, genericDecodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Effect.Aff (Aff, message)
import Effect.Aff.Class (class MonadAff, liftAff)
import Servant.PureScript.JsUtils (unsafeToString)


newtype AjaxError res
  = AjaxError
    { request     :: Request res
    , description :: ErrorDescription res
    }

data ErrorDescription res
  = UnexpectedHTTPStatus (Response res)
  | ParsingError String
  | DecodingError String
  | ConnectionError String


makeAjaxError :: forall res. Request res -> ErrorDescription res -> AjaxError res
makeAjaxError req desc =
  AjaxError
    { request : req
    , description : desc
    }

runAjaxError :: forall res. AjaxError res -> { request :: Request res, description :: ErrorDescription res }
runAjaxError (AjaxError err) = err

errorToString :: forall res. AjaxError res -> String
errorToString = unsafeToString

requestToString :: forall res. Request res -> String
requestToString = unsafeToString

responseToString :: forall res. Response res -> String
responseToString = unsafeToString


-- | Do an affjax call but report Aff exceptions in our own MonadError
ajax :: forall m res rep. Generic res rep => DecodeRep rep => MonadError (AjaxError res) m => MonadAff m
        => Request res -> m (Response res)
ajax req = do
  jsonResponse <- liftWithError $ request (req { responseFormat=Response.json })
  let errorDetails (Response.ResponseFormatError nativeError _val) = show nativeError
  decoded <- toDecodingError $ genericDecodeJson =<< lmap errorDetails jsonResponse.body
  pure
    { status: jsonResponse.status
    , statusText: jsonResponse.statusText
    , headers: jsonResponse.headers
    , body: decoded
    }
  where
    liftWithError :: forall a. Aff a -> m a
    liftWithError action = do
      res <- liftAff $ toEither action
      toAjaxError res

    toEither :: forall a. Aff a -> Aff (Either String a)
    toEither action = catchError (Right <$> action) $ \e ->
      pure $ Left (message e)

    toAjaxError :: forall a. Either String a -> m a
    toAjaxError r = case r of
        Left err -> throwError $ makeAjaxError req $ ConnectionError err
        Right v  -> pure v

    toDecodingError :: forall a. Either String a -> m a
    toDecodingError r = case r of
        Left err -> throwError $ makeAjaxError req $ DecodingError err
        Right v  -> pure v
