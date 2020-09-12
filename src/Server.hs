{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Server where

import Todo
import Repository 

import Happstack.Server (ServerPart)
import Happstack.Server.Types (Response)
import Data.Int (Int64)
import Data.ByteString.Lazy.UTF8 (ByteString)
import Happstack.Server.RqData (decodeBody)
import Happstack.Server.RqData (defaultBodyPolicy)
import Control.Monad (msum)
import Happstack.Server.SimpleHTTP (dir)
import Happstack.Server (method)
import Happstack.Server (Method(POST))
import Data.Maybe (fromJust)
import Data.Aeson (decode, encode)
import Happstack.Server (resp)
import Happstack.Server.Response (ToMessage(toResponse))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Happstack.Server (takeRequestBody)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Happstack.Server (ServerMonad(askRq))
import Happstack.Server (RqBody(unBody))
import Happstack.Server (path)
import Happstack.Server.Types (Method(PATCH))
import Happstack.Server.Types (Method(GET))
import Happstack.Server.Response (ok)

handlers :: ServerPart Response
handlers = do 
            decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
            msum [
                dir "todos" $ do 
                    method POST
                    body <- getBody
                    let todo = fromJust $ decode body :: InputTodoItem
                    case (validate defaultValidations todo) of 
                        Left errors -> resp 400 $ toResponse (encode $ InvalidItemPayload {item = todo, errors = errors})
                        Right validTodo -> do 
                                    id <- lift $ write validTodo
                                    resp 201 $  toResponse (encode $ id),
                dir "todos" $ path $ \(id :: Int64) -> do method PATCH
                                                          body <- getBody
                                                          let updateTodoItem = (fromJust $ decode body :: UpdatedTodoItem)
                                                          lift $ updateTodo id updateTodoItem
                                                          [todo] <- lift $ readTodo id
                                                          ok (toResponse $ encode $ todo),                                   
                dir "todos" $ path $ \(id :: Int64) -> do method GET 
                                                          todo <- lift $ readTodo id
                                                          ok (toResponse $ encode todo),
                dir "todos" $ do method GET 
                                 list <- lift readTodoList
                                 ok (toResponse $ encode list)
             ]


-- https://stackoverflow.com/questions/8865793/how-to-create-json-rest-api-with-happstack-json-body
-- https://stackoverflow.com/questions/35592415/multiple-monads-in-one-do-block
getBody :: ServerPart ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return "" 
