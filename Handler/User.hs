{-# LANGUAGE FlexibleInstances, OverlappingInstances, ScopedTypeVariables #-}
module Handler.User where

import Import
import Yesod.Auth

instance ToJSON (Entity User) where
    toJSON (Entity userId (User ident pwd)) = object
        [ "id" .= userId
        , "ident" .= ident
        ]
    
instance FromJSON User where
    parseJSON (Object o) =  User
        <$> o .: "ident"
        <*> o .: "password"
    parseJSON _ = fail "Invalid user"
    
getUserR  :: UserId -> Handler TypedContent
getUserR userId = do
    user <- runDB $ get404 userId
    selectRep $ do
        provideRep $ return $ toJSON (Entity userId user)

getUsersR :: Handler TypedContent
getUsersR = do
    users <- runDB (selectList [] [Asc UserIdent]) 
    selectRep $ do
        provideRep $ return $ toJSON users -- this is mind blowing ...
        
postUsersR :: Handler TypedContent
postUsersR  = do
    user <- parseJsonBody_
    uid <- runDB $ insert $ user
    sendResponseCreated $ UserR uid
    --redirect $ UserR userId
