{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Card where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

--Aform From Entity Card
cardForm :: Maybe Card -> AForm Handler Card
cardForm   card = Card 
    <$> areq textField "name" (cardName <$> card)
    <*> areq textField "description" (cardDescription <$> card) 
    <*> areq textField "expansion" (cardExpansion <$> card) 
    <*> aopt intField "Defence" (cardExpansion <$> card)
    <*> aopt intField "Attack" (cardExpansion <$> card)  

--CRUD 
--Create
getCreateCardR ::  Handler Html 
getCreateCardR = do 
               (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ cardForm Nothing
               defaultLayout $ do
                    let actionR = CreateCardR                          
                    $(widgetFile "Card/CardCreate") 

postCreateCardR :: Handler Html
postCreateCardR = do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ cardForm  Nothing
                case result of
                    FormSuccess card -> do 
                                _ <- runDB $ insert card
                                redirect HomeR
                    _ -> defaultLayout $ do
                        let actionR = CreateCardR
                        $(widgetFile "Card/CardCreate")

