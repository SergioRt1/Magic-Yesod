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
    <$> areq textField "Name" (cardName <$> card)
    <*> areq textField "Description" (cardDescription <$> card)
    <*> areq textField "Expansion" (cardExpansion <$> card)
    <*> aopt defenseField "Defense" (cardDefense <$> card)
    <*> aopt attackField "Attack" (cardAttack <$> card)
    <*> areq (selectFieldList colors) "Color" (cardColor <$> card)
    <*> areq intField "Mana" (cardMana <$> card)
    where
        errorMessage :: Text
        errorMessage = "There are no such powerful cards, be realistic!"
        defenseField = checkBool (<= 7) errorMessage intField
        attackField = checkBool (<= 7) errorMessage intField
        colors :: [(Text, Text)]
        colors = [("White - Plains", "White - Plains"),("Red - Mountain", "Red - Mountain"), ("Blue - Island", "Blue - Island"), ("Green - Forest", "Green - Forest"), ("Black - Swamps", "Black - Swamps")]


--CRUD 
--Create
getCreateCardR ::  Handler Html 
getCreateCardR = do 
               (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ cardForm Nothing
               defaultLayout $ do
                    let actionR = CreateCardR                          
                    $(widgetFile "Card/Form")

postCreateCardR :: Handler Html
postCreateCardR = do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ cardForm  Nothing
                case result of
                    FormSuccess card -> do 
                                _ <- runDB $ insert card
                                redirect HomeR
                    _ -> defaultLayout $ do
                        let actionR = CreateCardR
                        $(widgetFile "Card/Form")

