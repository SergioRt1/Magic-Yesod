{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Handler.Search where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data Search = Search {
    name :: Maybe Text,
    description :: Maybe Text,
    expansion :: Maybe Text,
    defense :: Maybe Int,
    attack :: Maybe Int,
    color :: Maybe Text,
    mana :: Maybe Int
 }

--Aform From Entity product
searchForm :: Maybe Search -> AForm Handler Search
searchForm _ = Search
    <$> aopt textField "Name" Nothing
    <*> aopt textField "Description" Nothing
    <*> aopt textField "Expansion" Nothing
    <*> aopt intField "Defense" Nothing
    <*> aopt intField "Attack" Nothing
    <*> aopt (selectFieldList colors) "Color" Nothing
    <*> aopt intField "Mana" Nothing
   where
      colors :: [(Text, Text)]
      colors = [("White - Plains", "White - Plains"),("Red - Mountain", "Red - Mountain"), ("Blue - Island", "Blue - Island"), ("Green - Forest", "Green - Forest"), ("Black - Swamps", "Black - Swamps")]

--CRUD
--Create

getSearchR ::  Handler Html
getSearchR  = do
                    ( widget , encoding ) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ searchForm Nothing
                    let actionR = SearchR
                    defaultLayout $ do
                       $(widgetFile "Card/Form")

postSearchR :: Handler Html
postSearchR = do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ searchForm Nothing
                case result of
                    FormSuccess values -> do
                              if name values /= Nothing then do
                                 cards <- runDB $ selectList [CardName ==. (des (name values))] []
                                 defaultLayout $ do
                                    $(widgetFile "Card/List")
                              else if description values /= Nothing then do
                                  cards <- runDB $ selectList [CardDescription ==. (des (description values))] []
                                  defaultLayout $ do
                                    $(widgetFile "Card/List")
                              else if expansion values /= Nothing then do
                                  cards <- runDB $ selectList [CardExpansion ==. (des (expansion values))] []
                                  defaultLayout $ do
                                    $(widgetFile "Card/List")
                              else if defense values /= Nothing then do
                                  cards <- runDB $ selectList [CardDefense ==. (defense values)] []
                                  defaultLayout $ do
                                    $(widgetFile "Card/List")
                              else if attack values /= Nothing then do
                                  cards <- runDB $ selectList [CardAttack ==. (attack values)] []
                                  defaultLayout $ do
                                    $(widgetFile "Card/List")
                              else if color values /= Nothing then do
                                  cards <- runDB $ selectList [CardColor ==. (des (color values))] []
                                  defaultLayout $ do
                                    $(widgetFile "Card/List")
                              else if mana values /= Nothing then do
                                  cards <- runDB $ selectList [CardMana ==. (des (mana values))] []
                                  defaultLayout $ do
                                    $(widgetFile "Card/List")
                              else do
                                  cards <- runDB $ selectList [][]
                                  defaultLayout $ do
                                    $(widgetFile "Card/List")
                    _ -> defaultLayout $ do
                        let actionR = SearchR
                        $(widgetFile "Card/Form")

des :: Maybe a -> a
des (Just a) = a
des Nothing = error "Empty"