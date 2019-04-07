{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- {"name": "Turtle", "description": "Power full turtle", "expansion": "NA", "defense":4, "attack": 3, "color": "Blue","mana": 2}
instance ToJSON (Entity Card) where
    toJSON (Entity cardId card) = object
        [ "id"      .= (String $ toPathPiece cardId)
        , "name"   .= cardName card
        , "description"   .= cardDescription card
        , "expansion" .= cardExpansion card
        , "defense" .= cardDefense card
        , "attack" .= cardAttack card
        , "color" .= cardColor card
        , "mana" .= cardMana card
        ]

instance FromJSON Card where
        parseJSON (Object card) = Card
            <$> card .: "name"
            <*> card .: "description"
            <*> card .: "expansion"
            <*> card .: "defense"
            <*> card .: "attack"
            <*> card .: "color"
            <*> card .: "mana"
        parseJSON _ = mzero