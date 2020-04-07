{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs  #-}

module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Database.Persist

getHomeR :: Handler Html
getHomeR = do
    hospitals <- runDB $ selectList [] [Asc HospitalsCnes]
    defaultLayout [whamlet|
            <ul>
                $forall Entity _ hospital <- hospitals
                    <li> #{hospitalsCnes hospital}
        |]

getStatesR :: Handler Value
getStatesR = do
    states <- runDB $ selectList [] [Asc StatesCode]
    sendStatusJSON ok200 ((array states)::Value)

getCitiesR :: StatesId -> Handler Value
getCitiesR idState = do
    state <- runDB $ selectList [CitiesState ==. idState] []
    sendStatusJSON ok200 state

getHospitalsR :: StatesId -> CitiesId -> Handler Value
getHospitalsR idState idCity = do
    hospitals <- runDB $ selectList [HospitalsInfoState ==. idState,HospitalsInfoCity ==. idCity] [Asc HospitalsInfoCnes]
    sendStatusJSON ok200 hospitals
