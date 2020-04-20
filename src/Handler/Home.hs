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
import Scrapper.HospitalParser
import Data.Text as T
import Data.Aeson
import Control.Monad.IO.Class (liftIO)
import Database.Persist

getHomeR :: Handler Html
getHomeR = do
    defaultLayout [whamlet| 
        Covid-19 API
    <ul>
        <li>
            /states (State List)
        <li>
            /states/#StatesId/cities (Cities list from state)
        <li>
            /states/#StatesId/cities/#CitiesId/hospitals (hospitals List from city)
        <li>
            /states/#StatesId/cities/#CitiesId/hospitals/#HospitalsId/rooms (Rooms list from Hospital)
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
    hospitals <- runDB $ selectList [HospitalsState ==. idState,HospitalsCity ==. idCity] [Asc HospitalsCnes]
    sendStatusJSON ok200 hospitals

getRoomsR:: StatesId -> CitiesId ->HospitalsId -> Handler Value
getRoomsR _ _ idHospital = do
    hospital <- runDB $ get idHospital
    rooms'' <- case hospital of
        (Just h) -> do
            rooms <- runDB $ selectList [RoomHospitalId ==. idHospital] []
            case rooms of
                [] -> do
                    rooms' <- liftIO $ getContentData  ((show $ hospitalsIbge h) ++ (show $ hospitalsCnes h))
                    putStrLn $ T.pack $ show rooms'
                    _<- insertAll $ roomToEntity idHospital rooms'
                    -- runDB $ selectList [RoomHospitalId ==. idHospital] []
                    return $ roomToEntity idHospital rooms'
                rooms' -> return $ entityToModel rooms'
        _ -> return []
    sendStatusJSON ok200 rooms''

roomToEntity :: HospitalsId -> [Room'] -> [Room]
roomToEntity _id ((Room' cnes codigo nome total total_sus):rms) = [ Room _id cnes codigo nome total total_sus ] ++ (roomToEntity _id rms)
roomToEntity _id _       = []

entityToModel es = fmap (\(Entity _ ee)-> ee) es

insertAll rooms = runDB $ mapM insert rooms
