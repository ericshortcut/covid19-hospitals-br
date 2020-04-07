{-# LANGUAGE OverloadedStrings #-}

module Scrapper.HospitalParser where

import Text.HTML.Scalpel
import Data.List.Split as D

data Room = NoRoom | Room {cnes::String,codigo::String,nome::String,total::String,total_sus::String} deriving (Eq,Show)

urlCNES :: String -> String
urlCNES _id = "http://cnes2.datasus.gov.br/cabecalho_reduzido.asp?VCod_Unidade=" ++ _id

getHostitalRoomTables :: [String] -> [String]
getHostitalRoomTables xs = takeWhile (/="Equipamento:") $ dropWhile (/="Leitos") xs

splitSubTables :: [String] -> [[String]]
splitSubTables xs = filter ((=="Nome Leitos").head)$ map reverse$ D.splitWhen (=="Codigo")  $ reverse xs

splitFilterTableLines :: [[a]] -> [[a]]
splitFilterTableLines lns = concat $ map (\x-> filter (\xl->4==(length xl)) $ D.chunksOf 4 $ drop 3 x) lns

arrayToRoom :: String -> [String] -> Room
arrayToRoom _id (a:b:c:d:_) = Room _id a b c d
arrayToRoom _ _ = NoRoom

getContentData :: String -> IO [Room]
getContentData url = do
    resp <- scrapeURL (urlCNES url) (texts $ tagSelector "font")
    return $ case resp of
        Just val-> processValues url val
        _ -> [NoRoom]

processValues :: String -> [String] -> [Room]
processValues url v
       | isEmpty v = map (arrayToRoom url) $ splitFilterTableLines $ splitSubTables $ getHostitalRoomTables  v
       | otherwise = [NoRoom]
    where
        isEmpty ["Estabelecimento n\227o Cadastrado"] = False
        isEmpty _ = True
