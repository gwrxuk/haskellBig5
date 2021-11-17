{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( big5Reader
    ) where

import System.IO
import Control.Monad
import Text.Regex.TDFA
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Aeson
import Data.List
import qualified Data.ByteString.Lazy.Char8 as B
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

type Facets = M.Map String Int

data Scores = Scores
    {scoresOverall :: Int
    ,scoresFacets :: Facets
    } 

data PersonalInfo = PersonalInfo
    {persoanlInfoName :: String
    ,personalInfoEmail :: String
    ,personalInfoExtraversion :: Scores
    ,personalInfoAgreeableness :: Scores
    ,personalInfoConscientiousness :: Scores
    ,personalInfoNeuroticism :: Scores
    ,personalInfoOpenness :: Scores
    }
 

instance ToJSON Scores where
    toJSON (Scores scoresOverall scoresFacets) = object
            [ "Overall Score" .= scoresOverall
                ,"Facets" .= scoresFacets
            ]

instance ToJSON PersonalInfo where
    toJSON (PersonalInfo name email extraversion agreeableness conscientiousness neuroticism openness) = object
        
            [ "NAME" .= name
              ,"EMAIL" .= email
              ,"EXTRAVERSION" .= extraversion
              ,"AGREEABLENESS" .= agreeableness
              ,"CONSCIENTIOUSNESS" .= conscientiousness
              ,"NEUROTICISM" .= neuroticism
              ,"OPENNESS TO EXPERIENCE" .= openness

            ]
facetScore =  M.fromList[]

scores :: Scores
scores = Scores 1 facetScore

personalInfo :: PersonalInfo
personalInfo = PersonalInfo "Jung-Hua Liu" "gwrx2005@gmail.com" scores scores scores scores scores
    
checkData :: String ->[String]
checkData x = do
    getAllTextMatches (x =~ ("[-a-zA-Z0-9 ]+"::String)) :: [String]

castString :: [String] -> (String, Int)
castString x = (x !! 0, read(x!!1)::Int)

createFacets :: [String] -> [(String, Int)]
createFacets xs = do
                let t = map checkData xs
                map castString t
             

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

domains :: [String]
domains = ["EXTRAVERSION","AGREEABLENESS","CONSCIENTIOUSNESS","NEUROTICISM","OPENNESS TO EXPERIENCE"]
big5Reader :: IO ()
big5Reader = do
    manager <- newManager tlsManagerSettings 
    handle <- openFile "data/big5.txt" ReadMode
    contents <- hGetContents handle
    let allFacets = checkData contents
    let allLines = lines contents
    
    let t = []
    let overall =  (elemIndices (domains !! 0) allFacets) ++ 
              (elemIndices (domains !! 1) allFacets) ++ 
              (elemIndices (domains !! 2) allFacets) ++ 
              (elemIndices (domains !! 3) allFacets) ++ 
              (elemIndices (domains !! 4) allFacets) 
    let from = 1
    let to =  ((overall !! 1) `div` 2)-1
    let parts = createFacets $ slice from to allLines
    let c = M.fromList(parts)
    let overallS = read(allFacets !! ((overall !! 0)+1))::Int
    let s = Scores overallS c
    let p1 = personalInfo{personalInfoExtraversion=s}

  
    let from = ((overall !! 1) `div` 2)+1
    let to =  ((overall !! 2) `div` 2)-1
    let parts = createFacets $ slice from to allLines
    let c = M.fromList(parts)
    let overallS = read(allFacets !! ((overall !! 1)+1))::Int
    let s = Scores overallS c
    let p2 = p1{personalInfoAgreeableness=s}

    let from = ((overall !! 2) `div` 2)+1
    let to =  ((overall !! 3) `div` 2)-1
    let parts = createFacets $ slice from to allLines
    let c = M.fromList(parts)
    let overallS = read(allFacets !! ((overall !! 2)+1))::Int
    let s = Scores overallS c
    let p3 = p2{personalInfoConscientiousness=s}


    let from = ((overall !! 3) `div` 2)+1
    let to =  ((overall !! 4) `div` 2)-1
    let parts = createFacets $ slice from to allLines
    let c = M.fromList(parts)
    let overallS = read(allFacets !! ((overall !! 3)+1))::Int
    let s = Scores overallS c
    let p4 = p3{personalInfoNeuroticism=s}

    let from = ((overall !! 4) `div` 2)+1
    let to =  length allLines
    let parts = createFacets $ slice from to allLines
    let c = M.fromList(parts)
    let overallS = read(allFacets !! ((overall !! 4)+1))::Int
    let s = Scores overallS c
    let p5 = p4{personalInfoOpenness=s}


    initialRequest <- parseRequest "https://recruitbot.trikeapps.com/api/v1/roles/bellroy-tech-team-recruit/big_five_profile_submissions"
    let request = initialRequest { method = "POST", requestBody = RequestBodyLBS $ encode p5 }

    response <- httpLbs request manager
    putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    print $ responseBody response
    hClose handle
