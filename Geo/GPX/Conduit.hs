{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Geo.GPX.Conduit
	( Track(..), GPX(..), Segment(..), Point(..)
	, readGPXFile
	) where

import Control.Monad.Trans.Control
import Control.Monad
import Data.Conduit
import Data.Conduit.Text
import Data.Conduit.List as L
import Data.Time.Format
import Data.Text (Text)
import qualified Data.Text as T
import System.Locale
import System.FilePath
import Data.String
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, buildTime, parseTime)
import Data.XML.Types
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import qualified Data.Attoparsec.Text as AT


-- |A GPX file usually is a single track (but can be many)
-- with one or more segments and many points in each segment.
data GPX = GPX { tracks :: [Track] }
	deriving (Eq, Ord, Show, Read)
data Track = Track 
	{ trkName		:: Maybe Text
	, trkDescription	:: Maybe Text
	, segments		:: [Segment]
	}
	deriving (Eq, Ord, Show, Read)

data Segment = Segment { points  :: [Point] }
	deriving (Eq, Ord, Show, Read)

type Latitude = Double
type Longitude = Double

-- |Track point is a full-fledged representation of all the data
-- available in most GPS loggers.  It is possible you don't want
-- all this data and can just made do with coordinates (via 'Pnt')
-- or a custom derivative.
data Point = Point
	{ pntLat	:: Latitude
	, pntLon	:: Longitude
	, pntEle	:: Maybe Double	-- ^ In meters
	, pntTime	:: Maybe UTCTime
	, pntSpeed	:: Maybe Double	-- ^ Non-standard.  Usually in meters/second.
	}
	deriving (Eq, Ord, Show, Read)

zeroPoint = Point 0 0 Nothing Nothing Nothing

readGPXFile :: FilePath -> IO (Maybe GPX)
readGPXFile fp = runResourceT (parseFile def (fromString fp) $$ conduitGPX)

parseGPX :: (MonadThrow m, MonadBaseControl IO m) => Text -> m (Maybe GPX)
parseGPX t = runResourceT (yield t =$= parseText def $$ conduitGPX)

conduitGPX :: MonadThrow m => Sink Event m (Maybe GPX)
conduitGPX =
	tagPredicate ((== "gpx") . nameLocalName)
			ignoreAttrs
			(\_ -> do
		skipTagAndContents "metadata"
		ts <- many conduitTrack
		return $ GPX ts)

skipTagAndContents :: (MonadThrow m) => Text -> Sink Event m (Maybe ())
skipTagAndContents n = tagPredicate ((== n) . nameLocalName)
				    ignoreAttrs
				    (const $ many (skipElements n) >> return ())

skipElements t = do
	x <- await
	case x of
		Just (EventEndElement n) | nameLocalName n == t -> Done x Nothing
		Nothing -> Done x Nothing
		_ -> return (Just ())

conduitTrack :: MonadThrow m => Sink Event m (Maybe Track)
conduitTrack = do
	tagPredicate ((== "trk") . nameLocalName) ignoreAttrs $ \_ -> do
	n <- join `fmap` tagPredicate (("name" ==) . nameLocalName) ignoreAttrs (const contentMaybe)
	d <- join `fmap` tagPredicate (("desc" ==) . nameLocalName) ignoreAttrs (const contentMaybe)
	segs <- many conduitSegment
	return (Track n d segs)
	

conduitSegment :: MonadThrow m => Sink Event m (Maybe Segment)
conduitSegment = do 
	tagPredicate ((== "trkseg") . nameLocalName) ignoreAttrs $ \_ -> do
	pnts <- (many conduitPoint)
	return (Segment pnts)

conduitPoint :: MonadThrow m => Sink Event m (Maybe Point)
conduitPoint = do
	tagPredicate ((== "trkpt") . nameLocalName )
				(do l <- parseDouble `fmap` requireAttr "lat"
				    g <- parseDouble `fmap` requireAttr "lon"
				    return $ zeroPoint { pntLon = g, pntLat = l })
				parseETS

-- Parse elevation, time, and speed tags
parseETS :: MonadThrow m => Point -> Sink Event m Point
parseETS pnt = do
	let nameParse :: Name -> Maybe (Point -> Text -> Point)
	    nameParse n =
		case nameLocalName n of
			"ele"   -> Just (\p t -> p { pntEle = Just (parseDouble t) })
			"time"  -> Just (\p t -> p { pntTime = (parseUTC t) })
			"speed" -> Just (\p t -> p { pntSpeed  = Just (parseDouble t) } )
			_ -> Nothing
	    handleName :: (MonadThrow m) => pnt -> (pnt -> Text -> pnt) -> Sink Event m pnt
	    handleName p op = fmap (op p) content
	skipTagAndContents "extensions"
	pnt' <- tag nameParse return (handleName pnt)
	case pnt' of
		Nothing -> return pnt
		Just p  -> parseETS p

parseDouble :: Text -> Double
parseDouble l = either (const 0) id (AT.parseOnly AT.double l)

parseUTC :: Text -> Maybe UTCTime
parseUTC = either (const Nothing) id . AT.parseOnly (do 
	yearMonthDay <- AT.manyTill AT.anyChar (AT.char 'T')
	hourMinSec <- AT.manyTill AT.anyChar (AT.choice [AT.char '.', AT.char 'Z'])
	fraction <- AT.choice [AT.manyTill AT.anyChar (AT.char 'Z'), return ""]
	-- The Time package version 1.4 does not handle F T and Q property for buildTime.
	-- return (buildTime defaultTimeLocale [('F', yearMonthDay), ('T', hourMinSec), ('Q', fraction)]))
	return (parseTime defaultTimeLocale "%F %T %Q"
			(unwords [yearMonthDay,hourMinSec,'.':fraction]))
	)
