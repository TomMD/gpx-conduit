{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Geo.GPX.Conduit
	( Point(..), HasLat(..), HasLon(..), HasSpeed(..), HasTime(..), HasEle(..)
	, Track(..), GPX(..), Segment(..), TrkPnt(..)
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
import Data.Monoid
import Data.String
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, buildTime, parseTime)
import Data.XML.Types
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import qualified Data.Attoparsec.Text as AT

class HasLat a where
	setLat :: a -> Double -> a
class HasLon a where
	setLon :: a -> Double -> a

class (Monoid a, HasEle a, HasTime a, HasSpeed a, HasLat a, HasLon a, Show a) => Point a

class HasEle a where
	setEle :: a -> Double -> a

class HasTime a where
	setTime :: a -> UTCTime -> a

class HasSpeed a where
	setSpeed :: a -> Double -> a

gpxNS n = Name n (Just "http://www.topografix.com/GPX/1/0") Nothing

-- |A GPX file usually is a single track (but can be many)
-- with one or more segments and many points in each segment.
data GPX pnt = GPX [Track pnt]
	deriving (Eq, Ord, Show, Read)
data Track pnt = Track 
	{ trkName		:: Maybe Text
	, trkDescription	:: Maybe Text
	, segements		:: [Segment pnt]
	}
	deriving (Eq, Ord, Show, Read)

data Segment pnt = Segment [pnt]
	deriving (Eq, Ord, Show, Read)

-- |Track point is a full-fledged representation of all the data
-- available in most GPS loggers.  It is possible you don't want
-- all this data and can just made do with coordinates (via 'Pnt')
-- or a custom derivative.
data TrkPnt = TrkPnt
	{ coordinate	:: (Double, Double)
	, elevation	:: Maybe Double	-- ^ In meters
	, time		:: Maybe UTCTime
	, speed		:: Maybe Double -- ^ Non-standard.  Usually in meters/second.
	}
	deriving (Eq, Ord, Show, Read)

instance HasLat TrkPnt where
	setLat a l = a { coordinate = (l,snd $ coordinate a) }

instance HasLon TrkPnt where
	setLon a g = a { coordinate = (fst $ coordinate a, g) }

instance HasEle TrkPnt where
	setEle a e = a { elevation = Just e }

instance HasTime TrkPnt where
	setTime a e = a { time = Just e }

instance HasSpeed TrkPnt where
	setSpeed a e = a { speed = Just e }

instance Monoid TrkPnt where
	mempty  = TrkPnt (0,0) Nothing Nothing Nothing
	mappend = error "WTF, why am I using Monoid?"

instance Point TrkPnt

readGPXFile :: (Point pnt) => FilePath -> IO (Maybe (GPX pnt))
readGPXFile fp = runResourceT (parseFile def (fromString fp) $$ conduitGPX)

parseGPX :: (Point pnt, MonadThrow m, MonadBaseControl IO m) => Text -> m (Maybe (GPX pnt))
parseGPX t = runResourceT (yield t =$= parseText def $$ conduitGPX)

conduitGPX :: (Point pnt, MonadThrow m) => Sink Event m (Maybe (GPX pnt))
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

conduitTrack :: (Point pnt, MonadThrow m) => Sink Event m (Maybe (Track pnt))
conduitTrack = do
	tagPredicate ((== "trk") . nameLocalName) ignoreAttrs $ \_ -> do
	n <- join `fmap` tagPredicate (("name" ==) . nameLocalName) ignoreAttrs (const contentMaybe)
	d <- join `fmap` tagPredicate (("desc" ==) . nameLocalName) ignoreAttrs (const contentMaybe)
	segs <- many conduitSegment
	return (Track n d segs)
	

conduitSegment :: (Point pnt, MonadThrow m) => Sink Event m (Maybe (Segment pnt))
conduitSegment = do 
	tagPredicate ((== "trkseg") . nameLocalName) ignoreAttrs $ \_ -> do
	pnts <- (many conduitPoint)
	return (Segment pnts)

conduitPoint   :: (Point pnt, MonadThrow m) => Sink Event m (Maybe pnt)
conduitPoint = do
	tagPredicate ((== "trkpt") . nameLocalName )
				(do l <- parseDouble `fmap` requireAttr "lat"
				    g <- parseDouble `fmap` requireAttr "lon"
				    return $ setLon (setLat mempty l) g)
				parseETS

-- Parse elevation, time, and speed tags
parseETS :: (Point pnt, MonadThrow m) => pnt -> Sink Event m pnt
parseETS pnt = do
	let nameParse :: Point pnt => Name -> Maybe (pnt -> Text -> pnt)
	    nameParse n =
		case nameLocalName n of
			"ele"   -> Just (\p t -> setEle p (parseDouble t))
			"time"  -> Just (\p t -> maybe p (setTime p) (parseUTC t))
			"speed" -> Just (\p t -> setSpeed p (parseDouble t))
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
