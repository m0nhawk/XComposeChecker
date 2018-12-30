module XComposeTypes where

import Text.Printf

type Event = String
type Keysym = String

data Target = Output Event (Maybe Keysym)
    deriving (Eq)

instance Show Target where
    show (Output str (Just sym)) = printf "%s, %s" str sym
    show (Output str Nothing) = printf "%s" str

data XCompose = Sequence { events :: [Event], target :: Target }
    deriving (Eq)

instance Show XCompose where
    show (Sequence events target) = show events

extract :: XCompose -> ([Event], [Target])
extract (Sequence events target) = (events, [target])
