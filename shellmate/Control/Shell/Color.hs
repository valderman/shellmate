-- | Color and formatting for terminal output.
--   For Windows systems, any and all formatting are no-ops due to
--   compatibility reasons.
--
--   While formattings can be stacked (i.e. @bold (underline "Hello Joe")@),
--   they do not properly nest: the escape code used to reset formatting at the
--   end of a formatted string removes *all* formatting. Thus,
--   @color Red ("Hello" ++ color Blue "Mike")@ will print \"Hello\" in red and
--   \"Mike\" in blue, but @color Red (color Blue "Hello" ++ "Robert")@ will
--   print \"Robert\" in the terminal's default color, and not in red as one
--   might expect.
module Control.Shell.Color
  ( Color (..)
  , color, background
  , highlight, bold, underline
  ) where
import System.Info (os)

data Color
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Purple
  | Magenta
  | White
    deriving (Show, Read, Eq, Ord, Enum)

colorNum :: Color -> Int
colorNum = fromEnum

-- | Apply the given color to the given string.
color :: Color -> String -> String
color c s = concat [esc ("0" ++ show (30+colorNum c)), s, normal]

-- | Apply the given background color to the given string.
background :: Color -> String -> String
background c s = concat [esc ("0" ++ show (40+colorNum c)), s, normal]

-- | Apply the terminal's default highlighting to the given string.
highlight :: String -> String
highlight s = esc "7" ++ s ++ normal

-- | Output the given string in bold font.
bold :: String -> String
bold s = esc "1" ++ s ++ normal

-- | Underline the given string.
underline :: String -> String
underline s = esc "4" ++ s ++ normal

-- | Escape sequence to reset formatting back to normal.
normal :: String
normal = esc "0"

-- | An escape character.
{-# INLINE esc #-}
esc :: String -> String
esc s
  | os == "mingw32" = ""
  | otherwise       = "\ESC[" ++ s ++ "m"
