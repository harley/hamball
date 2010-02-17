module Colors where

import Data.Array
import Graphics.Rendering.OpenGL hiding (Red, Blue, Green, colorTable, Color)

------------------------------------------------------------------------------
-- Color definitions.  PLEASE use 'colorf ColarName'
------------------------------------------------------------------------------
colorf :: Color -> Color4 GLfloat
colorf x = toF (colorTable ! x)

toF :: Color4 Int -> Color4 GLfloat
toF (Color4 r g b a) = Color4 (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255) (fromIntegral a)


-- Pretty arbitrary selection of colours.
data Color =
-- Basic colours.
      Black
    | Blue
    | Green
    | Cyan
    | Red
    | Magenta
    | Yellow
    | White
-- Various greys.
    | DarkGrey
    | DimGrey
    | Grey
    | LightGrey
    | DarkSlateGrey
    | SlateGrey
    | LightSlateGrey
-- Various blues/cyan.
    | MidnightBlue
    | NavyBlue
    | CornflowerBlue
    | DarkSlateBlue
    | SlateBlue
    | LightSlateBlue
    | MediumBlue
    | RoyalBlue
    | DeepSkyBlue
    | SteelBlue
    | CadetBlue
-- Various greens/olive greens/khaki.
    | DarkGreen
    | DarkOliveGreen
    | SeaGreen
    | MediumSeaGreen
    | LawnGreen
    | LimeGreen
    | ForestGreen
    | OliveDrab
    | DarkKhaki
    | Khaki
-- Various oranges/browns.
    | Goldenrod
    | DarkGoldenrod
    | SaddleBrown
    | Orange
-- Various violets/purples.
    | Maroon
    | MediumVioletRed
    | VioletRed
    | Violet
    | Plum
    | Orchid
    | MediumOrchid
    | DarkOrchid
    | BlueViolet
    | Purple
    deriving (Eq, Ord, Bounded, Enum, Ix)

colorTable :: Array Colors.Color (Color4 Int)
colorTable = array (minBound, maxBound) colorList
 where 
  --colorList :: [(Color, Color4 t)]
  colorList =
    [   -- Basic colours.
    (Black,			Color4   0   0   0 1),
	(Blue,			Color4   0   0 255 1),
	(Green,			Color4   0 255   0 1),
	(Cyan,			Color4   0 255 255 1),
	(Red,			Color4 255   0   0  1),
	(Magenta,		Color4 255   0 255 1),
	(Yellow,		Color4 255 255   0 1),
	(White,			Color4 255 255 255 1),

	-- Various greys.
	(DarkGrey,		Color4  64  64  64 1),
	(DimGrey,		Color4 105 105 105 1),
	(Grey,			Color4 190 190 190 1),
	(LightGrey,		Color4 211 211 211 1),
	(DarkSlateGrey,		Color4  47  79  79 1),
	(SlateGrey,		Color4 112 128 144 1),
	(LightSlateGrey,	Color4 119 136 153 1),

	-- Various blues/cyan.
	(MidnightBlue,		Color4  25  25 112 1),
	(NavyBlue,		Color4   0   0 128 1),
	(CornflowerBlue,	Color4 100 149 237 1),
	(DarkSlateBlue,		Color4  72  61 139 1),
	(SlateBlue,		Color4 106  90 205 1),
	(LightSlateBlue,	Color4 132 112 255 1),
	(MediumBlue,		Color4   0   0 205 1),
	(RoyalBlue,		Color4  65 105 225 1),
	(DeepSkyBlue,		Color4   0 191 255 1),
	(SteelBlue,		Color4  70 130 180 1),
	(CadetBlue,		Color4  95 158 160 1),

	-- Various greens/olive greens/khaki.
	(DarkGreen,		Color4   0 100   0 1),
	(DarkOliveGreen,	Color4  85 107  47 1),
	(SeaGreen,		Color4  46 139  87 1),
	(MediumSeaGreen,	Color4  60 179 113 1),
	(LawnGreen,		Color4 124 252   0 1),
	(LimeGreen,		Color4  50 205  50 1),
	(ForestGreen,		Color4  34 139  34 1),
	(OliveDrab,		Color4 107 142  35 1),
	(DarkKhaki,		Color4 189 183 107 1),
	(Khaki,			Color4 240 230 140 1),

	-- Various oranges/browns.
	(Goldenrod,		Color4 218 165  32 1),
	(DarkGoldenrod,		Color4 184 134  11 1),
	(SaddleBrown,		Color4 139  69  19 1),
	(Orange,		Color4 255 165   0 1),

	-- Various violets/purples.
	(Maroon,		Color4 176  48  96 1),
	(MediumVioletRed,	Color4 199  21 133 1),
	(VioletRed,		Color4 208  32 144 1),
	(Violet,		Color4 238 130 238 1),
	(Plum,			Color4 221 160 221 1),
	(Orchid,		Color4 218 112 214 1),
	(MediumOrchid,		Color4 186  85 211 1),
	(DarkOrchid,		Color4 153  50 204 1),
	(BlueViolet,		Color4 138  43 226 1),
	(Purple,		Color4 160  32 240 1)
    ]
