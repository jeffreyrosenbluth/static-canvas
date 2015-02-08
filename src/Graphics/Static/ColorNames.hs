{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Static.ColorNames
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Functions to create color styles.
--
-------------------------------------------------------------------------------

module Graphics.Static.ColorNames
  (
   aliceblue
 , antiquewhite
 , aqua
 , aquamarine
 , azure
 , beige
 , bisque
 , black
 , blanchedalmond
 , blue
 , blueviolet
 , brown
 , burlywood
 , cadetblue
 , chartreuse
 , chocolate
 , coral
 , cornflowerblue
 , cornsilk
 , crimson
 , cyan
 , darkblue
 , darkcyan
 , darkgoldenrod
 , darkgray
 , darkgreen
 , darkgrey
 , darkkhaki
 , darkmagenta
 , darkolivegreen
 , darkorange
 , darkorchid
 , darkred
 , darksalmon
 , darkseagreen
 , darkslateblue
 , darkslategray
 , darkslategrey
 , darkturquoise
 , darkviolet
 , deeppink
 , deepskyblue
 , dimgray
 , dimgrey
 , dodgerblue
 , firebrick
 , floralwhite
 , forestgreen
 , fuchsia
 , gainsboro
 , ghostwhite
 , gold
 , goldenrod
 , gray
 , grey
 , green
 , greenyellow
 , honeydew
 , hotpink
 , indianred
 , indigo
 , ivory
 , khaki
 , lavender
 , lavenderblush
 , lawngreen
 , lemonchiffon
 , lightblue
 , lightcoral
 , lightcyan
 , lightgoldenrodyellow
 , lightgray
 , lightgreen
 , lightgrey
 , lightpink
 , lightsalmon
 , lightseagreen
 , lightskyblue
 , lightslategray
 , lightslategrey
 , lightsteelblue
 , lightyellow
 , lime
 , limegreen
 , linen
 , magenta
 , maroon
 , mediumaquamarine
 , mediumblue
 , mediumorchid
 , mediumpurple
 , mediumseagreen
 , mediumslateblue
 , mediumspringgreen
 , mediumturquoise
 , mediumvioletred
 , midnightblue
 , mintcream
 , mistyrose
 , moccasin
 , navajowhite
 , navy
 , oldlace
 , olive
 , olivedrab
 , orange
 , orangered
 , orchid
 , palegoldenrod
 , palegreen
 , paleturquoise
 , palevioletred
 , papayawhip
 , peachpuff
 , peru
 , pink
 , plum
 , powderblue
 , purple
 , red
 , rosybrown
 , royalblue
 , saddlebrown
 , salmon
 , sandybrown
 , seagreen
 , seashell
 , sienna
 , silver
 , skyblue
 , slateblue
 , slategray
 , slategrey
 , snow
 , springgreen
 , steelblue
 , tan
 , teal
 , thistle
 , tomato
 , turquoise
 , violet
 , wheat
 , white
 , whitesmoke
 , yellow
 , yellowgreen
 )
where

import Graphics.Static.Types
import Prelude hiding (tan)

aliceblue :: Style
aliceblue = ColorStyle $ RGB 240 248 255

antiquewhite :: Style
antiquewhite = ColorStyle $ RGB 250 235 215

aqua :: Style
aqua = ColorStyle $ RGB 0 255 255

aquamarine :: Style
aquamarine = ColorStyle $ RGB 127 255 212

azure :: Style
azure = ColorStyle $ RGB 240 255 255

beige :: Style
beige = ColorStyle $ RGB 245 245 220

bisque :: Style
bisque = ColorStyle $ RGB 255 228 196

black :: Style
black = ColorStyle $ RGB 0 0 0

blanchedalmond :: Style
blanchedalmond = ColorStyle $ RGB 255 235 205

blue :: Style
blue = ColorStyle $ RGB 0 0 255

blueviolet :: Style
blueviolet = ColorStyle $ RGB 138 43 226

brown :: Style
brown = ColorStyle $ RGB 165 42 42

burlywood :: Style
burlywood = ColorStyle $ RGB 222 184 135

cadetblue :: Style
cadetblue = ColorStyle $ RGB 95 158 160

chartreuse :: Style
chartreuse = ColorStyle $ RGB 127 255 0

chocolate :: Style
chocolate = ColorStyle $ RGB 210 105 30

coral :: Style
coral = ColorStyle $ RGB 255 127 80

cornflowerblue :: Style
cornflowerblue = ColorStyle $ RGB 100 149 237

cornsilk :: Style
cornsilk = ColorStyle $ RGB 255 248 220

crimson :: Style
crimson = ColorStyle $ RGB 220 20 60

cyan :: Style
cyan = ColorStyle $ RGB 0 255 255

darkblue :: Style
darkblue = ColorStyle $ RGB 0 0 139

darkcyan :: Style
darkcyan = ColorStyle $ RGB 0 139 139

darkgoldenrod :: Style
darkgoldenrod = ColorStyle $ RGB 184 134 11

darkgray :: Style
darkgray = ColorStyle $ RGB 169 169 169

darkgreen :: Style
darkgreen = ColorStyle $ RGB 0 100 0

darkgrey :: Style
darkgrey = ColorStyle $ RGB 169 169 169

darkkhaki :: Style
darkkhaki = ColorStyle $ RGB 189 183 107

darkmagenta :: Style
darkmagenta = ColorStyle $ RGB 139 0 139

darkolivegreen :: Style
darkolivegreen = ColorStyle $ RGB 85 107 47

darkorange :: Style
darkorange = ColorStyle $ RGB 255 140 0

darkorchid :: Style
darkorchid = ColorStyle $ RGB 153 50 204

darkred :: Style
darkred = ColorStyle $ RGB 139 0 0

darksalmon :: Style
darksalmon = ColorStyle $ RGB 233 150 122

darkseagreen :: Style
darkseagreen = ColorStyle $ RGB 143 188 143

darkslateblue :: Style
darkslateblue = ColorStyle $ RGB 72 61 139

darkslategray :: Style
darkslategray = ColorStyle $ RGB 47 79 79

darkslategrey :: Style
darkslategrey = ColorStyle $ RGB 47 79 79

darkturquoise :: Style
darkturquoise = ColorStyle $ RGB 0 206 209

darkviolet :: Style
darkviolet = ColorStyle $ RGB 148 0 211

deeppink :: Style
deeppink = ColorStyle $ RGB 255 20 147

deepskyblue :: Style
deepskyblue = ColorStyle $ RGB 0 191 255

dimgray :: Style
dimgray = ColorStyle $ RGB 105 105 105

dimgrey :: Style
dimgrey = ColorStyle $ RGB 105 105 105

dodgerblue :: Style
dodgerblue = ColorStyle $ RGB 30 144 255

firebrick :: Style
firebrick = ColorStyle $ RGB 178 34 34

floralwhite :: Style
floralwhite = ColorStyle $ RGB 255 250 240

forestgreen :: Style
forestgreen = ColorStyle $ RGB 34 139 34

fuchsia :: Style
fuchsia = ColorStyle $ RGB 255 0 255

gainsboro :: Style
gainsboro = ColorStyle $ RGB 220 220 220

ghostwhite :: Style
ghostwhite = ColorStyle $ RGB 248 248 255

gold :: Style
gold = ColorStyle $ RGB 255 215 0

goldenrod :: Style
goldenrod = ColorStyle $ RGB 218 165 32

gray :: Style
gray = ColorStyle $ RGB 128 128 128

grey :: Style
grey = ColorStyle $ RGB 128 128 128

green :: Style
green = ColorStyle $ RGB 0 128 0

greenyellow :: Style
greenyellow = ColorStyle $ RGB 173 255 47

honeydew :: Style
honeydew = ColorStyle $ RGB 240 255 240

hotpink :: Style
hotpink = ColorStyle $ RGB 255 105 180

indianred :: Style
indianred = ColorStyle $ RGB 205 92 92

indigo :: Style
indigo = ColorStyle $ RGB 75 0 130

ivory :: Style
ivory = ColorStyle $ RGB 255 255 240

khaki :: Style
khaki = ColorStyle $ RGB 240 230 140

lavender :: Style
lavender = ColorStyle $ RGB 230 230 250

lavenderblush :: Style
lavenderblush = ColorStyle $ RGB 255 240 245

lawngreen :: Style
lawngreen = ColorStyle $ RGB 124 252 0

lemonchiffon :: Style
lemonchiffon = ColorStyle $ RGB 255 250 205

lightblue :: Style
lightblue = ColorStyle $ RGB 173 216 230

lightcoral :: Style
lightcoral = ColorStyle $ RGB 240 128 128

lightcyan :: Style
lightcyan = ColorStyle $ RGB 224 255 255

lightgoldenrodyellow :: Style
lightgoldenrodyellow = ColorStyle $ RGB 250 250 210

lightgray :: Style
lightgray = ColorStyle $ RGB 211 211 211

lightgreen :: Style
lightgreen = ColorStyle $ RGB 144 238 144

lightgrey :: Style
lightgrey = ColorStyle $ RGB 211 211 211

lightpink :: Style
lightpink = ColorStyle $ RGB 255 182 193

lightsalmon :: Style
lightsalmon = ColorStyle $ RGB 255 160 122

lightseagreen :: Style
lightseagreen = ColorStyle $ RGB 32 178 170

lightskyblue :: Style
lightskyblue = ColorStyle $ RGB 135 206 250

lightslategray :: Style
lightslategray = ColorStyle $ RGB 119 136 153

lightslategrey :: Style
lightslategrey = ColorStyle $ RGB 119 136 153

lightsteelblue :: Style
lightsteelblue = ColorStyle $ RGB 176 196 222

lightyellow :: Style
lightyellow = ColorStyle $ RGB 255 255 224

lime :: Style
lime = ColorStyle $ RGB 0 255 0

limegreen :: Style
limegreen = ColorStyle $ RGB 50 205 50

linen :: Style
linen = ColorStyle $ RGB 250 240 230

magenta :: Style
magenta = ColorStyle $ RGB 255 0 255

maroon :: Style
maroon = ColorStyle $ RGB 128 0 0

mediumaquamarine :: Style
mediumaquamarine = ColorStyle $ RGB 102 205 170

mediumblue :: Style
mediumblue = ColorStyle $ RGB 0 0 205

mediumorchid :: Style
mediumorchid = ColorStyle $ RGB 186 85 211

mediumpurple :: Style
mediumpurple = ColorStyle $ RGB 147 112 219

mediumseagreen :: Style
mediumseagreen = ColorStyle $ RGB 60 179 113

mediumslateblue :: Style
mediumslateblue = ColorStyle $ RGB 123 104 238

mediumspringgreen :: Style
mediumspringgreen = ColorStyle $ RGB 0 250 154

mediumturquoise :: Style
mediumturquoise = ColorStyle $ RGB 72 209 204

mediumvioletred :: Style
mediumvioletred = ColorStyle $ RGB 199 21 133

midnightblue :: Style
midnightblue = ColorStyle $ RGB 25 25 112

mintcream :: Style
mintcream = ColorStyle $ RGB 245 255 250

mistyrose :: Style
mistyrose = ColorStyle $ RGB 255 228 225

moccasin :: Style
moccasin = ColorStyle $ RGB 255 228 181

navajowhite :: Style
navajowhite = ColorStyle $ RGB 255 222 173

navy :: Style
navy = ColorStyle $ RGB 0 0 128

oldlace :: Style
oldlace = ColorStyle $ RGB 253 245 230

olive :: Style
olive = ColorStyle $ RGB 128 128 0

olivedrab :: Style
olivedrab = ColorStyle $ RGB 107 142 35

orange :: Style
orange = ColorStyle $ RGB 255 165 0

orangered :: Style
orangered = ColorStyle $ RGB 255 69 0

orchid :: Style
orchid = ColorStyle $ RGB 218 112 214

palegoldenrod :: Style
palegoldenrod = ColorStyle $ RGB 238 232 170

palegreen :: Style
palegreen = ColorStyle $ RGB 152 251 152

paleturquoise :: Style
paleturquoise = ColorStyle $ RGB 175 238 238

palevioletred :: Style
palevioletred = ColorStyle $ RGB 219 112 147

papayawhip :: Style
papayawhip = ColorStyle $ RGB 255 239 213

peachpuff :: Style
peachpuff = ColorStyle $ RGB 255 218 185

peru :: Style
peru = ColorStyle $ RGB 205 133 63

pink :: Style
pink = ColorStyle $ RGB 255 192 203

plum :: Style
plum = ColorStyle $ RGB 221 160 221

powderblue :: Style
powderblue = ColorStyle $ RGB 176 224 230

purple :: Style
purple = ColorStyle $ RGB 128 0 128

red :: Style
red = ColorStyle $ RGB 255 0 0

rosybrown :: Style
rosybrown = ColorStyle $ RGB 188 143 143

royalblue :: Style
royalblue = ColorStyle $ RGB 65 105 225

saddlebrown :: Style
saddlebrown = ColorStyle $ RGB 139 69 19

salmon :: Style
salmon = ColorStyle $ RGB 250 128 114

sandybrown :: Style
sandybrown = ColorStyle $ RGB 244 164 96

seagreen :: Style
seagreen = ColorStyle $ RGB 46 139 87

seashell :: Style
seashell = ColorStyle $ RGB 255 245 238

sienna :: Style
sienna = ColorStyle $ RGB 160 82 45

silver :: Style
silver = ColorStyle $ RGB 192 192 192

skyblue :: Style
skyblue = ColorStyle $ RGB 135 206 235

slateblue :: Style
slateblue = ColorStyle $ RGB 106 90 205

slategray :: Style
slategray = ColorStyle $ RGB 112 128 144

slategrey :: Style
slategrey = ColorStyle $ RGB 112 128 144

snow :: Style
snow = ColorStyle $ RGB 255 250 250

springgreen :: Style
springgreen = ColorStyle $ RGB 0 255 127

steelblue :: Style
steelblue = ColorStyle $ RGB 70 130 180

tan :: Style
tan = ColorStyle $ RGB 210 180 140

teal :: Style
teal = ColorStyle $ RGB 0 128 128

thistle :: Style
thistle = ColorStyle $ RGB 216 191 216

tomato :: Style
tomato = ColorStyle $ RGB 255 99 71

turquoise :: Style
turquoise = ColorStyle $ RGB 64 224 208

violet :: Style
violet = ColorStyle $ RGB 238 130 238

wheat :: Style
wheat = ColorStyle $ RGB 245 222 179

white :: Style
white = ColorStyle $ RGB 255 255 255

whitesmoke :: Style
whitesmoke = ColorStyle $ RGB 245 245 245

yellow :: Style
yellow = ColorStyle $ RGB 255 255 0

yellowgreen :: Style
yellowgreen = ColorStyle $ RGB 154 205 50
