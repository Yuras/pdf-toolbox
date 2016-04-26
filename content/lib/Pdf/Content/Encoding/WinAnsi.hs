{-# LANGUAGE OverloadedStrings #-}

module Pdf.Content.Encoding.WinAnsi
(
  encoding
)
where

import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

encoding :: Map Word8 Text
encoding = Map.fromList $ zip codes chars

chars :: [Text]
chars = [
  "A",
  "Æ",
  "Á",
  "Â",
  "Ä",
  "À",
  "Å",
  "Ã",
  "B",
  "C",
  "Ç",
  "D",
  "E",
  "É",
  "Ê",
  "Ë",
  "È",
  "Ð",
  "€",
  "F",
  "G",
  "H",
  "I",
  "Í",
  "Î",
  "Ï",
  "Ì",
  "J",
  "K",
  "L",
  "M",
  "N",
  "Ñ",
  "O",
  "Œ",
  "Ó",
  "Ô",
  "Ö",
  "Ò",
  "Ø",
  "Õ",
  "P",
  "Q",
  "R",
  "S",
  "Š",
  "T",
  "Þ",
  "U",
  "Ú",
  "Û",
  "Ü",
  "Ù",
  "V",
  "W",
  "X",
  "Y",
  "Ý",
  "Ÿ",
  "Z",
  "Ž",
  "a",
  "á",
  "â",
  "´",
  "ä",
  "æ",
  "à",
  "&",
  "å",
  "^",
  "~",
  "*",
  "@",
  "ã",
  "b",
  "\\",
  "|",
  "{",
  "}",
  "[",
  "]",
  "¦",
  "•",
  "c",
  "ç",
  " ̧",
  "¢",
  "ˆ",
  ":",
  ",",
  "©",
  "¤",
  "d",
  "†",
  "‡",
  "°",
  "¨",
  "÷",
  "$",
  "e",
  "é",
  "ê",
  "ë",
  "è",
  "8",
  "…",
  "—",
  "–",
  "=",
  "ð",
  "!",
  "¡",
  "f",
  "5",
  "ƒ",
  "4",
  "g",
  "ß",
  "`",
  ">",
  "«",
  "»",
  "‹",
  "›",
  "h",
  "-",
  "i",
  "í",
  "î",
  "ï",
  "ì",
  "j",
  "k",
  "l",
  "<",
  "¬",
  "m",
  " ̄",
  "μ",
  "×",
  "n",
  "9",
  "ñ",
  "#",
  "o",
  "ó",
  "ô",
  "ö",
  "œ",
  "ò",
  "1",
  "½",
  "¼",
  "¹",
  "ª",
  "º",
  "ø",
  "õ",
  "p",
  "¶",
  "(",
  ")",
  "%",
  ".",
  "·",
  "‰",
  "+",
  "±",
  "q",
  "?",
  "¿",
  "\"",
  "„",
  "“",
  "”",
  "‘",
  "’",
  "‚",
  "'",
  "r",
  "®",
  "s",
  "š",
  "§",
  ";",
  "7",
  "6",
  "/",
  " ",
  "£",
  "t",
  "þ",
  "3",
  "¾",
  "³",
  "~",
  "™",
  "2",
  "²",
  "u",
  "ú",
  "û",
  "ü",
  "ù",
  "_",
  "v",
  "w",
  "x",
  "y",
  "ý",
  "ÿ",
  "¥",
  "z",
  "ž",
  "0"
  ]

codes :: [Word8]
codes = [
  0o101,
  0o306,
  0o301,
  0o302,
  0o304,
  0o300,
  0o305,
  0o303,
  0o102,
  0o103,
  0o307,
  0o104,
  0o105,
  0o311,
  0o312,
  0o313,
  0o310,
  0o320,
  0o200,
  0o106,
  0o107,
  0o110,
  0o111,
  0o315,
  0o316,
  0o317,
  0o314,
  0o112,
  0o113,
  0o114,
  0o115,
  0o116,
  0o321,
  0o117,
  0o214,
  0o323,
  0o324,
  0o326,
  0o322,
  0o330,
  0o325,
  0o120,
  0o121,
  0o122,
  0o123,
  0o212,
  0o124,
  0o336,
  0o125,
  0o332,
  0o333,
  0o334,
  0o331,
  0o126,
  0o127,
  0o130,
  0o131,
  0o335,
  0o237,
  0o132,
  0o216,
  0o141,
  0o341,
  0o342,
  0o264,
  0o344,
  0o346,
  0o340,
  0o046,
  0o345,
  0o136,
  0o176,
  0o052,
  0o100,
  0o343,
  0o142,
  0o134,
  0o174,
  0o173,
  0o175,
  0o133,
  0o135,
  0o246,
  0o225,
  0o143,
  0o347,
  0o270,
  0o242,
  0o210,
  0o072,
  0o054,
  0o251,
  0o244,
  0o144,
  0o206,
  0o207,
  0o260,
  0o250,
  0o367,
  0o044,
  0o145,
  0o351,
  0o352,
  0o353,
  0o350,
  0o070,
  0o205,
  0o227,
  0o226,
  0o075,
  0o360,
  0o041,
  0o241,
  0o146,
  0o065,
  0o203,
  0o064,
  0o147,
  0o337,
  0o140,
  0o076,
  0o253,
  0o273,
  0o213,
  0o233,
  0o150,
  0o055,
  0o151,
  0o355,
  0o356,
  0o357,
  0o354,
  0o152,
  0o153,
  0o154,
  0o074,
  0o254,
  0o155,
  0o257,
  0o265,
  0o327,
  0o156,
  0o071,
  0o361,
  0o043,
  0o157,
  0o363,
  0o364,
  0o366,
  0o234,
  0o362,
  0o061,
  0o275,
  0o274,
  0o271,
  0o252,
  0o272,
  0o370,
  0o365,
  0o160,
  0o266,
  0o050,
  0o051,
  0o045,
  0o056,
  0o267,
  0o211,
  0o053,
  0o261,
  0o161,
  0o077,
  0o277,
  0o042,
  0o204,
  0o223,
  0o224,
  0o221,
  0o222,
  0o202,
  0o047,
  0o162,
  0o256,
  0o163,
  0o232,
  0o247,
  0o073,
  0o067,
  0o066,
  0o057,
  0o040,
  0o243,
  0o164,
  0o376,
  0o063,
  0o276,
  0o263,
  0o230,
  0o231,
  0o062,
  0o262,
  0o165,
  0o372,
  0o373,
  0o374,
  0o371,
  0o137,
  0o166,
  0o167,
  0o170,
  0o171,
  0o375,
  0o377,
  0o245,
  0o172,
  0o236,
  0o060
  ]