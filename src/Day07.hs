module Day07 where

import AOCUtils
import Data.List (maximum, minimum)

test1 = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

input = [1101, 1, 29, 67, 1102, 0, 1, 65, 1008, 65, 35, 66, 1005, 66, 28, 1, 67, 65, 20, 4, 0, 1001, 65, 1, 65, 1106, 0, 8, 99, 35, 67, 101, 99, 105, 32, 110, 39, 101, 115, 116, 32, 112, 97, 115, 32, 117, 110, 101, 32, 105, 110, 116, 99, 111, 100, 101, 32, 112, 114, 111, 103, 114, 97, 109, 10, 104, 709, 235, 932, 796, 119, 731, 120, 462, 837, 152, 100, 214, 411, 71, 28, 91, 1231, 401, 417, 900, 1733, 683, 107, 101, 582, 159, 72, 11, 514, 566, 1054, 638, 774, 413, 222, 568, 526, 53, 303, 635, 664, 21, 67, 133, 913, 292, 95, 963, 7, 440, 78, 1455, 283, 104, 106, 431, 749, 468, 325, 319, 922, 433, 2, 108, 10, 95, 89, 1074, 190, 91, 52, 1313, 242, 475, 964, 395, 437, 604, 277, 525, 162, 191, 923, 124, 219, 35, 707, 18, 1123, 30, 1163, 41, 467, 290, 420, 393, 279, 159, 59, 206, 160, 592, 52, 267, 696, 218, 151, 807, 301, 262, 424, 102, 1871, 406, 443, 149, 1035, 1286, 141, 403, 37, 872, 1031, 788, 1138, 962, 89, 357, 885, 367, 499, 175, 556, 157, 1571, 759, 989, 2, 1305, 38, 132, 579, 335, 1452, 171, 627, 175, 557, 1108, 274, 263, 1036, 482, 432, 21, 1769, 63, 17, 731, 83, 1329, 131, 101, 6, 1135, 317, 110, 41, 706, 142, 292, 473, 783, 566, 230, 34, 243, 405, 32, 55, 987, 646, 62, 92, 52, 597, 48, 319, 1159, 827, 769, 125, 420, 308, 60, 345, 461, 159, 229, 1064, 298, 1200, 861, 364, 1051, 26, 584, 702, 1717, 19, 61, 35, 581, 297, 63, 945, 1469, 3, 1168, 588, 339, 1182, 1357, 823, 293, 85, 77, 40, 847, 235, 326, 364, 474, 619, 732, 105, 517, 153, 32, 198, 65, 1026, 278, 1170, 1092, 941, 1747, 147, 124, 86, 975, 856, 1173, 350, 51, 206, 17, 319, 111, 89, 49, 94, 97, 319, 887, 307, 991, 372, 175, 409, 359, 129, 1242, 1409, 644, 205, 424, 1644, 1515, 1134, 299, 571, 78, 695, 101, 365, 385, 1188, 1162, 17, 106, 972, 198, 381, 656, 9, 291, 1415, 95, 1048, 541, 162, 1408, 776, 308, 308, 278, 495, 1679, 302, 1, 138, 7, 382, 981, 455, 719, 607, 541, 136, 449, 1059, 227, 453, 1614, 315, 283, 583, 143, 1806, 499, 1062, 1115, 219, 22, 160, 650, 326, 70, 316, 4, 200, 1542, 1554, 266, 377, 123, 1302, 1814, 139, 383, 304, 324, 167, 850, 63, 306, 365, 83, 490, 201, 41, 352, 593, 118, 45, 554, 75, 1352, 49, 92, 1399, 231, 104, 289, 134, 1307, 9, 247, 883, 999, 1069, 301, 307, 743, 729, 365, 3, 1251, 415, 304, 40, 330, 293, 72, 393, 562, 12, 183, 41, 229, 306, 209, 281, 1557, 126, 1119, 286, 12, 18, 1010, 729, 741, 738, 44, 615, 748, 193, 598, 423, 68, 174, 36, 70, 1455, 325, 0, 229, 409, 211, 423, 183, 271, 233, 952, 601, 320, 109, 1051, 502, 684, 546, 239, 1279, 215, 1497, 125, 427, 489, 500, 10, 415, 189, 630, 261, 63, 102, 1459, 79, 1113, 199, 684, 251, 801, 573, 16, 99, 1805, 716, 45, 18, 631, 290, 508, 67, 0, 2, 461, 63, 325, 607, 697, 812, 58, 262, 316, 754, 37, 848, 60, 101, 202, 1000, 128, 20, 355, 313, 140, 279, 833, 168, 1197, 1668, 1062, 255, 626, 205, 326, 321, 591, 243, 1093, 38, 26, 986, 508, 424, 229, 143, 163, 1173, 608, 349, 468, 571, 95, 140, 10, 279, 112, 12, 552, 0, 326, 258, 195, 113, 470, 651, 1298, 439, 53, 134, 151, 447, 299, 905, 40, 19, 23, 719, 10, 557, 1339, 474, 119, 329, 1487, 55, 602, 255, 284, 162, 783, 524, 452, 899, 327, 236, 1826, 295, 265, 598, 1825, 220, 517, 592, 862, 57, 762, 465, 313, 499, 694, 1328, 5, 81, 137, 936, 46, 852, 448, 1301, 1101, 35, 77, 1283, 11, 193, 937, 757, 9, 208, 160, 736, 54, 1574, 87, 546, 51, 373, 29, 25, 79, 1091, 1432, 125, 158, 728, 835, 1, 614, 172, 389, 173, 808, 1788, 223, 125, 135, 25, 318, 6, 691, 724, 104, 467, 269, 66, 39, 362, 155, 100, 165, 425, 1844, 41, 284, 602, 226, 294, 172, 942, 223, 1, 14, 199, 1292, 235, 434, 612, 980, 139, 61, 735, 276, 62, 864, 56, 460, 652, 713, 98, 408, 1314, 320, 116, 171, 114, 93, 804, 260, 339, 451, 392, 31, 156, 176, 60, 279, 1272, 271, 1494, 164, 170, 451, 857, 317, 1379, 44, 166, 115, 823, 349, 4, 352, 54, 389, 1548, 302, 454, 1412, 231, 86, 2, 239, 117, 272, 462, 1030, 171, 14, 301, 249, 66, 114, 360, 676, 510, 1149, 58, 91, 46, 317, 425, 1219, 64, 1538, 638, 1227, 62, 214, 386, 1148, 180, 327, 1084, 27, 886, 565, 157, 215, 313, 462, 129, 1293, 397, 823, 753, 50, 539, 705, 813, 531, 779, 30, 501, 1072, 1125, 2, 1640, 691, 1140, 573, 1081, 1232, 488, 721, 113, 113, 127, 270, 1095, 6, 68, 301, 465, 43, 322, 88, 892, 841, 323, 981, 642, 1231, 346, 247, 623, 161, 1291, 76, 709, 1148, 306, 87, 1147, 645, 818, 1520, 692, 352, 133, 71, 443, 1190, 271, 1171, 42, 980, 589, 493, 312, 211, 78, 1369, 329, 304, 1057, 202, 405, 1294, 49, 363, 835, 1295, 53, 530, 20, 24, 947, 885, 1054, 252, 1170, 337, 460, 476, 50, 657, 1201, 715, 555, 132, 344, 26, 1369, 675, 234, 1362, 875, 224, 1910, 338, 175, 93, 595, 27, 211, 210, 787, 790, 990, 425, 1176, 48, 43, 201, 15, 279, 344, 203, 15, 790, 255, 125, 159, 45, 162, 290, 198, 796, 52, 146, 512, 200, 1051, 1850, 1202, 775, 237, 767, 13, 180, 294, 26, 896, 1263, 749, 1239, 1621, 642, 607, 88, 123, 651, 630, 1178, 135, 5, 686, 989, 1250, 60, 1266, 360, 49, 1089, 175, 355, 162, 375, 350, 1203]

spec = hspec do
  describe "part 1" do
    it "should take 37 fuel for the crabs to align themselves at position 2" do
      minHorizontalAlignCost naiveCost test1 `shouldBe` 37
  describe "part 2" do
    it "should take 168 fuel for the crabs to align themselves at position 5 with increasing cost" do
      minHorizontalAlignCost increasingCost test1 `shouldBe` 168

naiveCost crab pos = abs (crab - pos)

minHorizontalAlignCost cost crabs =
  minimum $
    map
      (foldMap (\crab -> Sum . cost crab) crabs)
      [minimum crabs .. maximum crabs]

increasingCost crab pos = let n = abs (crab - pos) in (n * (n + 1)) `div` 2 -- sum [1..n]

part1 = minHorizontalAlignCost naiveCost input
part2 = minHorizontalAlignCost increasingCost input
