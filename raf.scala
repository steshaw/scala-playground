def raf(s: String) = (s :\ 0)(_ - 'a' + 1 + _ * 26)

val prop_1 = raf("a") == 1
val prop_2 = raf("b") == 2
val prop_3 = raf("z") == 26
val prop_4 = raf("aa") == 27
val prop_5 = raf("az") == 52
val prop_6 = raf("ba") == 53
val prop_7 = raf("bz") == 78
val prop_8 = raf("aaa") == 703
val prop_9 = raf("aaz") == 728
val prop_10 = raf("aza") == 1353
