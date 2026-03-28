# Named vector mapping FF48 code -> industry label
ffi48_labels <- c(
    "1"  = "Agric", "2"  = "Food",  "3"  = "Soda",  "4"  = "Beer",  "5"  = "Smoke",
    "6"  = "Toys",  "7"  = "Fun",   "8"  = "Books", "9"  = "Hshld", "10" = "Clths",
    "11" = "Hlth",  "12" = "MedEq", "13" = "Drugs", "14" = "Chems", "15" = "Rubbr",
    "16" = "Txtls", "17" = "BldMt", "18" = "Cnstr", "19" = "Steel", "20" = "FabPr",
    "21" = "Mach",  "22" = "ElcEq", "23" = "Autos", "24" = "Aero",  "25" = "Ships",
    "26" = "Guns",  "27" = "Gold",  "28" = "Mines", "29" = "Coal",  "30" = "Oil",
    "31" = "Util",  "32" = "Telcm", "33" = "PerSv", "34" = "BusSv", "35" = "Comps",
    "36" = "Chips", "37" = "LabEq", "38" = "Paper", "39" = "Boxes", "40" = "Trans",
    "41" = "Whlsl", "42" = "Rtail", "43" = "Meals", "44" = "Banks", "45" = "Insur",
    "46" = "RlEst", "47" = "Fin",   "48" = "Other"
)

# Assign Fama-French 48 industry code from a vector of SIC codes
# Returns integer vector (1-48, NA if unclassified)
assign_ffi48 <- function(sic) {

    case_when(
        # 1: Agric
        (sic >= 100 & sic <= 299) | (sic >= 700 & sic <= 799) |
            (sic >= 910 & sic <= 919) | sic == 2048 ~ 1L,
        # 2: Food
        (sic >= 2000 & sic <= 2046) | (sic >= 2050 & sic <= 2063) |
            (sic >= 2070 & sic <= 2079) | (sic >= 2090 & sic <= 2092) |
            sic == 2095 | (sic >= 2098 & sic <= 2099) ~ 2L,
        # 3: Soda
        (sic >= 2064 & sic <= 2068) | (sic >= 2086 & sic <= 2087) |
            (sic >= 2096 & sic <= 2097) ~ 3L,
        # 4: Beer
        sic == 2080 | (sic >= 2082 & sic <= 2085) ~ 4L,
        # 5: Smoke
        (sic >= 2100 & sic <= 2199) ~ 5L,
        # 6: Toys
        (sic >= 920 & sic <= 999) | (sic >= 3650 & sic <= 3652) | sic == 3732 |
            (sic >= 3930 & sic <= 3931) | (sic >= 3940 & sic <= 3949) ~ 6L,
        # 7: Fun
        (sic >= 7800 & sic <= 7833) | (sic >= 7840 & sic <= 7841) | sic == 7900 |
            (sic >= 7910 & sic <= 7911) | (sic >= 7920 & sic <= 7933) |
            (sic >= 7940 & sic <= 7949) | sic == 7980 | (sic >= 7990 & sic <= 7999) ~ 7L,
        # 8: Books
        (sic >= 2700 & sic <= 2749) | (sic >= 2770 & sic <= 2771) |
            (sic >= 2780 & sic <= 2799) ~ 8L,
        # 9: Hshld
        sic == 2047 | (sic >= 2391 & sic <= 2392) | (sic >= 2510 & sic <= 2519) |
            (sic >= 2590 & sic <= 2599) | (sic >= 2840 & sic <= 2844) |
            (sic >= 3160 & sic <= 3161) | (sic >= 3170 & sic <= 3172) |
            (sic >= 3190 & sic <= 3199) | sic == 3229 | sic == 3260 |
            (sic >= 3262 & sic <= 3263) | sic == 3269 | (sic >= 3230 & sic <= 3231) |
            (sic >= 3630 & sic <= 3639) | (sic >= 3750 & sic <= 3751) | sic == 3800 |
            (sic >= 3860 & sic <= 3861) | (sic >= 3870 & sic <= 3873) |
            (sic >= 3910 & sic <= 3911) | (sic >= 3914 & sic <= 3915) |
            (sic >= 3960 & sic <= 3962) | sic == 3991 | sic == 3995 ~ 9L,
        # 10: Clths
        (sic >= 2300 & sic <= 2390) | (sic >= 3020 & sic <= 3021) |
            (sic >= 3100 & sic <= 3111) | (sic >= 3130 & sic <= 3131) |
            (sic >= 3140 & sic <= 3151) | (sic >= 3963 & sic <= 3965) ~ 10L,
        # 11: Hlth
        (sic >= 8000 & sic <= 8099) ~ 11L,
        # 12: MedEq
        sic == 3693 | (sic >= 3840 & sic <= 3851) ~ 12L,
        # 13: Drugs
        (sic >= 2830 & sic <= 2831) | (sic >= 2833 & sic <= 2836) ~ 13L,
        # 14: Chems
        (sic >= 2800 & sic <= 2829) | (sic >= 2850 & sic <= 2879) |
            (sic >= 2890 & sic <= 2899) ~ 14L,
        # 15: Rubbr
        sic == 3031 | sic == 3041 | (sic >= 3050 & sic <= 3053) |
            (sic >= 3060 & sic <= 3069) | (sic >= 3070 & sic <= 3099) ~ 15L,
        # 16: Txtls
        (sic >= 2200 & sic <= 2284) | (sic >= 2290 & sic <= 2295) |
            (sic >= 2297 & sic <= 2299) | (sic >= 2393 & sic <= 2395) |
            (sic >= 2397 & sic <= 2399) ~ 16L,
        # 17: BldMt
        (sic >= 800 & sic <= 899) | (sic >= 2400 & sic <= 2439) |
            (sic >= 2450 & sic <= 2459) | (sic >= 2490 & sic <= 2499) |
            (sic >= 2660 & sic <= 2661) | (sic >= 2950 & sic <= 2952) |
            sic == 3200 | (sic >= 3210 & sic <= 3211) | (sic >= 3240 & sic <= 3241) |
            (sic >= 3250 & sic <= 3259) | sic == 3261 | sic == 3264 |
            (sic >= 3270 & sic <= 3275) | (sic >= 3280 & sic <= 3281) |
            (sic >= 3290 & sic <= 3293) | (sic >= 3295 & sic <= 3299) |
            (sic >= 3420 & sic <= 3433) | (sic >= 3440 & sic <= 3442) |
            sic == 3446 | (sic >= 3448 & sic <= 3452) |
            (sic >= 3490 & sic <= 3499) | sic == 3996 ~ 17L,
        # 18: Cnstr
        (sic >= 1500 & sic <= 1511) | (sic >= 1520 & sic <= 1549) |
            (sic >= 1600 & sic <= 1799) ~ 18L,
        # 19: Steel
        sic == 3300 | (sic >= 3310 & sic <= 3317) | (sic >= 3320 & sic <= 3325) |
            (sic >= 3330 & sic <= 3341) | (sic >= 3350 & sic <= 3357) |
            (sic >= 3360 & sic <= 3379) | (sic >= 3390 & sic <= 3399) ~ 19L,
        # 20: FabPr
        sic == 3400 | (sic >= 3443 & sic <= 3444) | (sic >= 3460 & sic <= 3479) ~ 20L,
        # 21: Mach
        (sic >= 3510 & sic <= 3536) | sic == 3538 | (sic >= 3540 & sic <= 3569) |
            (sic >= 3580 & sic <= 3582) | (sic >= 3585 & sic <= 3586) |
            (sic >= 3589 & sic <= 3599) ~ 21L,
        # 22: ElcEq
        sic == 3600 | (sic >= 3610 & sic <= 3613) | (sic >= 3620 & sic <= 3621) |
            (sic >= 3623 & sic <= 3629) | (sic >= 3640 & sic <= 3646) |
            (sic >= 3648 & sic <= 3649) | sic == 3660 | (sic >= 3690 & sic <= 3692) |
            sic == 3699 ~ 22L,
        # 23: Autos
        sic == 2296 | sic == 2396 | (sic >= 3010 & sic <= 3011) | sic == 3537 |
            sic == 3647 | sic == 3694 | sic == 3700 | (sic >= 3710 & sic <= 3711) |
            (sic >= 3713 & sic <= 3716) | (sic >= 3790 & sic <= 3792) |
            sic == 3799 ~ 23L,
        # 24: Aero
        (sic >= 3720 & sic <= 3721) | (sic >= 3723 & sic <= 3725) |
            (sic >= 3728 & sic <= 3729) ~ 24L,
        # 25: Ships
        (sic >= 3730 & sic <= 3731) | (sic >= 3740 & sic <= 3743) ~ 25L,
        # 26: Guns
        (sic >= 3760 & sic <= 3769) | sic == 3795 | (sic >= 3480 & sic <= 3489) ~ 26L,
        # 27: Gold
        (sic >= 1040 & sic <= 1049) ~ 27L,
        # 28: Mines
        (sic >= 1000 & sic <= 1039) | (sic >= 1050 & sic <= 1119) |
            (sic >= 1400 & sic <= 1499) ~ 28L,
        # 29: Coal
        (sic >= 1200 & sic <= 1299) ~ 29L,
        # 30: Oil
        sic == 1300 | (sic >= 1310 & sic <= 1339) | (sic >= 1370 & sic <= 1382) |
            sic == 1389 | (sic >= 2900 & sic <= 2912) | (sic >= 2990 & sic <= 2999) ~ 30L,
        # 31: Util
        sic == 4900 | (sic >= 4910 & sic <= 4911) | (sic >= 4920 & sic <= 4925) |
            (sic >= 4930 & sic <= 4932) | (sic >= 4939 & sic <= 4942) ~ 31L,
        # 32: Telcm
        sic == 4800 | (sic >= 4810 & sic <= 4813) | (sic >= 4820 & sic <= 4822) |
            (sic >= 4830 & sic <= 4841) | (sic >= 4880 & sic <= 4892) |
            sic == 4899 ~ 32L,
        # 33: PerSv
        (sic >= 7020 & sic <= 7021) | (sic >= 7030 & sic <= 7033) | sic == 7200 |
            (sic >= 7210 & sic <= 7212) | (sic >= 7214 & sic <= 7217) |
            (sic >= 7219 & sic <= 7221) | (sic >= 7230 & sic <= 7231) |
            (sic >= 7240 & sic <= 7241) | (sic >= 7250 & sic <= 7251) |
            (sic >= 7260 & sic <= 7299) | sic == 7395 | sic == 7500 |
            (sic >= 7510 & sic <= 7515) | (sic >= 7520 & sic <= 7549) |
            sic == 7600 | sic == 7620 | (sic >= 7622 & sic <= 7623) |
            (sic >= 7629 & sic <= 7631) | (sic >= 7640 & sic <= 7641) |
            (sic >= 7690 & sic <= 7699) | (sic >= 8100 & sic <= 8499) |
            (sic >= 8600 & sic <= 8699) | (sic >= 8800 & sic <= 8899) ~ 33L,
        # 34: BusSv
        (sic >= 2750 & sic <= 2759) | sic == 3993 | sic == 7218 | sic == 7300 |
            (sic >= 7310 & sic <= 7342) | (sic >= 7349 & sic <= 7353) |
            (sic >= 7359 & sic <= 7372) | (sic >= 7374 & sic <= 7385) |
            (sic >= 7389 & sic <= 7394) | (sic >= 7396 & sic <= 7397) |
            sic == 7399 | sic == 7519 | sic == 8700 | (sic >= 8710 & sic <= 8713) |
            (sic >= 8720 & sic <= 8721) | (sic >= 8730 & sic <= 8734) |
            (sic >= 8740 & sic <= 8748) | (sic >= 8900 & sic <= 8911) |
            (sic >= 8920 & sic <= 8999) | (sic >= 4220 & sic <= 4229) ~ 34L,
        # 35: Comps
        (sic >= 3570 & sic <= 3579) | (sic >= 3680 & sic <= 3689) | sic == 3695 |
            sic == 7373 ~ 35L,
        # 36: Chips
        sic == 3622 | (sic >= 3661 & sic <= 3666) | (sic >= 3669 & sic <= 3679) |
            sic == 3810 | sic == 3812 ~ 36L,
        # 37: LabEq
        sic == 3811 | (sic >= 3820 & sic <= 3827) | (sic >= 3829 & sic <= 3839) ~ 37L,
        # 38: Paper
        (sic >= 2520 & sic <= 2549) | (sic >= 2600 & sic <= 2639) |
            (sic >= 2670 & sic <= 2699) | (sic >= 2760 & sic <= 2761) |
            (sic >= 3950 & sic <= 3955) ~ 38L,
        # 39: Boxes
        (sic >= 2440 & sic <= 2449) | (sic >= 2640 & sic <= 2659) |
            (sic >= 3220 & sic <= 3221) | (sic >= 3410 & sic <= 3412) ~ 39L,
        # 40: Trans
        (sic >= 4000 & sic <= 4013) | (sic >= 4040 & sic <= 4049) | sic == 4100 |
            (sic >= 4110 & sic <= 4121) | (sic >= 4130 & sic <= 4131) |
            (sic >= 4140 & sic <= 4142) | (sic >= 4150 & sic <= 4151) |
            (sic >= 4170 & sic <= 4173) | (sic >= 4190 & sic <= 4200) |
            (sic >= 4210 & sic <= 4219) | (sic >= 4230 & sic <= 4231) |
            (sic >= 4240 & sic <= 4249) | (sic >= 4400 & sic <= 4700) |
            (sic >= 4710 & sic <= 4712) | (sic >= 4720 & sic <= 4749) |
            sic == 4780 | (sic >= 4782 & sic <= 4785) | sic == 4789 ~ 40L,
        # 41: Whlsl
        sic == 5000 | (sic >= 5010 & sic <= 5015) | (sic >= 5020 & sic <= 5023) |
            (sic >= 5030 & sic <= 5060) | (sic >= 5063 & sic <= 5065) |
            (sic >= 5070 & sic <= 5078) | (sic >= 5080 & sic <= 5088) |
            (sic >= 5090 & sic <= 5094) | (sic >= 5099 & sic <= 5100) |
            (sic >= 5110 & sic <= 5113) | (sic >= 5120 & sic <= 5122) |
            (sic >= 5130 & sic <= 5172) | (sic >= 5180 & sic <= 5182) |
            (sic >= 5190 & sic <= 5199) ~ 41L,
        # 42: Rtail
        sic == 5200 | (sic >= 5210 & sic <= 5231) | (sic >= 5250 & sic <= 5251) |
            (sic >= 5260 & sic <= 5261) | (sic >= 5270 & sic <= 5271) |
            sic == 5300 | (sic >= 5310 & sic <= 5311) | sic == 5320 |
            (sic >= 5330 & sic <= 5331) | sic == 5334 | (sic >= 5340 & sic <= 5349) |
            (sic >= 5390 & sic <= 5400) | (sic >= 5410 & sic <= 5412) |
            (sic >= 5420 & sic <= 5469) | (sic >= 5490 & sic <= 5500) |
            (sic >= 5510 & sic <= 5579) | (sic >= 5590 & sic <= 5700) |
            (sic >= 5710 & sic <= 5722) | (sic >= 5730 & sic <= 5736) |
            (sic >= 5750 & sic <= 5799) | sic == 5900 | (sic >= 5910 & sic <= 5912) |
            (sic >= 5920 & sic <= 5932) | (sic >= 5940 & sic <= 5990) |
            (sic >= 5992 & sic <= 5995) | sic == 5999 ~ 42L,
        # 43: Meals
        (sic >= 5800 & sic <= 5829) | (sic >= 5890 & sic <= 5899) | sic == 7000 |
            (sic >= 7010 & sic <= 7019) | (sic >= 7040 & sic <= 7049) |
            sic == 7213 ~ 43L,
        # 44: Banks
        sic == 6000 | (sic >= 6010 & sic <= 6036) | (sic >= 6040 & sic <= 6062) |
            (sic >= 6080 & sic <= 6082) | (sic >= 6090 & sic <= 6100) |
            (sic >= 6110 & sic <= 6113) | (sic >= 6120 & sic <= 6179) |
            (sic >= 6190 & sic <= 6199) ~ 44L,
        # 45: Insur
        sic == 6300 | (sic >= 6310 & sic <= 6331) | (sic >= 6350 & sic <= 6351) |
            (sic >= 6360 & sic <= 6361) | (sic >= 6370 & sic <= 6379) |
            (sic >= 6390 & sic <= 6411) ~ 45L,
        # 46: RlEst
        sic == 6500 | sic == 6510 | (sic >= 6512 & sic <= 6515) |
            (sic >= 6517 & sic <= 6532) | (sic >= 6540 & sic <= 6541) |
            (sic >= 6550 & sic <= 6553) | (sic >= 6590 & sic <= 6599) |
            (sic >= 6610 & sic <= 6611) ~ 46L,
        # 47: Fin
        (sic >= 6200 & sic <= 6299) | sic == 6700 | (sic >= 6710 & sic <= 6726) |
            (sic >= 6730 & sic <= 6733) | (sic >= 6740 & sic <= 6779) |
            (sic >= 6790 & sic <= 6795) | (sic >= 6798 & sic <= 6799) ~ 47L,
        # 48: Other
        (sic >= 4950 & sic <= 4961) | (sic >= 4970 & sic <= 4971) |
            (sic >= 4990 & sic <= 4991) | sic == 9999 ~ 48L,
        TRUE ~ NA_integer_
    )

}
