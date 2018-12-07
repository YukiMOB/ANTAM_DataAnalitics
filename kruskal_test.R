# kruskal test for ANTAM data Analysis

# クラスカル・ウォリス検定 : 距離
kruskal.test(x=list(staticLeft.distance.list,staticRight.distance.list,it160.distance.list,it40.distance.list,it10.distance.list,NoStimulus.distance.list))
kruskal.test(x=list(staticLeft.list.velocity_average,staticRight.list.velocity_average,it160.list.velocity_average,it40.list.velocity_average,it10.list.velocity_average,NoStimulus.list.Velocity_average))
kruskal.test(x=list(staticLeft.residence_ratio.list,staticRight.residence_ratio.list,it160.residence_ratio.list,it40.residence_ratio.list,it10.residence_ratio.list,NoStimulus.residence_ratio.list))
kruskal.test(x=list(subset(it30min_L.ratio.PN$value_Left,it30min_L.ratio.PN$PorN_Left == "Negative")
                    ,subset(it160.ratio.PN$value_Left,it160.ratio.PN$PorN_Left == "Negative")
                    ,subset(it40.ratio.PN$value_Left,it40.ratio.PN$PorN_Left == "Negative")
                    ,subset(it10.ratio.PN$value_Left,it10.ratio.PN$PorN_Left == "Negative")))

kruskal.test(x=list(subset(it30min_R.ratio.PN$value_Right,it30min_R.ratio.PN$PorN_Right == "Negative")
                    ,subset(it160.ratio.PN$value_Right,it160.ratio.PN$PorN_Right == "Negative")
                    ,subset(it40.ratio.PN$value_Right,it40.ratio.PN$PorN_Right == "Negative")
                    ,subset(it10.ratio.PN$value_Right,it10.ratio.PN$PorN_Right == "Negative")))