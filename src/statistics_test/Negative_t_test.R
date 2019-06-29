# ネガティブ比のt検定
# staticのt検定
test.mu <- 0.5 # t検定のμの値,今回は50%

# t検定
t.test(static.PN.Ratio.list,mu = test.mu)
t.test(it160.PN.Ratio.list,mu = test.mu)
t.test(it40.PN.Ratio.list,mu = test.mu)
t.test(it10.PN.Ratio.list,mu = test.mu)
