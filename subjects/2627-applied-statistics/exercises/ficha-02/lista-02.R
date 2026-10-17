#----------------------------------------------
# lista 2 - nao parametrica
#----------------------------------------------

binom.test(8,9,0.5)


binom.test(2,13,0.5)

x = c(4.91, 4.10, 6.74, 7.27, 7.42, 7.50, 6.56, 4.64, 5.98, 3.14, 3.23, 5.80, 6.17, 5.39, 5.77)
wilcox.test(x,mu=5.05,alternative='two.sided')


xdiff = x-5.05
x1 = abs(xdiff)
x1_rank = rank(x1)
sinal = ifelse(xdiff<0,-1,1)
sort(x1)
rank = sinal*x1
sum(x1_rank[sinal==-1])
sum(x1_rank[sinal==1])

# ex1
x = c(4,5,8,8,9,6,10,7,6,6)

binom.test(sum(x==5),length(x),0.5)

# ex2

antes = c(1.69, 2.77, 1.00, 1.66, 3.00, 0.85, 1.42, 2.82, 2.58, 1.84, 1.89, 1.91, 1.75, 2.46, 2.35)
depois = c(1.69, 2.22, 3.07, 3.35, 3.00, 2.74, 3.61, 5.14, 2.44, 4.17, 2.42, 2.94, 3.04, 4.62, 4.42)
d = antes-depois



# ex 4
x = c(63, 68, 79, 65, 64, 63, 65, 64,
      76, 74, 66, 66, 67, 73, 69, 76)

wilcox.test(x,70,alternative = 'less')

xdiff = x - 70
x1 = rank(xdiff)
sinal = ifelse()

# ex5

urb = c(35, 26, 27, 21, 27, 38, 23, 25, 25, 27, 45, 46, 33, 26, 46, 41) 
rur = c(29, 50, 43, 22, 42, 47, 42, 32, 50, 37, 34, 31)

wilcox.test(urb,rur)


# ex6

exp = c(14.4, 14.2, 13.8, 16.5, 14.1, 16.6, 15.9, 15.6, 14.1, 15.3, 15.7, 16.7, 13.7, 15.3, 14.0)
nexp = c(17.4, 16.2, 17.1, 17.5, 15.0, 16.0, 16.9, 15.0, 16.3, 16.8)

wilcox.test(exp,nexp,alternative = 'less')

# ex7 

x = c(44.0, 46.6, 48.2, 51.8, 60.3, 61.7, 63.6, 72.7, 77.4, 82.4, 96.1, 105.6)
npos = sum(x>60)
wilcox.test(x,60)
binom.test(npos,length(x),0.5, alternative = 'two.sided') # p-value = 0.3877


# ex8
x = c(1.9, 2.0, 2.2, 2.8, 3.1, 3.1, 3.3, 3.4, 3.7)
wilcox.test(x,3.3,alternative = 'less')

# ex9

reg_a = c(0.237, 0.235, 0.423, 0.398, 0.241, 0.237, 0.344, 0.449, 0.741, 0.405)
reg_b = c(0.341, 0.482, 0.464, 0.256, 0.908, 0.286, 0.518, 0.326)

wilcox.test(reg_a,reg_b,alternative = 'two.sided')


# exercicios ficha 03

# 01
power.t.test(n=NULL,d=.5,power=.8,sig.level=0.05)$n

# 02
power.t.test(n=NULL,d=.3,power=.8,sig.level=0.05,alternative = 'two.sided')$n

# 03
power.t.test(n=20,d=.4,power=NULL,sig.level=0.05)$power
power.t.test(n=NULL,d=.4,power=0.8,sig.level=0.05)$n

# 04
power.t.test(n=20,d=.4,power=NULL,sig.level=0.05)$power

# 05
power.t.test(n=NULL,d=.35,power=0.8,sig.level=0.05)$n

# 06
1-power.t.test(n=20,d=.6,power=NULL,sig.level=0.05)$power
1-power.t.test(n=20,d=.6,power=NULL,sig.level=0.1)$power

# 07
cost_per_person = 200
power.t.test(n=NULL,d=.5,power=0.8,sig.level=0.05)$n # per group!
round(power.t.test(n=NULL,d=.5,power=0.8,sig.level=0.05)$n,0)*2*cost_per_person

# increase power
max_budget = 14000
n_teste = max_budget/cost_per_person
power.t.test(n=n_teste,d=.5,power=0.81,sig.level=NULL)$sig.level # per group!
power.t.test(n=n_teste,d=.7,power=0.81,sig.level=NULL)$sig.level # per group!

# 08
power.t.test(n=NULL,d=NULL,power=0.3,sig.level=0.06) # per group!

# 09
pwr.r.test(n=1000,r=.05,power=NULL,sig.level=0.05) # per group!
pwr.r.test(n=1000,r=.05,power=.35,sig.level=NULL) # per group!
pwr.r.test(n=1000,r=.0025,power=NULL,sig.level=0.05)$power # per group!

# ex 10 ?
t.test(70,mu=75,sd)
pwr.r.test(n=50,r=.0025,power=NULL,sig.level=0.05)$power # per group!

# ex 11
propo
pwr.r.test(n=NULL,r=.0025,power=0.8,sig.level=0.05)$power # per group!

install.packages('pwr')
library(pwr)

4.5837
bss/(k-1)