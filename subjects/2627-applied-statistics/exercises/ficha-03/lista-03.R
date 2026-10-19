# exercicios ficha 03

#----------------------------------------------
# exercise 1
#----------------------------------------------
power.t.test(n=NULL,d=.5,power=.8,sig.level=0.05)

#----------------------------------------------
# exercise 2
#----------------------------------------------
pwr.r.test(r=0.3,sig.level = 0.05,power=0.80,alternative = 'two.sided')

#----------------------------------------------
# exercise 3
#----------------------------------------------
# 1
pwr.t.test(d=0.4,n=20,sig.level=0.05,power=NULL)$power
# 2
pwr.t.test(d=0.4,n=NULL,sig.level=0.05,power=0.8)$n # per group

#----------------------------------------------
# exercise 4 - chisq test
#----------------------------------------------

pwr.chisq.test(w=0.2,sig.level = 0.05,power=0.8,N=NULL,df=2)

#----------------------------------------------
# exercise 5 - paired sample t test
#----------------------------------------------

pwr.t.test(d=0.35,sig.level = 0.05,power=0.8,alternative = 'two.sided',type='paired')

#----------------------------------------------
# exercise 6
#----------------------------------------------

#----------------------------------------------
# exercise 7
#----------------------------------------------


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
