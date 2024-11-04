# *Disclaimer*
*This package is still in alpha. A few of the planned interfaces are still a work in progress, and further tests will be written to ensure code robustness.*

# Installation

JoCoMo is not yet available on CRAN. In the meantime it can be installed with

```
devtools::install_github("https://github.com/ForrestCKoch/jocomo.git")
```

# Current Features
- An implementation of Wu's test is provided by `jocomo::wu.test`. Data may be provided in:
  - wide format: `jocomo::wu.test(x, y, models, samples)` where `x` is a matrix
  - long format: `jocomo::wu.test(x, y, models, samples)` where `x` is a vector
  - frequency table format: `jocomo::wu.test(xt)` where `xt` is an `xtabs` object
  - wide formula: `jocomo::wu.test(outcome~m1 + m2 + m3 + ., data=DF)` where the `outcome` is the true class, the RHS contains the model predictions, and `DF` contains one row per sample.
  - long formula: `jocomo::wu.test(outcome~pred:model|sample, data=DF)` where `outcome` is the true class, `pred` contains the model predictions, `model` is a factor for the models, `sample` is a factor of samples, and `DF` contains one row for each prediction.
  - frequency table formula: `jocomo::wu.test(Freq~m1 + m2 + . || outcome, data=DF)` where `Freq` is the frequency count, `outcome` is the true class, and the remaining terms are the model predictions. `DF` should have 1 row for each possible combination of `outcome`, `m1`, `m2`, ... The output of converting `xtabs` to a `data.frame` should suffice.
  
- An implementation of Wu's test which allows for two or more classes in provided by `jocomo::multiclass.wu.test`.  This interface is complete and follows the same convention as `jocomo::wu.test`.

- The implementation of Wu's test allowing for two or more classes and blocking factors is a work in progress, and is provided by `jocomo::jocomo.chisq.test`. Currently only wide and long format interfaces are available with formula and frequency table interfaces to come soon!


# Background Theory

## Background Theory: McNemar's Test (McNemar 1947)

|              | **Test 1 +** | **Test 1 -** |
|--------------|--------------|--------------|
| **Test 2 +** | $A$          | $B$          |
| **Test 2 -** | $C$          | $D$          |

- A paired sample test of proportions (e.g accuracy)
- $H\_0:\pi\_B=\pi\_C$, i.e no difference in the proportion of discordant pairs.
- $X=\frac{(B-C)^2}{B+C}$
- $X\overset{a}\sim \chi^2\_1$

## Background Theory: Lachenbruch's Extension (Lachenbruch & Lynch 1998)
|   *Gold +*   | **Test 1 +** | **Test 1 -** |   *Gold -*   | **Test 1 +** | **Test 1 -** |
|--------------|--------------|--------------|--------------|--------------|--------------|
| **Test 2 +** | $A^{\small(+)}$    | $B^{\small(+)}$    |              | $A^{\small(-)}$    | $B^{\small(-)}$    |
| **Test 2 -** | $C^{\small(+)}$    | $D^{\small(+)}$    |              | $C^{\small(-)}$    | $D^{\small(-)}$    |

- Proposed a joint test of sensitivity (Sn) and specificity (Sp) 
- $H\_0: [\text{Sn}\_1=\text{Sn}\_2]\cap[\text{Sp}\_1=\text{Sp}\_2]$
- $X=\frac{\small(B^{\small(+)}-C^{\small(+)})^2}{B^{\small(+)}+C^{\small(+)}}+\frac{\small(B^{\small(-)}-C^{\small(-)})^2}{B^{\small(-)}+C^{\small(-)}}$
- $X\overset{a}\sim \chi^2\_2$

## Background Theory: Wu's Extension (Wu 2023)

- Allows for more than 2 tests/models to be compared
- $H\_0: [\text{Sn}\_1=...=\text{Sn}\_J]\cap[\text{Sp}\_1=...=\text{Sp}\_J]$
- A test statistic based on:
  - Type I $(T\_1=1, T\_j=0, T\_{j'}=0)$ and
  - Type II $(T\_1=0, T\_j=1, T\_{j'}=1)$ discordant pairs
- $X^2\_{EM} = a^TA^{-1}a + b^TB^{-1}b$
- where $a^TA^{-1}a$ is a weighted sum of squared deviation from the mean under $H\_0$
- $X^2\_{EM}\overset{a}\sim \chi^2\_{2\times(J-1)}$

## Wu's Extension: Further Information

$a = (n^{(22)}\_{10}-n^{(22)}\_{01}, ..., n^{(22)}\_{10}-n^{(22)}\_{01})$ 

$$A = \begin{pmatrix} n^{(22)}\_{10}+n^{(22)}\_{01} & n^{(23)}\_{10}+n^{(23)}\_{01} & ... & n^{(2J)}\_{10}+n^{(2J)}\_{01} \\
& \vdots & & \\
n^{(J2)}\_{10}+n^{(J2)}\_{01} & n^{(23)}\_{10}+n^{(23)}\_{01} & ... & n^{(JJ)}\_{10}+n^{(JJ)}\_{01} \\
\end{pmatrix}$$ 

- $n^{(jj')}\_{10}$ is the number of Type I discordant pairs, and
- $n^{(jj')}\_{01}$ is the number of Type II discordant pairs for positive cases.
- $b$, $B$, and $m$ are constructed similarly for negative cases.

## Proposed Extension: Allowing for >2 Classes

- Sensitivity and specificity are Positive and Negative class accuracy respectively.
- If we can assume that the class-wise statistics are independent.
- $H\_0: \bigcap\_{c\in C} \big[\text{Acc}(T\_1,c)=...=\text{Acc}(T\_J,c)\big]$
- $X^2\_{EMMC} = \sum\_{c\in C}a\_c^TA\_c^{-1}a\_c$
- $X^2\_{EMMC}\overset{a}\sim\chi^2\_{C\times(J-1)}$

## Proposed Extension: Allowing for mixed effects
### (e.g dataset, collection center, patient, ...)
- $H\_0: \bigcap\_{d\in D} [\text{Sn}(d)\_1=...=\text{Sn}(d)\_J]\cap[\text{Sp}(d)\_1=...=\text{Sp}(d)\_J]$
- $X^2\_{EMMD} = \sum\_{d\in D}\big(\sideset{\_{(d)}}{^T}{a}\sideset{\_{(d)}}{^{-1}}{A}\sideset{\_{(d)}}{}{a} + \sideset{\_{(d)}}{^T}{b}\sideset{\_{(d)}}{^{-1}}{B}\sideset{\_{(d)}}{}{b}\big)$
- $X^2\_{EMMD}\overset{a}\sim\chi^2\_{D\times2\times(J-1)}$

## Proposed Extension: Combining the two 
- $H\_0: \bigcap\_{d\in D}\Big(\bigcap\_{c\in C} \big[\text{Acc}(T\_1,c,d)=...=\text{Acc}(T\_J,c,d)\big]\Big)$
- $X^2\_{JCM} = \sum\_{d\in D,c\in C}\big(\sideset{\_{(d)}}{^T\_c}{a}\sideset{\_{(d)}}{^{-1}\_c}{A}\sideset{\_{(d)}}{\_c}{a}\big)$
- $X^2\_{JCM}\overset{a}\sim\chi^2\_{D\times C\times(J-1)}$
