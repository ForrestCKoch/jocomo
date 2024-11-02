# *Disclaimer*
*This package is still in alpha. A few of the planned interfaces are still a work in progress, and further tests will be written to ensure code robustness.*

# Overview


## Background Theory: McNemar's Test (McNemar 1947)

|              | **Test 1 +** | **Test 1 -** |
|--------------|--------------|--------------|
| **Test 2 +** | $A$          | $B$          |
| **Test 2 -** | $C$          | $D$          |

- A paired sample test of proportions (e.g accuracy)
- $H_0:\pi_B=\pi_C$, i.e no difference in the proportion of discordant pairs.
- $X=\frac{(B-C)^2}{B+C}$
- $X\overset{a}\sim \chi^2_1$

## Background Theory: Lachenbruch's Extension (Lachenbruch & Lynch 1998)
|   *Gold +*   | **Test 1 +** | **Test 1 -** |   *Gold -*   | **Test 1 +** | **Test 1 -** |
|--------------|--------------|--------------|--------------|--------------|--------------|
| **Test 2 +** | $A^{\small(+)}$    | $B^{\small(+)}$    |              | $A^{\small(-)}$    | $B^{\small(-)}$    |
| **Test 2 -** | $C^{\small(+)}$    | $D^{\small(+)}$    |              | $C^{\small(-)}$    | $D^{\small(-)}$    |

- Proposed a joint test of sensitivity (Sn) and specificity (Sp) 
- $H_0: [\text{Sn}_1=\text{Sn}_2]\cap[\text{Sp}_1=\text{Sp}_2]$
- $X=\frac{\small(B^{\small(+)}-C^{\small(+)})^2}{B^{\small(+)}+C^{\small(+)}}+\frac{\small(B^{\small(-)}-C^{\small(-)})^2}{B^{\small(-)}+C^{\small(-)}}$
- $X\overset{a}\sim \chi^2_2$

## Background Theory: Wu's Extension (Wu 2023)

- Allows for more than 2 tests/models to be compared
- $H_0: [\text{Sn}_1=...=\text{Sn}_J]\cap[\text{Sp}_1=...=\text{Sp}_J]$
- A test statistic based on:
  - Type I $(T_1=1, T_j=0, T_{j'}=0)$ and
  - Type II $(T_1=0, T_j=1, T_{j'}=1)$ discordant pairs
- $X^2_{EM} = a^TA^{-1}a + b^TB^{-1}b$
- where $a^TA^{-1}a$ is a weighted sum of squared deviation from the mean under $H_0$
- $X^2_{EM}\overset{a}\sim \chi^2_{2\times(J-1)}$

## Wu's Extension: Further Information

$a = (n^{(22)}_{10}-n^{(22)}_{01}, ..., n^{(22)}_{10}-n^{(22)}_{01})$ 

$$A = \begin{pmatrix} n^{(22)}_{10}+n^{(22)}_{01} & n^{(23)}_{10}+n^{(23)}_{01} & ... & n^{(2J)}_{10}+n^{(2J)}_{01} \\
& \vdots & & \\
n^{(J2)}_{10}+n^{(J2)}_{01} & n^{(23)}_{10}+n^{(23)}_{01} & ... & n^{(JJ)}_{10}+n^{(JJ)}_{01} \\
\end{pmatrix}$$ 

- $n^{(jj')}_{10}$ is the number of Type I discordant pairs, and
- $n^{(jj')}_{01}$ is the number of Type II discordant pairs for positive cases.
- $b$, $B$, and $m$ are constructed similarly for negative cases.

## Proposed Extension: Allowing for >2 Classes

- Sensitivity and specificity are Positive and Negative class accuracy respectively.
- If we can assume that the class-wise statistics are independent.
- $H_0: \bigcap_{c\in C} \big[\text{Acc}(T_1,c)=...=\text{Acc}(T_J,c)\big]$
- $X^2_{EMMC} = \sum_{c\in C}a_c^TA_c^{-1}a_c$
- $X^2_{EMMC}\overset{a}\sim\chi^2_{C\times(J-1)}$

## Proposed Extension: Allowing for mixed effects
### (e.g dataset, collection center, patient, ...)
- $H_0: \bigcap_{d\in D} [\text{Sn}(d)_1=...=\text{Sn}(d)_J]\cap[\text{Sp}(d)_1=...=\text{Sp}(d)_J]$
- $X^2_{EMMD} = \sum_{d\in D}\big(\sideset{_{(d)}}{^T}{a}\sideset{_{(d)}}{^{-1}}{A}\sideset{_{(d)}}{}{a} + \sideset{_{(d)}}{^T}{b}\sideset{_{(d)}}{^{-1}}{B}\sideset{_{(d)}}{}{b}\big)$
- $X^2_{EMMD}\overset{a}\sim\chi^2_{D\times2\times(J-1)}$

## Proposed Extension: Combining the two 
- $H_0: \bigcap_{d\in D}\Big(\bigcap_{c\in C} \big[\text{Acc}(T_1,c,d)=...=\text{Acc}(T_J,c,d)\big]\Big)$
- $X^2_{JCM} = \sum_{d\in D,c\in C}\big(\sideset{_{(d)}}{^T_c}{a}\sideset{_{(d)}}{^{-1}_c}{A}\sideset{_{(d)}}{_c}{a}\big)$
- $X^2_{JCM}\overset{a}\sim\chi^2_{D\times C\times(J-1)}$
