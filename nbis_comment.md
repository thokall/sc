<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#org66d7307">1. Batch isssues with rna-seq data</a>
<ul>
<li><a href="#orge11e6e4">1.1. Blocking and normal voom</a></li>
<li><a href="#org047b797">1.2. Blocking and voom with weights</a></li>
<li><a href="#orgbfcce06">1.3. Omitting the "odd" samples</a></li>
</ul>
</li>
</ul>
</div>
</div>


<a id="org66d7307"></a>

# Batch isssues with rna-seq data

The lack of clustering of the problematic samples is not trivial to
resolve. It might be that including batch effect would reduce the
issue, but it is certainly not certain that you will resolve the issue
due to this as therse data points is not forming any kind of coherent
group, but is found at two different places far from the "good"
samples. I would hence based on this pattern and the fact that it
corresponds to slightly lower RIN values consider omitting this
samples all together. Optimally I would however like to have other
data to back that exclusion.

So based on your analysis I decided to try to use a, for me not very
commonly applied, function from the limma package. The
voomWithQualityWeigths function is doing the same thing as limmas voom
function, but instead of only adding weights to genes it try to
estimates samples weights in an attempt to put less emphasis in
samples that globally have a different pattern. Since this is outside
my comfort zone I encourage you to read up on this in the limma manual
as well as the 2006 paper describing how to jointly weight both genes
and arrays in a linera model approach.


<a id="orge11e6e4"></a>

## Blocking and normal voom

This is basically the same as your DEseq2 analysis and blocks the
family effect. As with your analysis this analysis does not find any
significant genes between sat and lat after multiple correction.

    library(limma)
    library(edgeR)

Reads in your data and reformat data into a DGElist that are suitable
for de analysis. I also estimate normfactor to take library sizes
differences into account.

    bc <- read.table("brain-counts.txt", header = TRUE)
    meta <- read.table("brain-meta.txt", header = TRUE)

    meta$condition <- relevel(meta$condition, ref = "sat")
    meta$family <- as.factor(meta$family)
    mod <- model.matrix(~meta$family+meta$condition )

    y <- DGEList(counts = bc, genes = row.names(bc), group = meta$condition)
    y <- calcNormFactors(y)

Using voom to convert counts to log2 scale and also estimate a weight
for each gene that is later used in the lmFit function.

The summary shows that 0 genes are significant for the condition and
there seem to be large differences between between family 19 compared
to the rest which is due to the fact that this family have many
samples with low at the rinbatch factor, so it is likely to mostly
reflect this.

    # limma with blocking for family
    # Gives 0 significant genes for lat/sat
    y.voom <- voom(y, des = mod)
    fit.y <- lmFit(y.voom, des = mod)
    fit.y <- eBayes(fit.y)
    summary(dt.y <- decideTests(fit.y))
    topTable(fit.y, coef = 5)

       (Intercept) meta$family16 meta$family19 meta$family20 meta$conditionlat
    -1         135             0          2394             1                 0
    0         2703         20860         17075         20852             20861
    1        18023             1          1392             8                 0
                                    genes      logFC  AveExpr         t
    ENSDARG00000038213 ENSDARG00000038213 -0.2571926 4.477578 -3.874298
    ENSDARG00000024877 ENSDARG00000024877 -0.8697534 4.511426 -3.743882
    ENSDARG00000045367 ENSDARG00000045367  1.0291212 4.302541  3.644256
    ENSDARG00000102403 ENSDARG00000102403  0.3114920 5.825162  3.296172
    ENSDARG00000096904 ENSDARG00000096904 -0.5924871 3.541640 -3.561318
    ENSDARG00000069589 ENSDARG00000069589 -0.2506152 3.680434 -3.462826
    ENSDARG00000000796 ENSDARG00000000796 -0.5645205 3.964481 -3.378700
    ENSDARG00000101726 ENSDARG00000101726  0.6844830 3.870745  3.349761
    ENSDARG00000069498 ENSDARG00000069498  0.3885613 2.367423  3.957819
    ENSDARG00000077180 ENSDARG00000077180  0.2960716 3.999188  3.199637
                            P.Value adj.P.Val         B
    ENSDARG00000038213 0.0004176489 0.9999821 -2.585068
    ENSDARG00000024877 0.0006099773 0.9999821 -2.723873
    ENSDARG00000045367 0.0008121738 0.9999821 -2.906091
    ENSDARG00000102403 0.0021570005 0.9999821 -3.016529
    ENSDARG00000096904 0.0010285303 0.9999821 -3.075896
    ENSDARG00000069589 0.0013578367 0.9999821 -3.108228
    ENSDARG00000000796 0.0017171893 0.9999821 -3.137574
    ENSDARG00000101726 0.0018606281 0.9999821 -3.183842
    ENSDARG00000069498 0.0003269599 0.9999821 -3.242570
    ENSDARG00000077180 0.0028078243 0.9999821 -3.254578


<a id="org047b797"></a>

## Blocking and voom with weights

In this section we have the same results, but with the alternative
voomWithWeights function. It alters things quite drastically, which
oftem makes me a bit worried as it moves us far from the raw
counts. Looking at the estimated weights it is basically downweighting
the samples that is far from the main cluster in the MDS plot. So the
samples from fam. 13 that in your MDS plot is forming a group clearly
sep. from the main cluster along axis 3 have estimated weights between
0.32, 0.16 and 0.18. The samples that are clearly sep. along the first
dimension have weights lower than 0.05 and there signal will hence
contribute much less to the inferred expression pattern.

Due to my limited experience with this, I would in your place still
focus my analysis on the samples that behaves as expected and omit the
rest. It is however somewhat re-assuring that among the 48 significant
genes with this approach, we find all genes that are significant in
the analysis where these samples are excluded, so the approach is not
interrupting this signal.

    y.voomW <- voomWithQualityWeights(y, des = mod)
    fit.yw <- lmFit(y.voomW, des = mod)
    fit.yw <- eBayes(fit.yw)
    summary(dt.yw <- decideTests(fit.yw))
    topTable(fit.yw, coef = 5)

       (Intercept) meta$family16 meta$family19 meta$family20 meta$conditionlat
    -1         448           970           188           331                23
    0         1613         19218         20518         20177             20813
    1        18800           673           155           353                25
                                    genes      logFC   AveExpr         t
    ENSDARG00000101726 ENSDARG00000101726  0.8742591 3.8707450  6.147013
    ENSDARG00000012499 ENSDARG00000012499  0.3504051 7.0847155  6.174131
    ENSDARG00000102403 ENSDARG00000102403  0.3327276 5.8251616  6.030203
    ENSDARG00000104818 ENSDARG00000104818  0.8157812 3.4364518  5.966072
    ENSDARG00000000212 ENSDARG00000000212 -1.5210869 0.9591816 -6.096417
    ENSDARG00000045367 ENSDARG00000045367  1.3598822 4.3025407  5.837156
    ENSDARG00000024877 ENSDARG00000024877 -1.0314416 4.5114261 -5.824597
    ENSDARG00000000796 ENSDARG00000000796 -0.7051283 3.9644808 -5.700994
    ENSDARG00000069262 ENSDARG00000069262  0.4843307 3.8443761  5.634835
    ENSDARG00000040536 ENSDARG00000040536 -0.7757342 2.2704160 -5.520455
                            P.Value   adj.P.Val        B
    ENSDARG00000101726 2.744277e-07 0.001676650 6.776055
    ENSDARG00000012499 2.511736e-07 0.001676650 6.759785
    ENSDARG00000102403 4.018624e-07 0.001676650 6.316085
    ENSDARG00000104818 4.954655e-07 0.001722651 6.212450
    ENSDARG00000000212 3.237266e-07 0.001676650 5.899153
    ENSDARG00000045367 7.546467e-07 0.002050142 5.796014
    ENSDARG00000024877 7.862106e-07 0.002050142 5.749124
    ENSDARG00000000796 1.176445e-06 0.002726868 5.380422
    ENSDARG00000069262 1.459386e-06 0.003044425 5.189759
    ENSDARG00000040536 2.117372e-06 0.004015500 4.768774


<a id="orgbfcce06"></a>

## Omitting the "odd" samples

Below I have done the same analysis as above, but I have omitted the
samples that are clustering outside the main cluster in the MDS. Doing
this reveals a small number of genes that are different between Sat
and Lat. For the sake of making my tests complete I also include
weights here and that increases the number of significant genes. The
later analysis reveals 2 genes that are not found among the
significant from weighted analysis of the complete data above, but
overall the patterns are largely the same.

    meta2 <- meta[meta$rinbatch !="low",]
    bc2 <- bc[,colnames(bc)%in%row.names(meta2)]
    meta2$condition <- relevel(meta2$condition, ref = "sat")
    meta2$family <- as.factor(meta2$family)
    mod2 <- model.matrix(~meta2$family+meta2$condition )

    y2 <- DGEList(counts = bc2, genes = row.names(bc2), group = meta2$condition)
    y2 <- calcNormFactors(y2)

    y.voom2 <- voom(y2, des = mod2)
    fit.y2 <- lmFit(y.voom2, des = mod2)
    fit.y2 <- eBayes(fit.y2)
    summary(dt.y2 <- decideTests(fit.y2))
    topTable(fit.y2, coef = 5)

       (Intercept) meta2$family16 meta2$family19 meta2$family20 meta2$conditionlat
    -1         398            750             40             63                  3
    0         1804          19736          20802          20733              20851
    1        18659            375             19             65                  7
                                    genes      logFC   AveExpr         t
    ENSDARG00000069262 ENSDARG00000069262  0.4581458 3.4395424  6.597473
    ENSDARG00000045768 ENSDARG00000045768  0.2958556 6.7019539  6.091893
    ENSDARG00000102403 ENSDARG00000102403  0.3503684 5.8042317  6.024707
    ENSDARG00000012499 ENSDARG00000012499  0.3615351 7.2026446  5.714886
    ENSDARG00000024877 ENSDARG00000024877 -1.1013067 4.2632573 -5.520183
    ENSDARG00000086933 ENSDARG00000086933  0.7217278 2.1742931  5.333333
    ENSDARG00000000212 ENSDARG00000000212 -1.5575497 0.6753062 -5.590954
    ENSDARG00000078567 ENSDARG00000078567  0.3229310 5.7934707  5.163730
    ENSDARG00000101726 ENSDARG00000101726  0.8265865 3.5768913  4.856424
    ENSDARG00000098899 ENSDARG00000098899 -0.6156610 4.6572151 -4.815047
                            P.Value   adj.P.Val        B
    ENSDARG00000069262 1.997158e-07 0.004166272 7.022350
    ENSDARG00000045768 8.497327e-07 0.007171621 5.752628
    ENSDARG00000102403 1.031344e-06 0.007171621 5.580309
    ENSDARG00000012499 2.526621e-06 0.011994120 4.692468
    ENSDARG00000024877 4.444638e-06 0.013245655 4.212869
    ENSDARG00000086933 7.647223e-06 0.019941090 3.464674
    ENSDARG00000000212 3.619347e-06 0.012583868 3.394976
    ENSDARG00000078567 1.251509e-05 0.029008596 3.183816
    ENSDARG00000101726 3.050809e-05 0.053035780 2.418071
    ENSDARG00000098899 3.438739e-05 0.055181184 2.275077

    y.voom22 <- voomWithQualityWeights(y2, des = mod2)
    fit.y22 <- lmFit(y.voom22, des = mod2)
    fit.y22 <- eBayes(fit.y22)
    summary(dt.y22 <- decideTests(fit.y22))
    topTable(fit.y22, coef = 5)

       (Intercept) meta2$family16 meta2$family19 meta2$family20 meta2$conditionlat
    -1         441           2118            163            508                 11
    0         1732          17395          20590          19954              20834
    1        18688           1348            108            399                 16
                                    genes      logFC   AveExpr         t
    ENSDARG00000069262 ENSDARG00000069262  0.4914066 3.4395424  7.275510
    ENSDARG00000012499 ENSDARG00000012499  0.3530555 7.2026446  6.372873
    ENSDARG00000102403 ENSDARG00000102403  0.3293710 5.8042317  5.865138
    ENSDARG00000086933 ENSDARG00000086933  0.7728629 2.1742931  5.896854
    ENSDARG00000070971 ENSDARG00000070971  0.4855436 6.0660989  5.823174
    ENSDARG00000045768 ENSDARG00000045768  0.2643559 6.7019539  5.689056
    ENSDARG00000101726 ENSDARG00000101726  0.8699100 3.5768913  5.537323
    ENSDARG00000104818 ENSDARG00000104818  0.8168754 3.1469681  5.454815
    ENSDARG00000000212 ENSDARG00000000212 -1.5529246 0.6753062 -5.679082
    ENSDARG00000078567 ENSDARG00000078567  0.2957581 5.7934707  5.388188
                            P.Value    adj.P.Val        B
    ENSDARG00000069262 2.717201e-08 0.0005668352 8.964528
    ENSDARG00000012499 3.553188e-07 0.0037061523 6.500055
    ENSDARG00000102403 1.550444e-06 0.0073106126 5.114299
    ENSDARG00000086933 1.413576e-06 0.0073106126 5.053914
    ENSDARG00000070971 1.752220e-06 0.0073106126 4.979699
    ENSDARG00000045768 2.591667e-06 0.0079518440 4.570791
    ENSDARG00000101726 4.037805e-06 0.0096445442 4.299145
    ENSDARG00000104818 5.139578e-06 0.0107216737 4.065371
    ENSDARG00000000212 2.668276e-06 0.0079518440 3.851922
    ENSDARG00000078567 6.245376e-06 0.0118440723 3.757336

