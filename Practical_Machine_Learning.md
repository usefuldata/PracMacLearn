Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now
possible to collect a large amount of data about personal activity
relatively inexpensively. These type of devices are part of the
quantified self movement - a group of enthusiasts who take measurements
about themselves regularly to improve their health, to find patterns in
their behavior, or because they are tech geeks. One thing that people
regularly do is quantify how much of a particular activity they do, but
they rarely quantify how well they do it. In this project, your goal
will be to use data from accelerometers on the belt, forearm, arm, and
dumbell of 6 participants. They were asked to perform barbell lifts
correctly and incorrectly in 5 different ways. More information is
available from the website here:
<http://groupware.les.inf.puc-rio.br/har> (see the section on the Weight
Lifting Exercise Dataset).

Data
====

The training data for this project are available here:
<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv>

The test data are available here:
<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv>

The data for this project come from this source:
<http://groupware.les.inf.puc-rio.br/har>. If you use the document you
create for this class for any purpose please cite them as they have been
very generous in allowing their data to be used for this kind of
assignment.

Step 1: Load Data
-----------------

-   First we'll download the csv files and read them into data frames.

<!-- -->

    library(caret)

    ## Loading required package: lattice
    ## Loading required package: ggplot2

    library(randomForest)

    ## randomForest 4.6-12
    ## Type rfNews() to see new features/changes/bug fixes.

    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","pml-testing.csv")
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","pml-training.csv")
    train <- read.csv("pml-training.csv",na.strings = c("", "NA", "#DIV/0!"))
    test <- read.csv("pml-testing.csv",na.strings = c("", "NA", "#DIV/0!"))

Step 2: Exploratory Analysis
----------------------------

-   Next we'll perform some exploratory analysis:

<!-- -->

    dim(train)

    ## [1] 19622   160

    table(train$classe)

    ## 
    ##    A    B    C    D    E 
    ## 5580 3797 3422 3216 3607

    head(train[,1:11])

    ##   X user_name raw_timestamp_part_1 raw_timestamp_part_2   cvtd_timestamp
    ## 1 1  carlitos           1323084231               788290 05/12/2011 11:23
    ## 2 2  carlitos           1323084231               808298 05/12/2011 11:23
    ## 3 3  carlitos           1323084231               820366 05/12/2011 11:23
    ## 4 4  carlitos           1323084232               120339 05/12/2011 11:23
    ## 5 5  carlitos           1323084232               196328 05/12/2011 11:23
    ## 6 6  carlitos           1323084232               304277 05/12/2011 11:23
    ##   new_window num_window roll_belt pitch_belt yaw_belt total_accel_belt
    ## 1         no         11      1.41       8.07    -94.4                3
    ## 2         no         11      1.41       8.07    -94.4                3
    ## 3         no         11      1.42       8.07    -94.4                3
    ## 4         no         12      1.48       8.05    -94.4                3
    ## 5         no         12      1.48       8.07    -94.4                3
    ## 6         no         12      1.45       8.06    -94.4                3

    dim(test)

    ## [1]  20 160

    head(test[,1:11])

    ##   X user_name raw_timestamp_part_1 raw_timestamp_part_2   cvtd_timestamp
    ## 1 1     pedro           1323095002               868349 05/12/2011 14:23
    ## 2 2    jeremy           1322673067               778725 30/11/2011 17:11
    ## 3 3    jeremy           1322673075               342967 30/11/2011 17:11
    ## 4 4    adelmo           1322832789               560311 02/12/2011 13:33
    ## 5 5    eurico           1322489635               814776 28/11/2011 14:13
    ## 6 6    jeremy           1322673149               510661 30/11/2011 17:12
    ##   new_window num_window roll_belt pitch_belt yaw_belt total_accel_belt
    ## 1         no         74    123.00      27.00    -4.75               20
    ## 2         no        431      1.02       4.87   -88.90                4
    ## 3         no        439      0.87       1.82   -88.50                5
    ## 4         no        194    125.00     -41.60   162.00               17
    ## 5         no        235      1.35       3.33   -88.60                3
    ## 6         no        504     -5.92       1.59   -87.70                4

Step 3: Pre Process Training Data
---------------------------------

-   Now we'll select and reject necessary variables with which we can
    fit our model.
-   We'll firt reject descreiptive fields which are first 6 fields in
    the train dataset.
-   Then we'll reject the variables with nearly 0 variation (small
    variation, which won't impact the result much)
-   Finally we'll reject fields wirh 40% or more NAs.

<!-- -->

    train<- train[,7:dim(train)[2]][,-nearZeroVar(train)]
    cnt <- sapply(train, function(x) {
        sum(!(is.na(x) | x == ""))
    })
    nulvar <- names(cnt[cnt < 0.6 * length(train$classe)])
    train <- train[, !names(train) %in% nulvar]

Step 4: Splitting data
----------------------

-   Dividing Train data into Train Set and Validation Set
-   Used 80% data as Training set and 20% as Test/Validation set

<!-- -->

    set.seed(777)
    trainset <- createDataPartition(train$classe, p = 0.8, list = FALSE)
    trainst <- train[trainset, ]
    testst <- train[-trainset, ]

Step 5: Model Training
----------------------

-   Using Random Forest model to classe Field of Training Set

<!-- -->

    rfMdl <- randomForest(classe ~ ., data = trainst, importance = TRUE, ntrees = 10)

Step 6: Model Validation
------------------------

-   Validating Model using the validation set

<!-- -->

    pvalidation <- predict(rfMdl, testst)
    print(confusionMatrix(pvalidation, testst$classe))

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1116    1    0    0    0
    ##          B    0  758    2    0    0
    ##          C    0    0  682    1    0
    ##          D    0    0    0  642    1
    ##          E    0    0    0    0  720
    ## 
    ## Overall Statistics
    ##                                          
    ##                Accuracy : 0.9987         
    ##                  95% CI : (0.997, 0.9996)
    ##     No Information Rate : 0.2845         
    ##     P-Value [Acc > NIR] : < 2.2e-16      
    ##                                          
    ##                   Kappa : 0.9984         
    ##  Mcnemar's Test P-Value : NA             
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            1.0000   0.9987   0.9971   0.9984   0.9986
    ## Specificity            0.9996   0.9994   0.9997   0.9997   1.0000
    ## Pos Pred Value         0.9991   0.9974   0.9985   0.9984   1.0000
    ## Neg Pred Value         1.0000   0.9997   0.9994   0.9997   0.9997
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2845   0.1932   0.1738   0.1637   0.1835
    ## Detection Prevalence   0.2847   0.1937   0.1741   0.1639   0.1835
    ## Balanced Accuracy      0.9998   0.9990   0.9984   0.9991   0.9993

-   With 99.87% Cross Validation accuracy, we can conclude that our
    model is performing good.
-   It can be applied to the Prediction Set given to get theprediction
    for the 20 Records.

Step 7: Predicting Test Set and Dumping
---------------------------------------

-   Applying the model to predict the answer given for the 20 sets of
    Test Records.
-   Also, splitting that to 20 files to submit the "Submission" part.

<!-- -->

    ptest <- predict(rfMdl, test)
    ptest

    ##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
    ##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
    ## Levels: A B C D E

    answers <- as.vector(ptest)

    pml_write_files = function(x) {
        n = length(x)
        for (i in 1:n) {
            filename = paste0("problem_id_", i, ".txt")
            write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
                col.names = FALSE)
        }
    }

    pml_write_files(answers)

.................. Thank You!!! .....................
=====================================================
