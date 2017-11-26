# Scalaで実装するパターン認識と機械学習
## Scala’s Pattern Recognition & Machine Learning

- sample codes of machine learning models and algorithms on Scala

## details

- see http://pafelog.net/mine.pdf

## build

`$ gradle build`

## usage

### linear regression

```
$ java -jar build/libs/mine.jar LR
```

### k nearest neighbors

```
$ java -jar build/libs/mine.jar KNN
```

### k-means & EM algorithm

```
$ java -jar build/libs/mine.jar GMM
```

### naive Bayes classifier

```
$ java -jar build/libs/mine.jar NBC
```

### latent Dirichlet allocation (LDA)

```
$ java -jar build/libs/mine.jar LDA
```

### multiple layer perceptron

```
$ java -jar build/libs/mine.jar MLP
```

### stochastic gradient descent & AdaDelta

```
$ java -jar build/libs/mine.jar SGD
```

### support vector machine & kernel trick

```
$ java -jar build/libs/mine.jar SVM
```
