Scala's Pattern Recognition and Machine Learning
====

![image](https://img.shields.io/badge/Gradle-6-red.svg)
![image](https://img.shields.io/badge/Java-SE13-red.svg)
![image](https://img.shields.io/badge/Scala-2.12-orange.svg)
![image](https://img.shields.io/badge/license-BSD%203--Clause-darkblue.svg)

sample codes of machine learning models and algorithms written in Scala, for the educational purpose in data science.

## Features

- linear regression
- k-nearest neighbors
- k-means & EM algorithm
- naive Bayes classifier
- latent Dirichlet allocation
- multiple layer perceptron
- stochastic gradient descent
- support vector machine & kernel trick

## Dependencies

- Python2.7
- matplotlib
- cartopy
- geos

## Documents

- [Scalaで実装するパターン認識と機械学習 (PDF)](https://pafelog.net/mine.pdf) [(HTML)](https://pafelog.net/mine.html)

## Usage

- Just run the following commands and open figures created:

```sh
$ java -jar build/libs/mine.jar LR
$ java -jar build/libs/mine.jar KNN
$ java -jar build/libs/mine.jar GMM
$ java -jar build/libs/mine.jar NBC
$ java -jar build/libs/mine.jar LDA
$ java -jar build/libs/mine.jar MLP
$ java -jar build/libs/mine.jar SGD
$ java -jar build/libs/mine.jar SVM
```

## Build

```sh
$ gradle build
```

## Contribution

Feel free to contact [@nextzlog](https://twitter.com/nextzlog) on Twitter.

## License

### Author

[無線部開発班 (JOURNAL OF HAMRADIO INFORMATICS LETTERS)](https://pafelog.net)

### Clauses

[BSD 3-Clause License](LICENSE.md)
