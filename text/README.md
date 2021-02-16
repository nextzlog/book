LaTeX to Markdown Converter
====

![image](https://img.shields.io/badge/Gradle-6-red.svg)
![image](https://img.shields.io/badge/Java-SE13-red.svg)
![image](https://img.shields.io/badge/Scala-2.13-orange.svg)
![image](https://img.shields.io/badge/license-BSD%203--Clause-darkblue.svg)

a naive LaTeX processor written in Scala, for conversion from LaTeX to Markdown, based on PEG parsing.

## Features

- macro expansion
- user-defined commands and environments

## Usage

```sh
$ gradle build
$ java -jar build/libs/text.jar [cls file] [sty files] source.tex > temp.md
$ pandoc temp.md -t gfm -N --webtex -F pandoc-crossref > target.md
```

## Contribution

Feel free to contact [@nextzlog](https://twitter.com/nextzlog) on Twitter.

## License

### Author

[無線部開発班 (JOURNAL OF HAMRADIO INFORMATICS LETTERS)](https://pafelog.net)

### Clauses

[BSD 3-Clause License](LICENSE.md)
