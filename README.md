# Lamcalcj [![Build Status](https://travis-ci.com/yuxuanchiadm/lamcalcj.svg?branch=1.1.x)](https://travis-ci.com/yuxuanchiadm/lamcalcj)

Lamcalcj is a collection of lambda calculus libraries including:

- Basic lambda AST (lamcalcj-ast)
- Utilities (lamcalcj-utils)
- Parser Combinators (lamcalcj-parser)
- Compiler (lamcalcj-compiler)
- Pretty printer (lamcalcj-pretty-print)
- Beta reducer and eta converter (lamcalcj-reducer)

With following features supported:

- Using trampoline to optimize all recursive code path to avoid stack overflow.
- Using parser combinators and customizable syntax for customizing, extending compiler and/or pretty printer.
- Multiple reduction strategies including: head only, evaluation only and their combinations or just using normal order reduction.
- Max step limitation can be given to reducer to avoid lambda terms those have no normal form.
- Other utilities like alpha conversion for renaming, searching free variables, checking whether two terms are alpha equivalent, etc.

# Usage

Release build are pushed to maven repository.

Add following to your `pom.xml` if using maven:

```
<repositories>
	<repository>
		<id>lamcalcj-maven</id>
		<name>lamcalcj-maven</name>
		<url>https://dl.bintray.com/yuxuanchiadm/lamcalcj-maven</url>
	</repository>
</repositories>

<dependencies>
	<dependency>
		<groupId>org.lamcalcj</groupId>
		<artifactId>lamcalcj-ast</artifactId>
		<version>1.1.3</version>
	</dependency>
	<dependency>
		<groupId>org.lamcalcj</groupId>
		<artifactId>lamcalcj-compiler</artifactId>
		<version>1.1.3</version>
	</dependency>
	<dependency>
		<groupId>org.lamcalcj</groupId>
		<artifactId>lamcalcj-pretty-print</artifactId>
		<version>1.1.3</version>
	</dependency>
	<dependency>
		<groupId>org.lamcalcj</groupId>
		<artifactId>lamcalcj-reducer</artifactId>
		<version>1.1.3</version>
	</dependency>
	<dependency>
		<groupId>org.lamcalcj</groupId>
		<artifactId>lamcalcj-utils</artifactId>
		<version>1.1.3</version>
	</dependency>
</dependencies>
```

Add following to your `build.gradle` if using gradle:

```
repositories {
    maven {
        name = 'lamcalcj-maven'
        url = 'https://dl.bintray.com/yuxuanchiadm/lamcalcj-maven'
    }
}

dependencies {
    compile 'org.lamcalcj:lamcalcj-ast:1.1.3'
    compile 'org.lamcalcj:lamcalcj-compiler:1.1.3'
    compile 'org.lamcalcj:lamcalcj-pretty-print:1.1.3'
    compile 'org.lamcalcj:lamcalcj-reducer:1.1.3'
    compile 'org.lamcalcj:lamcalcj-utils:1.1.3'
}
```

# Examples

- GUI Interface: [https://github.com/yuxuanchiadm/lambdacore](https://github.com/yuxuanchiadm/lambdacore)
