
class GreeterClass{

    constructor(prefix, suffix){
        this.prefix = prefix
        this.suffix = suffix
    }
    greet(name){
        return `${this.prefix} ${name}${this.suffix}`
    }

}

function GreeterClosure1(prefix, suffix){

    function greet(name){
        return `${prefix} ${name}${suffix}`
    }
    return {
        greet
    }

}

function GreeterClosure2(prefix, suffix){

    // pode ser uma função anônima tbm
    return function greet(name){
        return `${prefix} ${name}${suffix}`
    }

}

const GreeterFunction = (prefix, suffix) => name => `${prefix} ${name}${suffix}`

const polymorphicGreet = (greeter) => (...args) => greeter.greet(...args)

l = console.log

greeter1 = new GreeterClass("hello", "!")
l(greeter1.greet("Cristian"))

greeter2 = GreeterClosure1("hello", "!")
l(greeter2.greet("Cristian"))

greeter11 = polymorphicGreet(new GreeterClass("hello", "!"))
l(greeter11("Cristian"))

greeter22 = polymorphicGreet(GreeterClosure1("hello", "!"))
l(greeter22("Cristian"))

greeter3 = GreeterClosure2("hello", "!")
l(greeter3("Cristian"))

greeter4 = GreeterFunction("hello", "!")
l(greeter4("Cristian"))
