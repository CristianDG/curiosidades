
/*
 * Control Flow
 *
 * aqui vou fazer 4 funções:
 *  - parseId   :: String -> ID
 *  - getUser   :: ID -> User
 *  - sendEmail :: User -> String -> ()
 *  - main :: ()
 *
 */

// MOCKS
const users = [
    {name: "Cristian",  id: 11, email: "sla@sla.com"},
    {name: "João",      id: 12, email: "sla@sla.com"},
    {name: "Astolfo",   id: 12, email: "sla@sla.com"},
    {name: "Robson",    id: 13, email: ""},
]
const db = {
    getById : (id) => {
        return users.filter(user => user.id == id)
    }
}
const mail = {
    send : async (email, msg) => {
        if(email.length == 0){
            return null
        }else{
            const res = await new Promise( resolve => {
                console.log(`[EMAIL]: email enviado para ${email}`)
                resolve(true)
            }, 100)
            return res
        }
    }
}
// FIM DOS MOCKS

const input = "11"
const funcional = true

// Parte Imperativa
function parseIdI(s){
    if(!s){
        return null
    }

    // eu sei que parseInt pode receber valores indefinidos
    // mas outras linguagens não podem
    const id = parseInt(s)

    if(!id){
        return null
    }

    return id
}

function getUserI(id){
    const res = db.getById(id)
    if (!res || res.length !== 1) {
        return null
    }

    return res[0]
}

async function sendEmailI(email, message){
    if (await mail.send(email, message)){
        return await true
    }else{
        return false
    }
}

async function mainI(){
    const id = parseIdI(input)
    if(id !== null){
        const user = getUserI(id)
        if(user !== null){
            const res = await sendEmailI(user.email, "Olá")
            if (res){
                console.log("email enviado com sucesso")
            }else{
                console.error("o email não foi enviado")
            }
        }else{
            console.error("usuário não foi encontrado")
        }
    }else{
        console.error("o id não é um numero")
    }
}



// Parte Funcional
class Result{
    constructor(type, val){
        this.type = type
        this.val = val
    }

    static Ok(v){
        return new Result("Ok", v)
    }

    static Err(s){
        return new Result("Err", s)
    }

    static pure(v){
        return Result.Ok(v)
    }

    static pureWithDefault(v,msg){
        return v ? Result.Ok(v) : Result.Err(msg)
    }

    map(f){
        return this.flatMap(i => Result.pure(f(i)))
    }

    mapErr(f){
        return (this.type == "Err"
            ? R.Err(f(this.val))
            : this
        )
    }

    flatMap(f){
        if(this.type === "Ok"){
            return f(this.val)
        }else if(this.type === "Err" ){
            return this
        }else{
            throw new Error("undefined type")
        }
    }

    static isOk(r){
        return r.type === "Ok"
    }

    static isErr(r){
        return r.type === "Err"
    }

}
const R = Result

function parseIdF(s){
    const errmsg = "o id não é um numero"
    const res = R.pureWithDefault(s,errmsg).map(parseInt)
    return res
}

function getUserF(id){

    const res = db.getById(id)
    if (!res || res.length !== 1) {
        return R.Err(`usuário de id ${id} não encontrado`)
    }

    return R.Ok(res[0])
}

async function sendEmailF(email, message){
    const res = R.pureWithDefault(await mail.send(email,message), "o email não foi enviado")
    return res
}

async function mainF(){
    const middleRes = await parseIdF(input)
        .flatMap(getUserF)
        .flatMap( async user => (await sendEmailF(user.email, "Olá")))

    middleRes
        .map( _ => console.log("email enviado com sucesso"))
        .mapErr(console.error)
}

// Main
(async () => await functional ? mainF() : mainI())()
