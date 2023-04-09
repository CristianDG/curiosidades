
// jsf: unicos caracteres possíveis são []!+()

const zero = '+[]'
const one  = '+!![]'


const num = n => {
  if (n == 0) return zero;
  return Array.from({length: n}, () => one).join(' + ')
}

let chars = {}

const str = s => s.split('').map(c =>{
  if(chars[c]) return chars[c];
  if(c >= 'a' && c <= 'z') return `(${num(c.charCodeAt(0)-87)})[${str('toString')}](${num(36)})`;
  return `([]+[])[${str('constructor')}][${str('fromCharCode')}](${num(c.charCodeAt(0))})`
}).join(' + ')


chars['a'] = `(+ +[])[${num(1)}]`
chars['e'] = `(!![]+[])[${num(3)}]`
chars['l'] = `(![]+[])[${num(2)}]`
chars['f'] = `(![]+[])[${num(0)}]`
chars['r'] = `(!![]+[])[${num(1)}]`
chars['s'] = `(![]+[])[${num(3)}]`
chars['u'] = `(!![]+[])[${num(2)}]`
chars['a'] = `(![]+[])[${num(1)}]`
chars['d'] = `([][+![]]+[])[${num(2)}]`
chars['t'] = `(!![]+[])[${num(0)}]`
chars['c'] = `([][${str('at')}] +[])[${num(3)}]`
chars['o'] = `([][${str('at')}] +[])[${num(6)}]`
chars['i'] = `([][${str('at')}] +[])[${num(5)}]`
chars['n'] = `([][${str('at')}] +[])[${num(2)}]`
chars[' '] = `([][${str('at')}] +[])[${num(8)}]`
chars['{'] = `([][${str('at')}] +[])[${num(14)}]`
chars['}'] = `([][${str('at')}] +[])[${num(30)}]`
chars['('] = `([][${str('at')}] +[])[${num(11)}]`
chars[')'] = `([][${str('at')}] +[])[${num(12)}]`
chars['['] = `([][${str('at')}] +[])[${num(16)}]`
chars[']'] = `([][${str('at')}] +[])[${num(28)}]`
chars['S'] = `(([]+[])[${str('constructor')}] +[])[${num(9)}]`
chars['g'] = `(([]+[])[${str('constructor')}] +[])[${num(14)}]`
chars['F'] = `([][${str('constructor')}][${str('constructor')}] +[])[${num(9)}]`
chars['C'] = ` ([][${str('constructor')}][${str('constructor')}])(${str('try{undefined[undefined] } catch (e){ return e}')})()[${str('message')}][${num(0)}]`

const compile = code => `([][${str('constructor')}][${str('constructor')}])(${str(code)})()`


console.log(compile('console.log("hello world")'))
