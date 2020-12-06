class(a).
class(b).
class(c).
interface(d).
interface(e).
interface(f).
extends(a, b).
extends(b, c).
extends(d, e).
extends(e, f).
implements(c, d).

subclass(X, Y) :- class(X), class(Y), extends(X, Y).
subclass(X, Y) :- class(X), class(Y), extends(X, Z), subclass(Z, Y).

subinter(X,Y) :- interface(X), interface(Y), extends(X, Y).
subinter(X,Y) :- interface(X), interface(Y), extends(X, Z), subinter(Z, Y).

superinterface(Y, X) :- class(X), interface(Y), implements(X, Y).
superinterface(Y, X) :- class(X), interface(Y), subclass(X, Z), superinterface(Y, Z).
superinterface(Y, X) :- class(X), interface(Y), subinter(Z, Y), superinterface(Z, X).