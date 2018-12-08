#!/usr/bin/env python

#TODO: implement inputting of number of invented clauses

import sys

typed_prog = """
pred(P,T) :- binary_bg(P,T,_,_).
pred(P,T) :- skolem(P),meta(_,T,P,_,_).
pred(P,T) :- pos_ex(P,T,_,_).

{meta(chain,(S,T),P1,P2,P3)} :- 
  order(P1,P2),order(P1,P3),pred(P2,(S,R)),pred(P3,(R,T)),
  deduced(P2,(S,R),X,Z),deduced(P3,(R,T),Z,Y).
deduced(P1,(S,T),X,Y) :-
  meta(chain,(S,T),P1,P2,P3),
  deduced(P2,(S,R),X,Z),deduced(P3,(R,T),Z,Y).

deduced(P,T,X,Y) :- binary_bg(P,T,X,Y).

state(X,T) :- pos_ex(_,(T,_),X,_).
state(X,T) :- neg_ex(_,(T,_),X,_).
state(Y,T) :- deduced(P,(_,T),_,Y).

pos_ex(f,(int,int),1,0).

:- pos_ex(P,T,X,Y), not deduced(P,T,X,Y).
:- neg_ex(P,T,X,Y), deduced(P,T,X,Y).

size(3).
skolem(0).
skolem(1).
:- #count{ M,T,P1,P2,P3 : meta(M,T,P1,P2,P3) } != N, size(N).
order(X,Y) :- skolem(X), binary_bg(Y,_,_,_).
%order(X,Y) :- skolem(X), ho_bg(Y,_,_,_,_).
order(X,Y) :- pos_ex(X,_,_,_), binary_bg(Y,_,_,_).
%order(X,Y) :- pos_ex(X,_,_,_), ho_bg(Y,_,_,_,_).
order(X,Y) :- pos_ex(X,_,_,_), skolem(Y).
order(X,Y) :- skolem(X), skolem(Y), X < Y.
"""

untyped_prog = """
pred(P) :- binary_bg(P,_,_).
pred(P) :- skolem(P),meta(_,P,_,_).
pred(P) :- pos_ex(P,_,_).

{meta(chain,P1,P2,P3)} :- 
  order(P1,P2),order(P1,P3),pred(P2),pred(P3),
  deduced(P2,X,Z),deduced(P3,Z,Y).
deduced(P1,X,Y) :-
  meta(chain,P1,P2,P3),
  deduced(P2,X,Z),deduced(P3,Z,Y).

deduced(P,X,Y) :- binary_bg(P,X,Y).

state(X) :- pos_ex(_,X,_).
state(X) :- neg_ex(_,X,_).
state(Y) :- deduced(P,_,Y).

pos_ex(f,1,0).

:- pos_ex(P,X,Y), not deduced(P,X,Y).
:- neg_ex(P,X,Y), deduced(P,X,Y).

size(3).
skolem(0).
skolem(1).
:- #count{ M,P1,P2,P3 : meta(M,P1,P2,P3) } != N, size(N).
order(X,Y) :- skolem(X), binary_bg(Y,_,_).
order(X,Y) :- pos_ex(X,_,_), binary_bg(Y,_,_).
order(X,Y) :- pos_ex(X,_,_), skolem(Y).
order(X,Y) :- skolem(X), skolem(Y), X < Y.
"""

INT_LIMIT='5000'

def gen_simple_test(num_clauses, num_well_typed, total_prims, types_enabled):
    untyped = lambda n,i: "binary_bg({n}{i},X,Y):-Y=X+1,Y<{INT_LIMIT},state(X).".format(n=n,i=i,INT_LIMIT=INT_LIMIT)
    typed = lambda n,i,t: "binary_bg({n}{i},({t},{t}),X,Y):-Y=X+1,Y<{INT_LIMIT},state(X,{t}).".format(n=n,i=i,t=t,INT_LIMIT=INT_LIMIT)
    new_prim = lambda n,i,t: typed(n,i,t) if types_enabled else untyped(n,i)

    output = []
    for i in range(1,num_well_typed+1):
        output.append(new_prim('succ',i,'int'))
    for i in range(num_well_typed+1,total_prims+1):
        output.append(new_prim('sucker',i,'bottom'))

    return ('\n'.join(output) + (typed_prog if types_enabled else untyped_prog))

print(gen_simple_test(int(sys.argv[1]), int(sys.argv[2]), int(sys.argv[3]), sys.argv[4] == 'typed'))
