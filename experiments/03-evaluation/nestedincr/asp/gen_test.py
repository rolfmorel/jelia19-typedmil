#!/usr/bin/env python

#TODO: implement inputting of number of invented clauses

import sys
import random
from preds import PREDS

def_map_typed = """
ho_bg(map,(list(S),list(T),(S,T)),(),(),F) :- 
  pred(F,(S,T)),ty(S),ty(T).
ho_bg(map,(list(S),list(T),(S,T)),(H1,TL1),(H2,TL2),F) :-
  state((H1,TL1),list(S)),
  state(TL1,list(S)),
  ty(S),ty(T),
  ho_bg(map,(list(S),list(T),(S,T)),TL1,TL2,F),
  deduced(F,(S,T),H1,H2),
  pred(F,(S,T)).
% debug prints
%%derived(ho_bg(map,(list(S),list(T),(S,T)),(),(),F)) :-
%%  ho_bg(map,(list(S),list(T),(S,T)),(),(),F),
%%  &pyDbg["derived ho_bg(map)",ho_bg(map,(list(S),list(T),(S,T)),(),(),F)]().
%%derived(ho_bg(map,(list(S),list(T),(S,T)),(H1,TL1),(H2,TL2),F)) :-
%%  ho_bg(map,(list(S),list(T),(S,T)),(H1,TL1),(H2,TL2),F),
%%  &pyDbg["derived ho_bg(map)",ho_bg(map,(list(S),list(T),(S,T)),(H1,TL1),(H2,TL2),F)]().
"""

def_map_untyped = """
ho_bg(map,(),(),F) :- 
  pred(F).
ho_bg(map,(H1,TL1),(H2,TL2),F) :-
  state((H1,TL1)),
  state(TL1),
  ho_bg(map,TL1,TL2,F),
  deduced(F,H1,H2),
  pred(F). % FIXME: ty(T)?
% debug prints
%%derived(ho_bg(map,(),(),F)) :-
%%  ho_bg(map,(),(),F),
%%  &pyDbg["derived ho_bg(map)",ho_bg(map,(),(),F)]().
%%derived(ho_bg(map,(H1,TL1),(H2,TL2),F)) :-
%%  ho_bg(map,(H1,TL1),(H2,TL2),F),
%%  &pyDbg["derived ho_bg(map)",ho_bg(map,(H1,TL1),(H2,TL2),F)]().
"""

def_filter_typed = """
ho_bg(filter,(list(S),list(S),u(S)),(),(),P) :- 
  pred(P,u(S)).
ho_bg(filter,(list(S),list(S),u(S)),(H1,TL1),(H1,TL2),P) :-
  state((H1,TL1),list(S)),
  deduced(P,u(S),H1),
  ho_bg(filter,(list(S),list(S),u(S)),TL1,TL2,P),
  pred(P,u(S)).
ho_bg(filter,(list(S),list(S),u(S)),(H1,TL1),TL2,P) :-
  state((H1,TL1),list(S)),
  not deduced(P,u(S),H1),
  ho_bg(filter,(list(S),list(S),u(S)),TL1,TL2,P),
  pred(P,u(S)).
% debug prints
%derived(ho_bg(filter,(list(S),list(S),u(S)),(),(),P)) :-
%  ho_bg(filter,(list(S),list(S),u(S)),(),(),P),
%  &pyDbg["derived ho_bg(filter)",ho_bg(filter,(list(S),list(S),u(S)),(),(),P)]().
%derived(ho_bg(filter,(list(S),list(S),u(S)),L1,L2,P)) :-
%  ho_bg(filter,(list(S),list(S),u(S)),L1,L2,P),
%  &pyDbg["derived ho_bg(filter)",ho_bg(filter,(list(S),list(S),u(S)),L1,L2,P)]().
"""

def_filter_untyped = """
ho_bg(filter,(),(),P) :- 
  pred(P).
ho_bg(filter,(H1,TL1),(H1,TL2),P) :-
  state((H1,TL1)),
  deduced(P,H1),
  ho_bg(filter,TL1,TL2,P),
  pred(P).
ho_bg(filter,(H1,TL1),TL2,P) :-
  state((H1,TL1)),
  not deduced(P,H1),
  ho_bg(filter,TL1,TL2,P),
  pred(P).
% debug prints
%derived(ho_bg(filter,(list(S),list(S),u(S)),(),(),P)) :-
%  ho_bg(filter,(list(S),list(S),u(S)),(),(),P),
%  &pyDbg["derived ho_bg(filter)",ho_bg(filter,(list(S),list(S),u(S)),(),(),P)]().
%derived(ho_bg(filter,(list(S),list(S),u(S)),L1,L2,P)) :-
%  ho_bg(filter,(list(S),list(S),u(S)),L1,L2,P),
%  &pyDbg["derived ho_bg(filter)",ho_bg(filter,(list(S),list(S),u(S)),L1,L2,P)]().
"""

metarule_ho_typed = """
{meta(tohigherorder,(S,T),P1,P2,P3)} :- 
  order(P1,P2),pred(P2,(S,T,R)),pred(P3,R),deduced((P2,P3),(S,T),_,_),
  ty(S),ty(T).
deduced(P1,T,X,Y) :- meta(tohigherorder,T,P1,P2,P3),deduced((P2,P3),T,X,Y).
% debug
%%derived(meta(tohigherorder,(S,T),P1,P2,P3)) :- 
%%  meta(tohigherorder,(S,T),P1,P2,P3),
%%  &pyDbg["derived tohigherorder",meta(tohigherorder,(S,T),P1,P2,P3)]().
"""

metarule_ho_untyped = """
{meta(tohigherorder,P1,P2,P3)} :- 
  order(P1,P2),pred(P2),pred(P3),deduced((P2,P3),_,_).
deduced(P1,X,Y) :- meta(tohigherorder,P1,P2,P3),deduced((P2,P3),X,Y).
% debug
%%derived(meta(tohigherorder,P1,P2,P3)) :- 
%%  meta(tohigherorder,P1,P2,P3),
%%  &pyDbg["derived tohigherorder",meta(tohigherorder,P1,P2,P3)]().
"""

metarule_chain_typed = """
{meta(chain,(S,T),P1,P2,P3)} :- 
  order(P1,P2),order(P1,P3),pred(P2,(S,R)),pred(P3,(R,T)),
  deduced(P2,(S,R),X,Z),deduced(P3,(R,T),Z,Y).
deduced(P1,(S,T),X,Y) :-
  meta(chain,(S,T),P1,P2,P3),
  deduced(P2,(S,R),X,Z),deduced(P3,(R,T),Z,Y).
% debug
%%derived(meta(chain,(S,T),P1,P2,P3)) :- 
%%  meta(chain,(S,T),P1,P2,P3),
%%  &pyDbg["derived chain",meta(chain,(S,T),P1,P2,P3)]().
"""

metarule_chain_untyped = """
{meta(chain,P1,P2,P3)} :- 
  order(P1,P2),order(P1,P3),pred(P2),pred(P3),
  deduced(P2,X,Z),deduced(P3,Z,Y).
deduced(P1,X,Y) :-
  meta(chain,P1,P2,P3),
  deduced(P2,X,Z),deduced(P3,Z,Y).
% debug
%%derived(meta(chain,P1,P2,P3)) :- 
%%  meta(chain,P1,P2,P3),
%%  &pyDbg["derived chain",meta(chain,P1,P2,P3)]().
"""

metarule_dident_typed = """
{meta(dident,(S,T),P1,P2,P3)} :- 
  order(P1,P2),order(P1,P3),pred(P2,(S,T)),pred(P3,(S,T)),
  deduced(P2,(S,T),X,Y),deduced(P3,(S,T),X,Y).
deduced(P1,(S,T),X,Y) :-
  meta(dident,(S,T),P1,P2,P3),
  deduced(P2,(S,T),X,Y),deduced(P3,(S,T),X,Y).
% debug
%%derived(meta(chain,(S,T),P1,P2,P3)) :- 
%%  meta(chain,(S,T),P1,P2,P3),
%%  &pyDbg["derived chain",meta(chain,(S,T),P1,P2,P3)]().
"""

metarule_dident_untyped = """
{meta(dident,P1,P2,P3)} :- 
  order(P1,P2),order(P1,P3),pred(P2),pred(P3),
  deduced(P2,X,Y),deduced(P3,X,Y).
deduced(P1,X,Y) :-
  meta(dident,P1,P2,P3),
  deduced(P2,X,Y),deduced(P3,X,Y).
% debug
%%derived(meta(chain,P1,P2,P3)) :- 
%%  meta(chain,P1,P2,P3),
%%  &pyDbg["derived chain",meta(chain,P1,P2,P3)]().
"""

metarule_tailrec_typed = """
{meta(tailrec,(S,T),P1,P2,P1)} :- 
  order(P1,P2),pred(P2,(S,S)),pred(P1,(S,T)),
  deduced(P2,(S,S),X,Z),deduced(P1,(S,T),Z,Y).
deduced(P1,(S,T),X,Y) :-
  meta(tailrec,(S,T),P1,P2,P1),
  deduced(P2,(S,S),X,Z),deduced(P1,(S,T),Z,Y).
% debug
%%derived(meta(chain,(S,T),P1,P2,P3)) :- 
%%  meta(chain,(S,T),P1,P2,P3),
%%  &pyDbg["derived chain",meta(chain,(S,T),P1,P2,P3)]().
"""

metarule_tailrec_untyped = """
{meta(tailrec,P1,P2,P1)} :- 
  order(P1,P2),pred(P2),pred(P1),
  deduced(P2,X,Z),deduced(P2,Z,Y).
deduced(P1,X,Y) :-
  meta(tailrec,P1,P2,P1),
  deduced(P2,X,Z),deduced(P1,Z,Y).
% debug
%%derived(meta(chain,P1,P2,P3)) :- 
%%  meta(chain,P1,P2,P3),
%%  &pyDbg["derived chain",meta(chain,P1,P2,P3)]().
"""

typed_prog = """
type(int,0).
type(char,0).
type(list(T),I):- J + 1 = I, I < 3,type(T,J).
ty(T) :- type(T,_).

pred(P,T) :- unary_bg(P,T,_).
pred(P,T) :- binary_bg(P,T,_,_).
pred(P,T) :- ho_bg(P,T,_,_,_).
pred(P,T) :- skolem(P),meta(_,T,P,_,_).
pred(P,T) :- pos_ex_t(P,T,_,_).
% debug
%%derived(pred(P,T)):-
%%  pred(P,T),
%%  &pyDbg["derived",pred(P,T)]().

deduced(P,u(T),X) :- unary_bg(P,u(T),X).
deduced(P,T,X,Y) :- binary_bg(P,T,X,Y).
deduced((P2,P3),(S,T),X,Y) :- ho_bg(P2,(S,T,_),X,Y,P3).
% debug
%%derived(deduced(P,T,X,Y)) :-
%%  deduced(P,T,X,Y),
%%  &pyDbg["derived:",deduced(P,T,X,Y)]().

state(X,T) :- pos_ex_t(_,(T,_),X,_).
state(X,T) :- neg_ex(_,(T,_),X,_).
state(TL,list(T)) :- state((_,TL),list(T)). % FIXME: should really be able to get rid of this rule
state(HD,T) :- state((HD,_),list(T)). % FIXME: should really be able to get rid of this rule
state(Y,T) :- deduced(P,(_,T),_,Y).
state(X,T) :- deduced(P,(T,_),X,_).

:- pos_ex_t(P,T,X,Y), not deduced(P,T,X,Y).
:- neg_ex_t(P,T,X,Y), deduced(P,T,X,Y).

size(3).
skolem(0).
skolem(1).
:- #count{ M,T,P1,P2,P3 : meta(M,T,P1,P2,P3) } != N, size(N).
order(X,Y) :- skolem(X), binary_bg(Y,_,_,_).
order(X,Y) :- skolem(X), ho_bg(Y,_,_,_,_).
order(X,Y) :- pos_ex_t(X,_,_,_), binary_bg(Y,_,_,_).
order(X,Y) :- pos_ex_t(X,_,_,_), ho_bg(Y,_,_,_,_).
order(X,Y) :- pos_ex_t(X,_,_,_), skolem(Y).
order(X,Y) :- skolem(X), skolem(Y), X < Y.
"""

untyped_prog = """
pred(P) :- unary_bg(P,_).
pred(P) :- binary_bg(P,_,_).
pred(P) :- ho_bg(P,_,_,_).
pred(P) :- skolem(P).
pred(P) :- pos_ex(P,_,_).
% debug
%%derived(pred(P)):-
%%  pred(P),
%%  &pyDbg["derived",pred(P)]().

deduced(P,X) :- unary_bg(P,X).
deduced(P,X,Y) :- binary_bg(P,X,Y).
deduced((P2,P3),X,Y) :- ho_bg(P2,X,Y,P3).
% debug
%%derived(deduced(P,X,Y)) :-
%%  deduced(P,X,Y),
%%  &pyDbg["derived:",deduced(P,X,Y)]().

state(X) :- pos_ex(_,X,_).
state(X) :- neg_ex(_,X,_).
state(TL) :- state((_,TL)). % FIXME: should really be able to get rid of this rule
state(HD) :- state((HD,_)). % FIXME: should really be able to get rid of this rule
state(Y) :- deduced(P,_,Y).
state(X) :- deduced(P,X,_).

:- pos_ex(P,X,Y), not deduced(P,X,Y).
:- neg_ex(P,X,Y), deduced(P,X,Y).

size(3).
skolem(0).
skolem(1).
:- #count{ M,P1,P2,P3 : meta(M,P1,P2,P3) } != N, size(N).
order(X,Y) :- skolem(X), binary_bg(Y,_,_).
order(X,Y) :- skolem(X), ho_bg(Y,_,_,_).
order(X,Y) :- pos_ex(X,_,_), binary_bg(Y,_,_).
order(X,Y) :- pos_ex(X,_,_), ho_bg(Y,_,_,_).
order(X,Y) :- pos_ex(X,_,_), skolem(Y).
order(X,Y) :- skolem(X), skolem(Y), X < Y.
"""


INT_LIMIT='10'

dist = [
['my_map', 
(lambda n: def_map_untyped),
(lambda n: def_map_typed)],
['my_filter', 
(lambda n: def_filter_untyped),
(lambda n: def_filter_typed)],
['my_len', 
(lambda n: 'binary_bg(my_len{},X,Y) :- &pyLength[X](Y),state(X).'.format(n)),
(lambda n: 'binary_bg(my_len{},(list(T),int),X,Y) :- &pyLength[X](Y),state(X,list(T)).'.format(n))],
['my_sumlist', 
(lambda n: 'binary_bg(my_sumlist{},X,Y) :- &pySumlist[X](Y),state(X).'.format(n)),
(lambda n: 'binary_bg(my_sumlist{},(list(int),int),X,Y) :- &pySumlist[X](Y),state(X,list(int)).'.format(n))],
['my_max_list', 
(lambda n: 'binary_bg(my_max_list{},X,Y) :- &pyMaxlist[X](Y),state(X).'.format(n)),
(lambda n: 'binary_bg(my_max_list{},(list(int),int),X,Y) :- &pyMaxlist[X](Y),state(X,list(int)).'.format(n))],
['my_min_list',
(lambda n: 'binary_bg(my_min_list{},X,Y) :- &pyMinlist[X](Y),state(X).'.format(n)),
(lambda n: 'binary_bg(my_min_list{},(list(int),int),X,Y) :- &pyMinlist[X](Y),state(X,list(int)).'.format(n))],  
['my_succ', 
(lambda n: 'binary_bg(my_succ{},X,Y) :- &pySucc[X](Y),state(X),X < {}.'.format(n, INT_LIMIT)),
(lambda n: 'binary_bg(my_succ{},(int,int),X,Y) :- &pySucc[X](Y),state(X,int),X < {}.'.format(n, INT_LIMIT))],
['my_pred', 
(lambda n: 'binary_bg(my_pred{},X,Y) :- &pyPred[X](Y),state(X),X > 0.'.format(n)),
(lambda n: 'binary_bg(my_pred{},(int,int),X,Y) :- &pyPred[X](Y),state(X,int),X > 0.'.format(n))],
['my_head', 
(lambda n: 'binary_bg(my_head{},(HD,TL),HD) :- state((HD,TL)).'.format(n)),
(lambda n: 'binary_bg(my_head{},(list(T),T),(HD,TL),HD) :- state((HD,TL),list(T)).'.format(n))],
['my_tail', 
(lambda n: 'binary_bg(my_tail{},(HD,TL),TL) :- state((HD,TL)).'.format(n)),
(lambda n: 'binary_bg(my_tail{},(list(T),list(T)),(HD,TL),TL) :- state((HD,TL),list(T)).'.format(n))],
['my_reverse', 
(lambda n: 'binary_bg(my_reverse{},X,Y) :- &pyReverse[X](Y),state(X).'.format(n)),
(lambda n: 'binary_bg(my_reverse{},(list(T),list(T)),X,Y) :- &pyReverse[X](Y),state(X,list(T)),ty(T).'.format(n))],
['my_last', 
(lambda n: 'binary_bg(my_last{},X,Y) :- &pyLast[X](Y),state(X).'.format(n)),
(lambda n: 'binary_bg(my_last{},(list(T),T),X,Y) :- &pyLast[X](Y),state(X,list(T)),ty(T).'.format(n))],
['my_double', 
(lambda n: 'binary_bg(my_double{},X,Y) :- Y=2*X,Y < {},Y >= 0,state(X).'.format(n,INT_LIMIT)),
(lambda n: 'binary_bg(my_double{},(int,int),X,Y) :- Y=2*X,Y < {},Y >= 0,state(X,int).'.format(n,INT_LIMIT))],
#['my_charcode',
#(lambda n: 'binary_bg(my_charcode{},X,Y) :- &pyCharcode[X](Y),state(X).'.format(n)),
#(lambda n: 'binary_bg(my_charcode{},(char,int),X,Y) :- &pyCharcode[X](Y),state(X,char).'.format(n))],
['my_flatten', 
(lambda n: 'binary_bg(my_flatten{},X,Y) :- &pyFlatten[X](Y),state(X).'.format(n)),
(lambda n: 'binary_bg(my_flatten{},(list(list(T)),list(T)),X,Y) :- &pyFlatten[X](Y),state(X,list(list(T))),ty(T).'.format(n))],
['my_list_to_set', 
(lambda n: 'binary_bg(my_list_to_set{},X,Y) :- &pyListToSet[X](Y),state(X).'.format(n)),
(lambda n: 'binary_bg(my_list_to_set{},(list(T),list(T)),X,Y) :- &pyListToSet[X](Y),state(X,list(T)),ty(T).'.format(n))],
['my_element', 
(lambda n: 'binary_bg(my_element{},X,Y) :- &pyElement[X](Y),state(X).'.format(n)),
(lambda n: 'binary_bg(my_element{},(list(T),T),X,Y) :- &pyElement[X](Y),state(X,list(T)),ty(T).'.format(n))],
['my_msort', 
(lambda n: 'binary_bg(my_msort{},X,Y) :- &pySort[X](Y),state(X).'.format(n)),
(lambda n: 'binary_bg(my_msort{},(list(int),list(int)),X,Y) :- &pySort[X](Y),state(X,list(int)).'.format(n))],
['my_toupper', 
(lambda n: 'binary_bg(my_toupper{},X,Y) :- &pyToUpper[X](Y),state(X).'.format(n)),
(lambda n: 'binary_bg(my_toupper{},(char,char),X,Y) :- &pyToUpper[X](Y),state(X,char).'.format(n))],
['my_tolower', 
(lambda n: 'binary_bg(my_tolower{},X,Y) :- &pyToLower[X](Y),state(X).'.format(n)),
(lambda n: 'binary_bg(my_tolower{},(char,char),X,Y) :- &pyToLower[X](Y),state(X,char).'.format(n))],
['my_set', 
(lambda n: 'unary_bg(my_set{0},X):-&pySet[X](),state(X),X<{1}.'.format(n,INT_LIMIT)),
(lambda n: 'unary_bg(my_set{0},u(list(T)),X):-&pySet[X](),state(X,list(T)),X<{1}.'.format(n,INT_LIMIT))],
['my_even', 
(lambda n: 'unary_bg(my_even{0},X):- 0 = X \ 2,state(X),X<{1}.'.format(n,INT_LIMIT)),
(lambda n: 'unary_bg(my_even{0},u(int),X):- 0 = X \ 2,state(X,int),X<{1}.'.format(n,INT_LIMIT))],
['my_odd', 
(lambda n: 'unary_bg(my_odd{0},X):- 1 = X \ 2,state(X),X<{1}.'.format(n,INT_LIMIT)),
(lambda n: 'unary_bg(my_odd{0},u(int),X):- 1 = X \ 2,state(X,int),X<{1}.'.format(n,INT_LIMIT))],
['my_uppercase', 
(lambda n: 'unary_bg(my_uppercase{0},X):-&pyUppercase[X](),state(X).'.format(n)),
(lambda n: 'unary_bg(my_uppercase{0},u(char),X):-&pyUppercase[X](),state(X,char).'.format(n))],
['my_lowercase', 
(lambda n: 'unary_bg(my_lowercase{0},X):-&pyLowercase[X](),state(X).'.format(n)),
(lambda n: 'unary_bg(my_lowercase{0},u(char),X):-&pyLowercase[X](),state(X,char).'.format(n))],
]

def get_prog(prims, types_enabled):
    #untyped = lambda n,i: "binary_bg({n}{i},X,Y):-Y=X+1,Y<{INT_LIMIT},state(X).".format(n=n,i=i,INT_LIMIT=INT_LIMIT)
    #typed = lambda n,i,t: "binary_bg({n}{i},({t},{t}),X,Y):-Y=X+1,Y<{INT_LIMIT},state(X,{t}).".format(n=n,i=i,t=t,INT_LIMIT=INT_LIMIT)
    #new_prim = lambda n,i,t: typed(n,i,t) if types_enabled else untyped(n,i)

    typed = typed_prog + metarule_chain_typed + metarule_ho_typed
    untyped = untyped_prog + metarule_chain_untyped + metarule_ho_untyped
    output = [typed if types_enabled else untyped]

    add_defs = []
    for i, prim in enumerate(prims):
        name, untyped, typed = prim
        if types_enabled:
            add_defs.append(typed(str(i)))
        else:
            add_defs.append(untyped(str(i)))

    return ('\n'.join(output + add_defs))

if __name__ == "__main__":
    num_clauses = int(sys.argv[1]) # ignored for now
    num_add_preds = int(sys.argv[2])
    untyped_file = open(sys.argv[3], 'w')
    typed_file = open(sys.argv[4], 'w')
#    prims = [random.choice(dist) for _ in range(num_add_preds)]
    def select_and_remove_pred(name):
        for i, pred in enumerate(dist):
            if pred[0] == name:
                dist.pop(i)
                return pred
        raise Exception("pred by name '{}' not found".format(name))
    default_prims = [select_and_remove_pred(name) for name in PREDS]
    prims = default_prims + random.sample(dist, num_add_preds)


    print(get_prog(prims,True), file=typed_file)
    print(get_prog(prims,False), file=untyped_file)

