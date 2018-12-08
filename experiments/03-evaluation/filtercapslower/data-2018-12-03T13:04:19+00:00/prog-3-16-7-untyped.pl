:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
my_uppercase0(A):-upcase_atom(A,A).
my_tolower1(A,B):-downcase_atom(A,B).

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).


map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_min_list4(A,B):-min_list(A,B).
my_msort5(A,B):-msort(A,B).
my_even6(A):-0 is A mod 2.
my_last7(A,B):-last(A,B).
my_lowercase8(A):-downcase_atom(A,A).
my_tail9([_|TL],TL).
my_toupper10(A,B):-upcase_atom(A,B).
my_head11([H|_],H).
my_list_to_set12(A,B):-list_to_set(A,B).
my_element13(A,B):-member(B,A).
my_odd14(A):-1 is A mod 2.
my_double15(N,M):-M is 2*N,M =< 10.
my_set16(A):-list_to_set(A,A).
my_max_list17(A,B):-max_list(A,B).
my_pred18(A,B):-succ(B,A),A > 0.
my_flatten19(A,B):-flatten(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_min_list4/2).
prim(my_msort5/2).
prim(my_even6/1).
prim(my_last7/2).
prim(my_lowercase8/1).
prim(my_tail9/2).
prim(my_toupper10/2).
prim(my_head11/2).
prim(my_list_to_set12/2).
prim(my_element13/2).
prim(my_odd14/1).
prim(my_double15/2).
prim(my_set16/1).
prim(my_max_list17/2).
prim(my_pred18/2).
prim(my_flatten19/2).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learn(Pos,Neg,H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p([m,'B',g,e,x,'C',q,'U',x],[b,c,u]).
p(['X',x,v,'P','F'],[x,p,f]).
p([t,q,'L',w,y,m,'Z'],[l,z]).
p(['B',u,g,q,g],[b]).
p([n,a,y,f,g],[]).
q([g,'A',v,m,'V',l,'D'],['O',d,v,a]).
q(['Q','Z',t,t,q,'Q','D',b],[z,q,q,'V',d]).
q(['X',l,'X',g,'V',w,u,'N','K'],[v,x,x,'J',n,k]).
q([o,'S',z,'O','I',v,v],[s,'Z',i,o]).
q(['T','Y','B',l,t,c,b],[g,b,y,t]).
