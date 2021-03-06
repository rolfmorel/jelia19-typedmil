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

my_flatten4(A,B):-flatten(A,B).
my_set5(A):-list_to_set(A,A).
my_reverse6(A,B):-reverse(A,B).
my_last7(A,B):-last(A,B).
my_succ8(A,B):-succ(A,B),B =< 10.
my_double9(N,M):-M is 2*N,M =< 10.
my_sumlist10(A,B):-sumlist(A,B).
my_tail11([_|TL],TL).
my_pred12(A,B):-succ(B,A),A > 0.
my_lowercase13(A):-downcase_atom(A,A).
my_odd14(A):-1 is A mod 2.
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_flatten4/2).
prim(my_set5/1).
prim(my_reverse6/2).
prim(my_last7/2).
prim(my_succ8/2).
prim(my_double9/2).
prim(my_sumlist10/2).
prim(my_tail11/2).
prim(my_pred12/2).
prim(my_lowercase13/1).
prim(my_odd14/1).
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
p(['N','Y','C',w,'D'],[n,y,c,d]).
p([d,d,'S','S','L','B',d,'A'],[s,s,l,b,a]).
p([a,'K',p,c,'U'],[k,u]).
p(['V','T',w,n,'X',e,'J','J'],[v,t,x,j,j]).
p(['G',o,l,q,f,'E'],[g,e]).
q(['M',h,l,'M',q,f,'U'],[u,d,m,m]).
q([r,p,'V',a],[v,'C']).
q(['J',p,'C','P',a,m],['E',j,p,c]).
q([i,r,b,'E',i,l,'O'],[o,e,'P']).
q(['N','W',x,a],[n,w,b]).
