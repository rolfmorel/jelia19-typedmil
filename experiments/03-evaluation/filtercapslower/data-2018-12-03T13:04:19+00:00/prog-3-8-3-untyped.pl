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

my_list_to_set4(A,B):-list_to_set(A,B).
my_msort5(A,B):-msort(A,B).
my_flatten6(A,B):-flatten(A,B).
my_min_list7(A,B):-min_list(A,B).
my_even8(A):-0 is A mod 2.
my_head9([H|_],H).
my_odd10(A):-1 is A mod 2.
my_max_list11(A,B):-max_list(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_list_to_set4/2).
prim(my_msort5/2).
prim(my_flatten6/2).
prim(my_min_list7/2).
prim(my_even8/1).
prim(my_head9/2).
prim(my_odd10/1).
prim(my_max_list11/2).
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
p([j,'C',d,k,'V',v],[c,v]).
p([c,'Q','M',x,u,'L'],[q,m,l]).
p(['P','E','K',z,'F'],[p,e,k,f]).
p([y,'C',f,'S',m,u,'P',u],[c,s,p]).
p(['U',f,'U','J'],[u,u,j]).
q(['G','P','J',x,m,f,'G'],[g,j,x,g,p]).
q([l,w,b,'K','K','S','N'],[k,n,q,s,k]).
q([g,'J',m,t,'P',z,d,'D',q],['R',p,j,d]).
q([k,o,'Y',b,'F','Q','U'],[y,q,u,f,v]).
q([l,u,i,'O','G','N',t],[g,o,r,n]).
