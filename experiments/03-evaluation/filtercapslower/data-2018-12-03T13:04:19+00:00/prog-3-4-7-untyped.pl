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

my_succ4(A,B):-succ(A,B),B =< 10.
my_set5(A):-list_to_set(A,A).
my_last6(A,B):-last(A,B).
my_lowercase7(A):-downcase_atom(A,A).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_succ4/2).
prim(my_set5/1).
prim(my_last6/2).
prim(my_lowercase7/1).
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
p(['D',l,x,'P','L','R','W',g,w],[d,p,l,r,w]).
p(['N',l,'K',g],[n,k]).
p([f,'W','X','C',e,'V','K','H',w],[w,x,c,v,k,h]).
p(['M',h,r,'C','X'],[m,c,x]).
p([n,g,t,b],[]).
q([p,s,'E',a,'W',p],[w,v,e]).
q(['A','L',d,q,y,'I','B',d],[w,a,l,b,i]).
q([x,g,'U',t,b,k,'D',h,a],[d,'A',u]).
q([s,'J',g,'X',k,r,'E','D',m],[e,e,d,j,x]).
q([h,'S',i,'A',w,'L',l,m],[m,a,s,l]).
