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

my_last4(A,B):-last(A,B).
my_len5(A,B):-length(A,B).
my_min_list6(A,B):-min_list(A,B).
my_head7([H|_],H).
my_flatten8(A,B):-flatten(A,B).
my_odd9(A):-1 is A mod 2.
my_tail10([_|TL],TL).
my_set11(A):-list_to_set(A,A).
my_msort12(A,B):-msort(A,B).
my_sumlist13(A,B):-sumlist(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_last4/2).
prim(my_len5/2).
prim(my_min_list6/2).
prim(my_head7/2).
prim(my_flatten8/2).
prim(my_odd9/1).
prim(my_tail10/2).
prim(my_set11/1).
prim(my_msort12/2).
prim(my_sumlist13/2).
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
p([u,m,x,'P',e],[p]).
p(['I','K',n,p,l],[i,k]).
p([c,'B',m,'V',z,'K',k],[b,v,k]).
p([g,w,'R',w,'T',i],[r,t]).
p(['Z','L','B','Z',n,'Z','S','W','T'],[z,l,b,z,z,s,w,t]).
q(['D',s,'S','E',d],[f,s,d,e]).
q([f,p,'I',z,u,'T',d],[t,i,n]).
q(['P',c,'F','I'],[p,i,b,f]).
q([z,g,r,'Y',t,v,'D','R'],[d,y,r,s]).
q(['S',e,'Y','S',t,z,b,'F'],[y,f,s,s,u]).
