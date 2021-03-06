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

my_sumlist4(A,B):-sumlist(A,B).
my_flatten5(A,B):-flatten(A,B).
my_head6([H|_],H).
my_pred7(A,B):-succ(B,A),A > 0.
my_list_to_set8(A,B):-list_to_set(A,B).
my_len9(A,B):-length(A,B).
my_reverse10(A,B):-reverse(A,B).
my_toupper11(A,B):-upcase_atom(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.
my_set13(A):-list_to_set(A,A).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_sumlist4/2).
prim(my_flatten5/2).
prim(my_head6/2).
prim(my_pred7/2).
prim(my_list_to_set8/2).
prim(my_len9/2).
prim(my_reverse10/2).
prim(my_toupper11/2).
prim(my_succ12/2).
prim(my_set13/1).
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
p([f,s,'X',z,'L','K',u],[x,l,k]).
p([f,r,a,m,'K',g,'U','O'],[k,u,o]).
p(['H',w,'H','F',i,i,j,'V',u],[h,h,f,v]).
p([l,'N',w,m,'Q',z],[n,q]).
p(['B','F','A','T','O','C','A'],[b,f,a,t,o,c,a]).
q([h,i,o,'F','Z',d,'N',p,'N'],[f,z,n,z,n]).
q([r,z,o,s,v,h,'Q',x,'F'],[q,'J',f]).
q(['C','U','H',z,'Z'],[h,z,u,k,c]).
q([o,x,w,'N',q,'Z'],[n,i,z]).
q([y,'H','A',t,z,'X',a],['G',h,a,x]).
