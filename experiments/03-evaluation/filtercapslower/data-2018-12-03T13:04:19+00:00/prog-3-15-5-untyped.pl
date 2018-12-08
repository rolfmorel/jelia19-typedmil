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

my_tail4([_|TL],TL).
my_toupper5(A,B):-upcase_atom(A,B).
my_len6(A,B):-length(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_flatten8(A,B):-flatten(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_list_to_set10(A,B):-list_to_set(A,B).
my_set11(A):-list_to_set(A,A).
my_msort12(A,B):-msort(A,B).
my_head13([H|_],H).
my_element14(A,B):-member(B,A).
my_succ15(A,B):-succ(A,B),B =< 10.
my_odd16(A):-1 is A mod 2.
my_reverse17(A,B):-reverse(A,B).
my_min_list18(A,B):-min_list(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_tail4/2).
prim(my_toupper5/2).
prim(my_len6/2).
prim(my_pred7/2).
prim(my_flatten8/2).
prim(my_sumlist9/2).
prim(my_list_to_set10/2).
prim(my_set11/1).
prim(my_msort12/2).
prim(my_head13/2).
prim(my_element14/2).
prim(my_succ15/2).
prim(my_odd16/1).
prim(my_reverse17/2).
prim(my_min_list18/2).
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
p(['L',b,'S',v,g,'X'],[l,s,x]).
p([m,l,l,e,o,'K','L'],[k,l]).
p(['R','W',v,'J',r],[r,w,j]).
p(['E',d,'P','B'],[e,p,b]).
p(['V',i,'E',u,o],[v,e]).
q([d,'Z',l,'O',z,o,'Z'],['L',z,z,o]).
q(['N',p,'P',p,'R',b,s,y,r],[p,n,'L',r]).
q([t,i,'W',q,g,'S',r,'R','E'],[s,e,r,w,e]).
q([i,l,'E','Z','S','O',n],[z,'H',o,e,s]).
q([j,'Q','N',c,f,x,'T',v,j],[x,n,t,q]).
