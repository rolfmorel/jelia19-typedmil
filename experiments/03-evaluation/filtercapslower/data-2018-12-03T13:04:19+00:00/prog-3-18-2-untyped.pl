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

my_reverse4(A,B):-reverse(A,B).
my_element5(A,B):-member(B,A).
my_odd6(A):-1 is A mod 2.
my_len7(A,B):-length(A,B).
my_max_list8(A,B):-max_list(A,B).
my_msort9(A,B):-msort(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_flatten11(A,B):-flatten(A,B).
my_double12(N,M):-M is 2*N,M =< 10.
my_set13(A):-list_to_set(A,A).
my_tail14([_|TL],TL).
my_toupper15(A,B):-upcase_atom(A,B).
my_lowercase16(A):-downcase_atom(A,A).
my_list_to_set17(A,B):-list_to_set(A,B).
my_even18(A):-0 is A mod 2.
my_last19(A,B):-last(A,B).
my_succ20(A,B):-succ(A,B),B =< 10.
my_head21([H|_],H).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_reverse4/2).
prim(my_element5/2).
prim(my_odd6/1).
prim(my_len7/2).
prim(my_max_list8/2).
prim(my_msort9/2).
prim(my_sumlist10/2).
prim(my_flatten11/2).
prim(my_double12/2).
prim(my_set13/1).
prim(my_tail14/2).
prim(my_toupper15/2).
prim(my_lowercase16/1).
prim(my_list_to_set17/2).
prim(my_even18/1).
prim(my_last19/2).
prim(my_succ20/2).
prim(my_head21/2).
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
p(['W','M','Z','A','J','H','S',m],[w,m,z,a,j,h,s]).
p(['C','B',u,y,d],[c,b]).
p(['Y','T','B',f],[y,t,b]).
p([q,e,'Z','X',i],[z,x]).
p(['Q',t,j,o],[q]).
q([e,g,'R','L',f],[u,r,l]).
q([d,'I','X','Q',l],[x,q,i,'J']).
q(['K',f,'S','O',s,'C',r,z],[s,o,d,k,c]).
q(['H','L',o,n,'O','E'],[e,o,l,'C',h]).
q(['P','X',f,'G','P',g,q],[p,g,x,p,'X']).
