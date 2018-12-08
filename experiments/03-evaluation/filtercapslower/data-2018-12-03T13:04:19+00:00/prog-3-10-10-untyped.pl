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

my_element4(A,B):-member(B,A).
my_lowercase5(A):-downcase_atom(A,A).
my_len6(A,B):-length(A,B).
my_succ7(A,B):-succ(A,B),B =< 10.
my_max_list8(A,B):-max_list(A,B).
my_even9(A):-0 is A mod 2.
my_pred10(A,B):-succ(B,A),A > 0.
my_odd11(A):-1 is A mod 2.
my_reverse12(A,B):-reverse(A,B).
my_set13(A):-list_to_set(A,A).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_element4/2).
prim(my_lowercase5/1).
prim(my_len6/2).
prim(my_succ7/2).
prim(my_max_list8/2).
prim(my_even9/1).
prim(my_pred10/2).
prim(my_odd11/1).
prim(my_reverse12/2).
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
p(['H','E','O','C','M'],[h,e,o,c,m]).
p(['J','H','O','I',b,t],[j,h,o,i]).
p([k,f,c,n],[]).
p(['Q','R','E',r,n,'G',d],[q,r,e,g]).
p(['W','M',c,m,r],[w,m]).
q(['C','A',t,k,s],[a,'M',c]).
q([x,'F',s,'J'],[j,'N',f]).
q([p,o,'U',t,y,'F','V','R',f],[u,r,'R',f,v]).
q(['P',z,'G',w,'O',f,d,p],[p,o,g,'C']).
q(['M','Q',l,'G','P'],[e,p,m,q,g]).
