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

my_set4(A):-list_to_set(A,A).
my_even5(A):-0 is A mod 2.
my_last6(A,B):-last(A,B).
my_tail7([_|TL],TL).
my_odd8(A):-1 is A mod 2.
my_double9(N,M):-M is 2*N,M =< 10.
my_toupper10(A,B):-upcase_atom(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_max_list12(A,B):-max_list(A,B).
my_list_to_set13(A,B):-list_to_set(A,B).
my_lowercase14(A):-downcase_atom(A,A).
my_pred15(A,B):-succ(B,A),A > 0.
my_msort16(A,B):-msort(A,B).
my_reverse17(A,B):-reverse(A,B).
my_head18([H|_],H).
my_flatten19(A,B):-flatten(A,B).
my_element20(A,B):-member(B,A).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_set4/1).
prim(my_even5/1).
prim(my_last6/2).
prim(my_tail7/2).
prim(my_odd8/1).
prim(my_double9/2).
prim(my_toupper10/2).
prim(my_sumlist11/2).
prim(my_max_list12/2).
prim(my_list_to_set13/2).
prim(my_lowercase14/1).
prim(my_pred15/2).
prim(my_msort16/2).
prim(my_reverse17/2).
prim(my_head18/2).
prim(my_flatten19/2).
prim(my_element20/2).
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
p(['B',l,n,o,y],[b]).
p(['F','B',m,t,q],[f,b]).
p([j,q,v,'V','V','Q','Z','Y'],[v,v,q,z,y]).
p([c,'E',m,'H','Z','W'],[e,h,z,w]).
p(['N',q,'J','Z','T',l,d],[n,j,z,t]).
q([z,t,'W',o,e,'K','T','M'],[w,'D',m,t,k]).
q([a,'T',o,c,'I',s,r,i,'P'],[t,'L',i,p]).
q([y,'U',n,'N',r],[n,a,u]).
q([u,o,'C',c,'F',k,n],[f,c,f]).
q(['A','O',h,'N','N','E','P','U'],[e,n,p,o,'H',u,n,a]).
