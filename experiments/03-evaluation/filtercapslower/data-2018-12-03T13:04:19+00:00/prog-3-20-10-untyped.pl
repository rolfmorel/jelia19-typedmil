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
my_lowercase5(A):-downcase_atom(A,A).
my_flatten6(A,B):-flatten(A,B).
my_max_list7(A,B):-max_list(A,B).
my_element8(A,B):-member(B,A).
my_set9(A):-list_to_set(A,A).
my_pred10(A,B):-succ(B,A),A > 0.
my_toupper11(A,B):-upcase_atom(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.
my_even13(A):-0 is A mod 2.
my_msort14(A,B):-msort(A,B).
my_reverse15(A,B):-reverse(A,B).
my_head16([H|_],H).
my_odd17(A):-1 is A mod 2.
my_min_list18(A,B):-min_list(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_tail20([_|TL],TL).
my_last21(A,B):-last(A,B).
my_double22(N,M):-M is 2*N,M =< 10.
my_len23(A,B):-length(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_list_to_set4/2).
prim(my_lowercase5/1).
prim(my_flatten6/2).
prim(my_max_list7/2).
prim(my_element8/2).
prim(my_set9/1).
prim(my_pred10/2).
prim(my_toupper11/2).
prim(my_succ12/2).
prim(my_even13/1).
prim(my_msort14/2).
prim(my_reverse15/2).
prim(my_head16/2).
prim(my_odd17/1).
prim(my_min_list18/2).
prim(my_sumlist19/2).
prim(my_tail20/2).
prim(my_last21/2).
prim(my_double22/2).
prim(my_len23/2).
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
p(['N','D','P','Y','J','Q','V','J'],[n,d,p,y,j,q,v,j]).
p(['R','Z','D',p,'D','X',e,'X'],[r,z,d,d,x,x]).
p(['T',e,'M',r,c],[t,m]).
p(['T',m,'P',q,m,v,w,w,q],[t,p]).
p(['M','S','U','G',d,'L',p,t],[m,s,u,g,l]).
q([t,'U',t,g,'R',z,'W',i,k],['W',u,w,r]).
q([b,'L',j,'P','I'],[p,i,'D',l]).
q(['G','D','O','L',y,m,'X',h],[g,x,e,o,d,l]).
q([z,i,'M','D'],[d,m,p]).
q([r,'Z','X',s,b,k,'Z'],[z,z,x,a]).
