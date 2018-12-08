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
my_double6(N,M):-M is 2*N,M =< 10.
my_toupper7(A,B):-upcase_atom(A,B).
my_odd8(A):-1 is A mod 2.
my_max_list9(A,B):-max_list(A,B).
my_pred10(A,B):-succ(B,A),A > 0.
my_reverse11(A,B):-reverse(A,B).
my_msort12(A,B):-msort(A,B).
my_element13(A,B):-member(B,A).
my_set14(A):-list_to_set(A,A).
my_len15(A,B):-length(A,B).
my_list_to_set16(A,B):-list_to_set(A,B).
my_even17(A):-0 is A mod 2.
my_last18(A,B):-last(A,B).
my_lowercase19(A):-downcase_atom(A,A).
my_succ20(A,B):-succ(A,B),B =< 10.
my_tail21([_|TL],TL).
my_min_list22(A,B):-min_list(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_sumlist4/2).
prim(my_flatten5/2).
prim(my_double6/2).
prim(my_toupper7/2).
prim(my_odd8/1).
prim(my_max_list9/2).
prim(my_pred10/2).
prim(my_reverse11/2).
prim(my_msort12/2).
prim(my_element13/2).
prim(my_set14/1).
prim(my_len15/2).
prim(my_list_to_set16/2).
prim(my_even17/1).
prim(my_last18/2).
prim(my_lowercase19/1).
prim(my_succ20/2).
prim(my_tail21/2).
prim(my_min_list22/2).
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
p([h,p,'H','F',k],[h,f]).
p([i,'X','Y','L',d,p],[x,y,l]).
p([o,f,v,z,o,'L','H',h,'E'],[l,h,e]).
p([k,'Z',u,'T','V','V',d,'L','W'],[z,t,v,v,l,w]).
p([q,'T',k,y,m,'O','H'],[t,o,h]).
q([b,'G',a,y,'I','N',w,h,c],[i,n,'A',g]).
q(['X',g,p,'C','G',o,v],[p,c,x,g]).
q(['B','V',o,d,d,t,'D',c],[z,v,b,d]).
q([n,'W','W','D',b,'Y'],[w,w,d,g,y]).
q(['D','K','Q',h,'S',i],[k,'H',d,q,s]).
