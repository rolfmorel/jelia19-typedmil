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
my_toupper5(A,B):-upcase_atom(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_last7(A,B):-last(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_element9(A,B):-member(B,A).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_list_to_set4/2).
prim(my_toupper5/2).
prim(my_pred6/2).
prim(my_last7/2).
prim(my_sumlist8/2).
prim(my_element9/2).
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
p(['K','Q',j,u,d,'E','B'],[k,q,e,b]).
p(['G','U','K','M','A','D','E'],[g,u,k,m,a,d,e]).
p([i,h,'Q',o,l,y,g,d,'M'],[q,m]).
p([i,j,f,'Y',j,'Q',a],[y,q]).
p([w,z,c,'Q','Z',b,n,n],[q,z]).
q(['N',e,s,e,'A',m,g,'R'],[a,n,'H',r]).
q([u,t,d,f,u],[q]).
q(['T','U','T',i,'C',d],[u,t,s,t,c]).
q([x,'Q',l,b],[q,j]).
q(['G','H','H',p,'B',b,'B'],[f,h,b,b,h,g]).
