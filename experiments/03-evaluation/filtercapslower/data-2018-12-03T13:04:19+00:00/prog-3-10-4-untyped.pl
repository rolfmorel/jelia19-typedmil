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
my_last5(A,B):-last(A,B).
my_max_list6(A,B):-max_list(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_tail8([_|TL],TL).
my_element9(A,B):-member(B,A).
my_min_list10(A,B):-min_list(A,B).
my_odd11(A):-1 is A mod 2.
my_succ12(A,B):-succ(A,B),B =< 10.
my_sumlist13(A,B):-sumlist(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_set4/1).
prim(my_last5/2).
prim(my_max_list6/2).
prim(my_list_to_set7/2).
prim(my_tail8/2).
prim(my_element9/2).
prim(my_min_list10/2).
prim(my_odd11/1).
prim(my_succ12/2).
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
p([t,l,g,n,y,l,'M','G'],[m,g]).
p(['B','J',s,'S',v,y,'B'],[b,j,s,b]).
p([b,z,'S','V','O',p,m,'W',l],[s,v,o,w]).
p(['S',g,'S',q,n,'S'],[s,s,s]).
p([f,h,p,'Z','K',p,f,'T','H'],[z,k,t,h]).
q([y,g,'C',w,u,'T',t],[c,'J',t]).
q([h,'J','Y',z,s,'Y',c],[y,y,n,j]).
q(['J',e,f,'T',k,g,c],[j,'H',t]).
q(['A','W','A','A','X','K',b,m,'A'],[a,a,'J',w,x,a,k,a]).
q(['L',t,'L',b,'S','Q',z,'U',h],[q,o,u,l,s,l]).
