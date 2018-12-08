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
my_last5(A,B):-last(A,B).
my_reverse6(A,B):-reverse(A,B).
my_toupper7(A,B):-upcase_atom(A,B).
my_odd8(A):-1 is A mod 2.
my_succ9(A,B):-succ(A,B),B =< 10.
my_pred10(A,B):-succ(B,A),A > 0.
my_head11([H|_],H).
my_max_list12(A,B):-max_list(A,B).
my_tail13([_|TL],TL).
my_set14(A):-list_to_set(A,A).
my_sumlist15(A,B):-sumlist(A,B).
my_lowercase16(A):-downcase_atom(A,A).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_element4/2).
prim(my_last5/2).
prim(my_reverse6/2).
prim(my_toupper7/2).
prim(my_odd8/1).
prim(my_succ9/2).
prim(my_pred10/2).
prim(my_head11/2).
prim(my_max_list12/2).
prim(my_tail13/2).
prim(my_set14/1).
prim(my_sumlist15/2).
prim(my_lowercase16/1).
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
p([a,z,'O','W','Q'],[o,w,q]).
p(['I','J',j,o,'L',w],[i,j,l]).
p(['I',t,'C','U'],[i,c,u]).
p([r,'D','I','X',m],[d,i,x]).
p(['V',s,'M','C','J','E','G'],[v,m,c,j,e,g]).
q([s,t,d,'Y',z,p,'G'],[y,g,j]).
q([h,f,h,x,'I'],[i,'A']).
q(['P','Q','K',d],[q,k,'T',p]).
q(['K','N','T','S','T','O','S'],[t,n,k,s,t,b,o,s]).
q([r,'F',z,i,'V',d],[v,'U',f]).
