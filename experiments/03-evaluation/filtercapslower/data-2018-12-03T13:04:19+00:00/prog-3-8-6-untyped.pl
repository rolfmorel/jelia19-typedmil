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

my_last4(A,B):-last(A,B).
my_min_list5(A,B):-min_list(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_odd7(A):-1 is A mod 2.
my_list_to_set8(A,B):-list_to_set(A,B).
my_tail9([_|TL],TL).
my_pred10(A,B):-succ(B,A),A > 0.
my_toupper11(A,B):-upcase_atom(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_last4/2).
prim(my_min_list5/2).
prim(my_lowercase6/1).
prim(my_odd7/1).
prim(my_list_to_set8/2).
prim(my_tail9/2).
prim(my_pred10/2).
prim(my_toupper11/2).
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
p(['P',j,'F','C','B',p,f],[p,f,c,b]).
p([a,q,'F','O',i,w,g],[f,o]).
p(['H',h,'B','P','R',d,a],[h,b,p,r]).
p(['C','G','S',y,'T',e],[c,g,s,t]).
p([d,g,'U','B',m,o],[u,b]).
q([h,'A',n,y,'D',l],[d,'U',a]).
q(['E',c,'F',u,'P','H'],[p,h,e,r,f]).
q([a,o,'U','D',r,h,'P'],[d,p,t,u]).
q(['D','J','A','T','W','Q','T','Q'],[t,a,d,q,w,t,j,'R',q]).
q(['B',v,b,p,x,'M'],['J',b,m]).
