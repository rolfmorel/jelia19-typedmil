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
my_len5(A,B):-length(A,B).
my_odd6(A):-1 is A mod 2.
my_list_to_set7(A,B):-list_to_set(A,B).
my_max_list8(A,B):-max_list(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_last4/2).
prim(my_len5/2).
prim(my_odd6/1).
prim(my_list_to_set7/2).
prim(my_max_list8/2).
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
p(['P','V',e,t,'B','Y','D','A',v],[p,v,b,y,d,a]).
p([f,m,'A','P','W','E',c,'T'],[a,p,w,e,t]).
p(['O','N',d,'V','N','L',h,d,'U'],[o,n,v,n,l,u]).
p(['D',j,'A',t,'D','S','I'],[d,a,d,s,i]).
p(['J','R','K','Y',q],[j,r,k,y]).
q(['D',u,'C',q,o,x,'V'],[v,c,x,d]).
q(['Y','N','K','B',z,'G','H',x],[h,b,y,'D',g,n,k]).
q(['E',l,'P','R'],['G',r,e,p]).
q(['M',j,'W',d,d,'J','R',y],[m,j,r,u,w]).
q([p,'G',w,'G','Q',u],[q,g,'X',g]).
