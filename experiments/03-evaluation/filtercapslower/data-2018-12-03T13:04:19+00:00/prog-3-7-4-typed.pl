:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
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
my_list_to_set5(A,B):-list_to_set(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_max_list7(A,B):-max_list(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_even9(A):-0 is A mod 2.
my_flatten10(A,B):-flatten(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_last4,[list(T),T]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_lowercase6,[char]).
prim(my_max_list7,[list(int),int]).
prim(my_sumlist8,[list(int),int]).
prim(my_even9,[int]).
prim(my_flatten10,[list(list(T)),list(T)]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(char),list(char)],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p(['K',z,h,r,'Y'],[k,y]).
p(['N',v,'U',z,v,'W'],[n,u,w]).
p([n,'U','G',e,'C',d,j],[u,g,c]).
p([x,c,'U','F',x,'X'],[u,f,x]).
p(['Z',i,r,p,'Y',q,'Z','I',n],[z,y,z,i]).
q(['K',i,t,'O'],[o,k,'I']).
q(['C',z,o,b,o,e],[y,c]).
q([z,'I','P',q,o,a,v],[p,h,i]).
q(['X','S',k,r,z,d],[s,'X',x]).
q([k,j,'T','N','T','C',q,'G'],[n,t,e,c,g,t]).
