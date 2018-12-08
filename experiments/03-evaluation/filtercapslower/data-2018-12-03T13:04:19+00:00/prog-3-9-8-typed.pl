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

my_odd4(A):-1 is A mod 2.
my_even5(A):-0 is A mod 2.
my_msort6(A,B):-msort(A,B).
my_min_list7(A,B):-min_list(A,B).
my_len8(A,B):-length(A,B).
my_tail9([_|TL],TL).
my_reverse10(A,B):-reverse(A,B).
my_double11(N,M):-M is 2*N,M =< 10.
my_head12([H|_],H).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_odd4,[int]).
prim(my_even5,[int]).
prim(my_msort6,[list(int),list(int)]).
prim(my_min_list7,[list(int),int]).
prim(my_len8,[list(_),int]).
prim(my_tail9,[list(T),list(T)]).
prim(my_reverse10,[list(T),list(T)]).
prim(my_double11,[int,int]).
prim(my_head12,[list(T),T]).
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
p(['R',s,'O','H','T',b,'F','Z'],[r,o,h,t,f,z]).
p(['M',p,'W','B','L',k,'C',t],[m,w,b,l,c]).
p([o,c,v,i,m,'Q'],[q]).
p(['E',j,x,m,'F','S',e,'X'],[e,f,s,x]).
p([c,x,'P',b,v,'M','B','X'],[p,m,b,x]).
q(['E','W','S',g,u,'H','I'],[s,e,h,i,'A',w]).
q([j,'K','V','O','D','X',u,v,v],[x,'R',v,k,d,o]).
q(['H',w,'R',l,b,'M'],[m,r,h,'F']).
q([c,'H',b,'W',o,'U','Q','J','U'],[u,'Q',q,h,u,j,w]).
q([y,w,'M','C'],[m,c,c]).
