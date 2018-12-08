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

my_min_list4(A,B):-min_list(A,B).
my_succ5(A,B):-succ(A,B),B =< 10.
my_len6(A,B):-length(A,B).
my_set7(A):-list_to_set(A,A).
my_even8(A):-0 is A mod 2.
my_list_to_set9(A,B):-list_to_set(A,B).
my_last10(A,B):-last(A,B).
my_reverse11(A,B):-reverse(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_min_list4,[list(int),int]).
prim(my_succ5,[int,int]).
prim(my_len6,[list(_),int]).
prim(my_set7,[list(_)]).
prim(my_even8,[int]).
prim(my_list_to_set9,[list(T),list(T)]).
prim(my_last10,[list(T),T]).
prim(my_reverse11,[list(T),list(T)]).
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
p(['B','S',j,'J',m,s],[b,s,j]).
p(['Q','N','B',y,f,w,z,j,'U'],[q,n,b,u]).
p([s,l,'C',o,h,g],[c]).
p([o,q,t,c,h,f],[]).
p([n,i,p,r,w,'M',q],[m]).
q(['Y','A','G',o,'S','C',q],[s,c,y,a,g,'A']).
q(['S',x,'P',w],[p,m,s]).
q([m,'C',r,'I',h,n],[w,i,c]).
q(['U','G',s,'R','F',o,'W','I',o],[f,g,i,w,u,r,'V']).
q([k,'P',h,x,o,'U',l,'D'],[d,x,u,p]).
