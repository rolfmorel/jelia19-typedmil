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

my_reverse4(A,B):-reverse(A,B).
my_len5(A,B):-length(A,B).
my_max_list6(A,B):-max_list(A,B).
my_last7(A,B):-last(A,B).
my_min_list8(A,B):-min_list(A,B).
my_element9(A,B):-member(B,A).
my_flatten10(A,B):-flatten(A,B).
my_lowercase11(A):-downcase_atom(A,A).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_reverse4,[list(T),list(T)]).
prim(my_len5,[list(_),int]).
prim(my_max_list6,[list(int),int]).
prim(my_last7,[list(T),T]).
prim(my_min_list8,[list(int),int]).
prim(my_element9,[list(T),T]).
prim(my_flatten10,[list(list(T)),list(T)]).
prim(my_lowercase11,[char]).
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
p([l,'H',o,e,'X',u,'P',a],[h,x,p]).
p([o,'V','I',d],[v,i]).
p(['X',x,'X',y,'S','E','P','K'],[x,x,s,e,p,k]).
p(['M',i,u,'K','S',q,'N'],[m,k,s,n]).
p([j,j,u,q,'V',g,p,'I',k],[v,i]).
q([b,b,y,n,q],[a]).
q([x,b,x,'G',y,h,a,'X','M'],[x,m,d,g]).
q([o,'N',h,v,e,f,e],[n,b]).
q([s,m,e,j,'Q'],[y,q]).
q(['X',a,v,z,'Y','S'],[y,x,t,s]).
