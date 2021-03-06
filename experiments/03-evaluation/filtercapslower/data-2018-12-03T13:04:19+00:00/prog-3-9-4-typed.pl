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

my_msort4(A,B):-msort(A,B).
my_min_list5(A,B):-min_list(A,B).
my_len6(A,B):-length(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
my_element8(A,B):-member(B,A).
my_reverse9(A,B):-reverse(A,B).
my_pred10(A,B):-succ(B,A),A > 0.
my_tail11([_|TL],TL).
my_head12([H|_],H).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_msort4,[list(int),list(int)]).
prim(my_min_list5,[list(int),int]).
prim(my_len6,[list(_),int]).
prim(my_double7,[int,int]).
prim(my_element8,[list(T),T]).
prim(my_reverse9,[list(T),list(T)]).
prim(my_pred10,[int,int]).
prim(my_tail11,[list(T),list(T)]).
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
p(['U',c,p,j,b,'Y'],[u,y]).
p(['Z','D',g,'P',u,c,d],[z,d,p]).
p([i,y,'A','R','N',x,'V'],[a,r,n,v]).
p(['O',b,'T',x,'N',g,e],[o,t,n]).
p(['A','H',o,v],[a,h]).
q(['V','E','N','G',y,f,m,l],[e,g,n,v,'X']).
q(['R',e,'S',z,'V','U','B','C'],[r,u,c,s,b,v,'V']).
q([i,u,p,v,p],[p]).
q(['F',g,q,b,'I','G'],[f,g,i,q]).
q([j,w,'N',u,'V',s,'D'],[e,d,n,v]).
