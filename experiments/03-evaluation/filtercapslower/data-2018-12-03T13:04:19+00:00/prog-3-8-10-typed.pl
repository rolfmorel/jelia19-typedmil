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

my_pred4(A,B):-succ(B,A),A > 0.
my_set5(A):-list_to_set(A,A).
my_lowercase6(A):-downcase_atom(A,A).
my_len7(A,B):-length(A,B).
my_msort8(A,B):-msort(A,B).
my_tail9([_|TL],TL).
my_list_to_set10(A,B):-list_to_set(A,B).
my_flatten11(A,B):-flatten(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_pred4,[int,int]).
prim(my_set5,[list(_)]).
prim(my_lowercase6,[char]).
prim(my_len7,[list(_),int]).
prim(my_msort8,[list(int),list(int)]).
prim(my_tail9,[list(T),list(T)]).
prim(my_list_to_set10,[list(T),list(T)]).
prim(my_flatten11,[list(list(T)),list(T)]).
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
p(['D','R','Z',k],[d,r,z]).
p(['K','H',b,'G',g,u,o],[k,h,g]).
p([u,d,p,f,'R','P'],[r,p]).
p([g,'I',q,r,v,p],[i]).
p(['N',p,'D','R','G',m,'F'],[n,d,r,g,f]).
q(['U','X',w,p,m,'T',f,'Z'],[z,l,u,x,t]).
q(['W',q,'D',k,m,'D','S',p,'P'],[s,b,d,p,d,w]).
q(['N',v,'F','P',f,u],[f,d,p,n]).
q([m,'C',y,h,r,b,'A'],[c,'P',a]).
q([u,b,'S','F','F','N'],['H',f,s,f,n]).
