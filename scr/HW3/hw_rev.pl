beat(michigan,osu,football).

focusOn(michigan).

focusOn(osu,football).

focusOn(mit,engr).

focusOn(alabama,football).

university(alabama).

university(osu).

university(mit).

university(michigan).

beat(universityBeatenBy(michigan,football),michigan,football).

university(universityBeatenBy(michigan,football)).

goodAt(X,football) :- university(X), beat(X,Y,football), university(Y), goodAt(Y,football).

wealthy(X) :- university(X), goodAt(X,football), focusOn(X,engr).

goodAt(X,football) :- university(X), focusOn(X,football).

goodAt(X,engr) :- university(X), focusOn(X,engr).

goodAt(X,cs) :- university(X), wealthy(X), goodAt(X,engr).

