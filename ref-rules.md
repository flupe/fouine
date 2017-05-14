(e0, ..., e4) :
	fun s5 ->
		let (v4, s4) = [[e4]] s5 in
		let (v3, s3) = [[e3]] s4 in
		...
		let (v0, s0) = [[e0]] s1 in
		((v0, ..., v4), s0)

[e0; ...; e4] :	
	fun s5 ->
		let (v4, s4) = [[e4]] s5 in
		let (v3, s3) = [[e3]] s4 in
		...
		let (v0, s0) = [[e0]] s1 in
		([v0; ...; v4], s0)

e1 e2:
	fun s ->
		let (v2, s2) = [[e2]] s in
		let (v1, s1) = [[e1]] s2 in
		v1 v2 s2

let x = e1 in e2:
	fun s ->
		let (x, s1) = [[e1]] s in
		[[e2]] s1

fun x -> e:
	fun s ->
		(fun x -> [[e]], s)

if cond then e1 else e2:
	fun s ->
		let (c, s2) = [[cond]] s in
		if c then
			[[e1]] s2
		else
			[[e2]] s2
---

let rec f = fun pat ->

in ...

let (!) s r =
	(read r s, s)

let (:=) s r s' v =
	((), modify s
