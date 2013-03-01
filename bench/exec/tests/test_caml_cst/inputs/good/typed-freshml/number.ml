module Make (X : AlphaLib.Signatures.Atom) = struct

  open X

  let number base (atoms : AtomSet.t) =
    let n = AtomSet.cardinal atoms in
    if n = 0 then
      let fail _ =
	assert false
      in
      n, fail, fail
    else
      let dummy = AtomSet.choose atoms in
      let direct, inverse, i =
	ref AtomMap.empty,
	Array.make n dummy,
	ref base
      in
      AtomSet.iter (fun atom ->
	let number = !i in
	direct := AtomMap.add atom number !direct;
	inverse.(number - base) <- atom;
	i := number + 1
      ) atoms;
      let direct atom =
	try
	  AtomMap.find atom !direct
	with Not_found ->
	  assert false
      and inverse number =
	inverse.(number - base)
      in
      n, direct, inverse

end

